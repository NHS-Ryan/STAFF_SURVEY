read_clean_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::select(-matches("^\\.\\.\\.[0-9]+$")) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~ .x %>%
          stringi::stri_enc_toutf8(is_unknown_8bit = TRUE) %>%
          stringr::str_trim()
      )
    )
}

load_csv_files <- function() {
  list(
    theme_questions_map    = read_clean_csv("maps/theme_questions_map.csv"),
    ox_teams_map           = read_clean_csv("maps/ox_teams_map.csv"),
    nat_result_themes      = read_clean_csv("data/nat_result_themes.csv"),
    nat_result_scores      = read_clean_csv("data/nat_result_scores.csv"),
    ox_q_aggregate_results = read_clean_csv("data/ox_q_aggregate_results.csv"),
    ox_theme_results       = read_clean_csv("data/ox_theme_results.csv"),
    dims_map               = read_clean_csv("maps/dims_map.csv"),
    question_scores_map = read_clean_csv("maps/question_scores_map.csv")
  )
}

load_postgres_files <- function(schema = Sys.getenv("STAFF_SURVEY_DB_SCHEMA", "test")) {

  required_pkgs <- c("DBI", "RPostgres", "jsonlite")

  missing_pkgs <- required_pkgs[
    !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
  ]

  if (length(missing_pkgs) > 0) {
    stop(
      "Missing R packages: ",
      paste(missing_pkgs, collapse = ", ")
    )
  }

  secret_id <- Sys.getenv(
    "STAFF_SURVEY_DB_SECRET",
    unset = "staff-survey/prod/postgres/app"
  )

  secret_string <- system2(
    "aws",
    args = c(
      "secretsmanager", "get-secret-value",
      "--secret-id", secret_id,
      "--query", "SecretString",
      "--output", "text"
    ),
    stdout = TRUE
  )

  secret <- jsonlite::fromJSON(paste(secret_string, collapse = "\n"))

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = secret$host,
    port = as.integer(secret$port),
    dbname = secret$dbname,
    user = secret$username,
    password = secret$password
  )

  on.exit(DBI::dbDisconnect(con), add = TRUE)

  read_pg_table <- function(table_name) {
    DBI::dbReadTable(
      con,
      DBI::Id(schema = schema, table = table_name)
    )
  }

  list(
    theme_questions_map    = read_pg_table("theme_questions_map"),
    themes_map             = read_pg_table("themes_map"),
    ox_teams_map           = read_pg_table("ox_teams_map"),
    nat_result_themes      = read_pg_table("nat_result_themes"),
    nat_result_scores      = read_pg_table("nat_result_scores"),
    ox_q_aggregate_results = read_pg_table("ox_q_aggregate_results"),
    ox_q_option_results    = read_pg_table("ox_q_option_results"),
    ox_theme_results       = read_pg_table("ox_theme_results"),
    dims_map               = read_pg_table("dims_map"),
    question_scores_map    = read_pg_table("question_scores_map"),
    question_options_map   = read_pg_table("question_options_map")
  )
}

data_backend <- Sys.getenv("STAFF_SURVEY_DATA_BACKEND", "csv")

files <- if (identical(data_backend, "postgres")) {
  load_postgres_files()
} else {
  load_csv_files()
}
get_theme_questions_map <- function() files$theme_questions_map
get_ox_teams_map <- function() files$ox_teams_map
get_nat_result_themes <- function() files$nat_result_themes
get_ox_q_aggregate_results <- function() files$ox_q_aggregate_results
get_ox_theme_results <- function() files$ox_theme_results
get_dims_map <- function() files$dims_map
get_nat_result_scores <- function() files$nat_result_scores
get_question_scores_map <- function() files$question_scores_map


get_dims_by_family <- function(family) {
  files$dims_map %>%
    dplyr::filter(filter_family == family) %>%
    dplyr::pull(rename_dim) %>%
    stats::na.omit() %>%
    unique() %>%
    sort()
}


get_protected_characteristic_dims <- function() {
  get_dims_by_family("protected_characteristics")
}

get_professional_group_dims <- function() {
  get_dims_by_family("professional_groups")
}

get_organisational_structure_dims <- function() {
  get_dims_by_family("organisational_structure")
}

get_trust_choices <- function() {
  files$nat_result_themes %>%
    dplyr::pull(org_name) %>%
    stats::na.omit() %>%
    unique() %>%
    sort()
}

get_latest_response_counts <- function(df) {

  latest_year <- suppressWarnings(max(df$year, na.rm = TRUE))

  if (is.infinite(latest_year) || is.na(latest_year)) {
    return(
      tibble(
        q_id = unique(df$q_id),
        Responses = NA_real_
      )
    )
  }

  count_name <- paste0("Responses (", latest_year, ")")

  if (!"n" %in% names(df)) {
    return(
      tibble(q_id = unique(df$q_id)) %>%
        mutate(!!count_name := NA_real_)
    )
  }

  df %>%
    filter(year == latest_year) %>%
    group_by(q_id) %>%
    summarise(
      !!count_name := if (all(is.na(n))) {
        NA_real_
      } else {
        sum(n, na.rm = TRUE)
      },
      .groups = "drop"
    )
}

get_question_table_df <- function(trust_sel,
                                  theme_sel,
                                  domain_sel,
                                  subdomain_sel = "All",
                                  filter_family = "Organisational Structure",
                                  directorate = NULL,
                                  team = NULL,
                                  protected_dim = NULL,
                                  protected_value = NULL,
                                  professional_dim = NULL,
                                  professional_value = NULL,
                                  score = "score") {

  selected_q_ids <- get_theme_questions_map() %>%
    mutate(subdomain = coalesce(subdomain, "")) %>%
    filter(
      theme == theme_sel,
      domain == domain_sel,
      if (is.null(subdomain_sel) || subdomain_sel == "All") TRUE else subdomain == subdomain_sel
    ) %>%
    pull(q_id) %>%
    unique()#

  question_lookup <- get_question_scores_map() %>%
    dplyr::select(q_id, q_text_short) %>%
    dplyr::distinct(q_id, .keep_all = TRUE)

  trust_only <- (
    filter_family == "Organisational Structure" &&
      (is.null(directorate) || identical(directorate, "All"))
  )

  if (trust_only) {
    selected_org_id <- get_nat_result_themes() %>%
      filter(org_name == trust_sel | org_id == trust_sel) %>%
      slice(1) %>%
      pull(org_id) %>%
      first()

    trust_questions <- get_nat_result_scores() %>%
      filter(org_id == selected_org_id, q_id %in% selected_q_ids)

    latest_counts <- get_latest_response_counts(trust_questions)

    return(
      trust_questions %>%
        left_join(question_lookup, by = "q_id") %>%
        mutate(
          question_label = dplyr::coalesce(
            as.character(q_text_short),
            as.character(q_text)
          )
        ) %>%
      group_by(q_id, question_label, year) %>%
        summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        tidyr::pivot_wider(
          names_from = year,
          values_from = score,
          names_sort = TRUE
        ) %>%
        left_join(latest_counts, by = "q_id") %>%
        arrange(q_id) %>%
        select(-q_id) %>%
        rename(Question = question_label)
    )
  }

  base_questions <- get_ox_q_aggregate_results() %>%
    filter(q_id %in% selected_q_ids)

  if (
    filter_family == "Organisational Structure" &&
    !is.null(directorate) &&
    directorate != "All" &&
    (is.null(team) || team == "All")
  ) {
    filtered_questions <- base_questions %>%
      filter(dim == "Directorate", dim_sub == directorate)

    if (nrow(filtered_questions) == 0) {
      directorate_teams <- get_team_aliases(directorate)

      filtered_questions <- base_questions %>%
        filter(dim == "Team", dim_sub %in% directorate_teams)
    }

  } else {
    filtered_questions <- base_questions %>%
      apply_family_filter(
        filter_family = filter_family,
        directorate = directorate,
        team = team,
        protected_dim = protected_dim,
        protected_value = protected_value,
        professional_dim = professional_dim,
        professional_value = professional_value,
        comparison = FALSE
      )
  }

  latest_counts <- get_latest_response_counts(filtered_questions)

  filtered_questions %>%
    left_join(question_lookup, by = "q_id") %>%
    mutate(
      question_label = dplyr::coalesce(
        as.character(q_text_short),
        as.character(q_text)
      )
    ) %>%
    group_by(q_id, question_label, year) %>%
    summarise(
      score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    tidyr::pivot_wider(
      names_from = year,
      values_from = score,
      names_sort = TRUE
    ) %>%
    left_join(latest_counts, by = "q_id") %>%
    arrange(q_id) %>%
    select(-q_id) %>%
    rename(Question = question_label)

}





get_team_aliases <- function(directorate_sel, team_sel = NULL) {
  team_map <- get_ox_teams_map() %>%
    filter(directorate == directorate_sel)

  if (!is.null(team_sel) && team_sel != "All") {
    team_map <- team_map %>%
      filter(team_full == team_sel | team_short == team_sel)
  }

  c(team_map$team_full, team_map$team_short) %>%
    na.omit() %>%
    unique()
}

# -----------------------------
# Gauge data selection
# -----------------------------
get_full_comparison_download_df <- function(trust_sel,
                                            filter_family = "Organisational Structure",
                                            directorate = NULL,
                                            team = NULL,
                                            protected_dim = NULL,
                                            protected_value = NULL,
                                            professional_dim = NULL,
                                            professional_value = NULL,
                                            score = "score") {

  question_lookup <- get_question_scores_map() %>%
    dplyr::select(dplyr::any_of(c("q_id", "q_text_short", "q_text"))) %>%
    dplyr::distinct(q_id, .keep_all = TRUE)

  if (!"q_text_short" %in% names(question_lookup)) {
    question_lookup$q_text_short <- NA_character_
  }

  if (!"q_text" %in% names(question_lookup)) {
    question_lookup$q_text <- NA_character_
  }

  question_lookup <- question_lookup %>%
    dplyr::mutate(
      question = dplyr::coalesce(
        as.character(q_text_short),
        as.character(q_text),
        as.character(q_id)
      )
    ) %>%
    dplyr::select(q_id, question)

  trust_level <- (
    filter_family == "Organisational Structure" &&
      (is.null(directorate) || directorate == "All")
  )

  # -----------------------------
  # Trust-level export
  # -----------------------------
  if (trust_level) {

    trust_meta <- get_nat_result_themes() %>%
      dplyr::filter(org_name == trust_sel | org_id == trust_sel) %>%
      dplyr::summarise(
        selected_org_id = dplyr::first(stats::na.omit(org_id)),
        selected_type = dplyr::first(stats::na.omit(org_type_reporting_name)),
        selected_region = dplyr::first(stats::na.omit(region_name)),
        .groups = "drop"
      )

    selected_org_id <- trust_meta$selected_org_id[[1]]
    selected_type <- trust_meta$selected_type[[1]]
    selected_region <- trust_meta$selected_region[[1]]

    comparator_orgs <- get_nat_result_themes() %>%
      dplyr::filter(
        org_type_reporting_name == .env$selected_type,
        region_name == .env$selected_region
      ) %>%
      dplyr::distinct(
        org_id,
        comp_org_name = org_name,
        comp_org_type_reporting_name = org_type_reporting_name,
        comp_region_name = region_name
      )

    return(
      get_nat_result_scores() %>%
        dplyr::filter(org_id %in% comparator_orgs$org_id) %>%
        dplyr::left_join(comparator_orgs, by = "org_id") %>%
        dplyr::left_join(question_lookup, by = "q_id") %>%
        dplyr::mutate(
          comparison_level = "Trust",
          comparison_group = comp_org_name,
          selected = org_id == selected_org_id,
          org_name = comp_org_name,
          org_type_reporting_name = comp_org_type_reporting_name,
          region_name = comp_region_name,
          score = .data[[score]]
        ) %>%
        dplyr::select(
          comparison_level,
          comparison_group,
          selected,
          org_id,
          org_name,
          org_type_reporting_name,
          region_name,
          q_id,
          question,
          year,
          score,
          dplyr::any_of("n")
        ) %>%
        dplyr::arrange(org_name, q_id, year)
    )
  }

  # -----------------------------
  # Below-trust exports
  # -----------------------------
  base <- get_ox_q_aggregate_results()

  if (
    filter_family == "Organisational Structure" &&
    !is.null(team) &&
    team != "All"
  ) {

    selected_team_aliases <- get_team_aliases(directorate, team)

    export_df <- base %>%
      dplyr::filter(
        dim == "Team",
        directorate == .env$directorate
      ) %>%
      dplyr::mutate(
        comparison_level = "Team",
        comparison_group = dim_sub,
        selected = dim_sub %in% selected_team_aliases
      )

  } else if (
    filter_family == "Organisational Structure" &&
    !is.null(directorate) &&
    directorate != "All"
  ) {

    export_df <- base %>%
      dplyr::filter(dim == "Directorate") %>%
      dplyr::mutate(
        comparison_level = "Directorate",
        comparison_group = dim_sub,
        selected = dim_sub == .env$directorate
      )

  } else if (filter_family == "Protected Characteristics") {

    selected_value <- if (
      is.null(protected_value) ||
      length(protected_value) == 0 ||
      is.na(protected_value) ||
      protected_value == "All"
    ) {
      NA_character_
    } else {
      protected_value[[1]]
    }

    export_df <- base %>%
      dplyr::filter(dim == .env$protected_dim) %>%
      dplyr::mutate(
        comparison_level = protected_dim,
        comparison_group = dim_sub,
        selected = !is.na(selected_value) & dim_sub == selected_value
      )

  } else if (filter_family == "Professional Groups") {

    selected_value <- if (
      is.null(professional_value) ||
      length(professional_value) == 0 ||
      is.na(professional_value) ||
      professional_value == "All"
    ) {
      NA_character_
    } else {
      professional_value[[1]]
    }

    export_df <- base %>%
      dplyr::filter(dim == .env$professional_dim) %>%
      dplyr::mutate(
        comparison_level = professional_dim,
        comparison_group = dim_sub,
        selected = !is.na(selected_value) & dim_sub == selected_value
      )

  } else {
    export_df <- tibble::tibble()
  }

  export_df %>%
    dplyr::left_join(question_lookup, by = "q_id") %>%
    dplyr::mutate(
      score = .data[[score]]
    ) %>%
    dplyr::select(
      comparison_level,
      comparison_group,
      selected,
      dplyr::any_of(c("directorate", "dim", "dim_sub")),
      q_id,
      question,
      year,
      score,
      dplyr::any_of("n")
    ) %>%
    dplyr::arrange(comparison_level, comparison_group, q_id, year)
}


get_selected_theme_ids <- function(theme_sel, domain_sel, subdomain_sel = "All") {
  tqm <- get_theme_questions_map() %>%
    mutate(subdomain = coalesce(subdomain, ""))

  if (is.null(subdomain_sel) || subdomain_sel == "All") {
    domain_rows <- tqm %>%
      filter(theme == theme_sel, domain == domain_sel)

    blank_row <- domain_rows %>%
      filter(subdomain == "") %>%
      pull(theme_id) %>%
      unique()

    if (length(blank_row) > 0) {
      blank_row
    } else {
      domain_rows %>%
        pull(theme_id) %>%
        unique()
    }
  } else {
    tqm %>%
      filter(
        theme == theme_sel,
        domain == domain_sel,
        subdomain == subdomain_sel
      ) %>%
      pull(theme_id) %>%
      unique()
  }
}


apply_subdomain_filter <- function(df, subdomain_sel = "All") {
  if (!is.null(subdomain_sel) && subdomain_sel != "All") {
    return(df %>% filter(subdomain == subdomain_sel))
  }

  has_blank_subdomain <- df %>%
    filter(is.na(subdomain) | subdomain == "" | subdomain == "No subdomain") %>%
    nrow() > 0

  if (has_blank_subdomain) {
    df %>%
      filter(is.na(subdomain) | subdomain == "" | subdomain == "No subdomain")
  } else {
    df
  }
}

apply_family_filter <- function(df,
                                filter_family,
                                directorate = NULL,
                                team = NULL,
                                protected_dim = NULL,
                                protected_value = NULL,
                                professional_dim = NULL,
                                professional_value = NULL,
                                comparison = FALSE) {

  if (filter_family == "Organisational Structure") {
    if (!is.null(team) && team != "All") {
      if (comparison) {
        valid_team_names <- get_team_aliases(directorate)
        return(df %>% filter(dim == "Team", dim_sub %in% valid_team_names))
      } else {
        selected_team_aliases <- get_team_aliases(directorate, team)
        return(df %>% filter(dim == "Team", dim_sub %in% selected_team_aliases))
      }
    }

    if (!is.null(directorate) && directorate != "All") {
      selected_directorate <- directorate

      if (comparison) {
        return(df %>% filter(dim == "Directorate"))
      } else {
        return(df %>% filter(
          dim == "Directorate",
          dim_sub == .env$selected_directorate
        ))
      }
    }

    return(df)
  }

  if (filter_family == "Protected Characteristics") {
    df <- df %>% filter(dim == protected_dim)

    if (!comparison && !is.null(protected_value) && protected_value != "All") {
      df <- df %>% filter(dim_sub == protected_value)
    }

    return(df)
  }

  if (filter_family == "Professional Groups") {
    df <- df %>% filter(dim == professional_dim)

    if (!comparison && !is.null(professional_value) && professional_value != "All") {
      df <- df %>% filter(dim_sub == professional_value)
    }

    return(df)
  }

  df
}


# GET CORRECT BENCHMARK DATA BASED ON TAB SELECTION
get_theme_benchmark_df <- function(theme_sel,
                                   domain_sel,
                                   filter_family,
                                   trust_sel = NULL,
                                   directorate = NULL,
                                   team = NULL,
                                   protected_dim = NULL,
                                   protected_value = NULL,
                                   professional_dim = NULL,
                                   professional_value = NULL,
                                   score = "score") {

  empty_df <- tibble::tibble(
    dim_sub = character(),
    score = numeric(),
    selected = logical(),
    benchmark_average = numeric(),
    year = numeric(),
    benchmark_scope = character()
  )

  # Trust-level themes from national theme file
  if (
    filter_family == "Organisational Structure" &&
    (is.null(directorate) || directorate == "All")
  ) {

    trust_row <- get_nat_result_themes() %>%
      dplyr::filter(org_name == trust_sel | org_id == trust_sel) %>%
      dplyr::slice(1)

    if (nrow(trust_row) == 0) {
      return(empty_df)
    }

    selected_org_id <- trust_row$org_id[[1]]

    theme_lookup <- get_theme_questions_map() %>%
      dplyr::filter(theme == theme_sel) %>%
      dplyr::distinct(theme_id, domain)

    base <- get_nat_result_themes() %>%
      dplyr::filter(org_id == selected_org_id) %>%
      dplyr::inner_join(theme_lookup, by = "theme_id")

    if (nrow(base) == 0) {
      return(empty_df)
    }

    latest_year <- max(base$year, na.rm = TRUE)

    return(
      base %>%
        dplyr::filter(year == latest_year) %>%
        dplyr::group_by(domain) %>%
        dplyr::summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(!is.na(score)) %>%
        dplyr::mutate(
          dim_sub = domain,
          selected = domain == domain_sel,
          benchmark_average = mean(score, na.rm = TRUE),
          year = latest_year,
          benchmark_scope = "themes"
        ) %>%
        dplyr::arrange(desc(score)) %>%
        dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope)
    )
  }

  # Oxleas-level themes from ox_theme_results
  base <- get_ox_theme_results() %>%
    dplyr::filter(theme == theme_sel)

  if (filter_family == "Organisational Structure") {

    if (!is.null(team) && team != "All") {
      selected_team_names <- get_team_aliases(directorate, team)

      base <- base %>%
        dplyr::filter(dim == "Team", dim_sub %in% selected_team_names)

    } else if (!is.null(directorate) && directorate != "All") {
      base <- base %>%
        dplyr::filter(dim == "Directorate", dim_sub == .env$directorate)
    }

  } else if (filter_family == "Protected Characteristics") {

    base <- base %>%
      dplyr::filter(
        dim == .env$protected_dim,
        dim_sub == .env$protected_value
      )

  } else if (filter_family == "Professional Groups") {

    base <- base %>%
      dplyr::filter(
        dim == .env$professional_dim,
        dim_sub == .env$professional_value
      )
  }

  if (nrow(base) == 0) {
    return(empty_df)
  }

  latest_year <- max(base$year, na.rm = TRUE)

  base %>%
    dplyr::filter(year == latest_year) %>%
    dplyr::group_by(domain) %>%
    dplyr::summarise(
      score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(!is.na(score)) %>%
    dplyr::mutate(
      dim_sub = domain,
      selected = domain == .env$domain_sel,
      benchmark_average = mean(score, na.rm = TRUE),
      year = latest_year,
      benchmark_scope = "themes"
    ) %>%
    dplyr::arrange(desc(score)) %>%
    dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope)
}


# -----------------------------
# Benchmark data selection
# -----------------------------
get_benchmark_bar_df <- function(theme_sel,
                                 domain_sel,
                                 subdomain_sel = "All",
                                 benchmark_view = NULL,
                                 filter_family = "Organisational Structure",
                                 directorate = NULL,
                                 team = NULL,
                                 trust_sel = NULL,
                                 protected_dim = NULL,
                                 protected_value = NULL,
                                 professional_dim = NULL,
                                 professional_value = NULL,
                                 score = "score") {

  empty_benchmark_df <- function() {
    tibble(
      dim_sub = character(),
      score = numeric(),
      selected = logical(),
      benchmark_average = numeric(),
      year = numeric()
    )
  }

  if (is.null(benchmark_view)) {
    benchmark_view <- dplyr::case_when(
      filter_family == "Protected Characteristics" ~ "demographics_other",
      filter_family == "Professional Groups" ~ "professions_other",
      filter_family == "Organisational Structure" &&
        !is.null(team) &&
        team != "All" ~ "team_benchmark_group",
      filter_family == "Organisational Structure" &&
        !is.null(directorate) &&
        directorate != "All" ~ "directorate_other",
      TRUE ~ "trust_region_type"
    )
  }

  if (benchmark_view == "themes") {
    return(
      get_theme_benchmark_df(
        theme_sel = theme_sel,
        domain_sel = domain_sel,
        filter_family = filter_family,
        trust_sel = trust_sel,
        directorate = directorate,
        team = team,
        protected_dim = protected_dim,
        protected_value = protected_value,
        professional_dim = professional_dim,
        professional_value = professional_value,
        score = score
      )
    )
  }

  # -----------------------------
  # Trust tab: Trust type
  # Matches barometer grouping
  # -----------------------------
  if (
    benchmark_view == "trust_type" &&
    (is.null(directorate) || directorate == "All")
  ) {

    if (is.null(trust_sel)) {
      return(empty_benchmark_df())
    }

    selected_theme_ids <- get_selected_theme_ids(
      theme_sel,
      domain_sel,
      subdomain_sel
    )

    trust_row <- get_nat_result_themes() %>%
      dplyr::filter(org_name == trust_sel | org_id == trust_sel) %>%
      dplyr::slice(1)

    if (nrow(trust_row) == 0) {
      return(empty_benchmark_df())
    }

    selected_org_id <- trust_row$org_id[[1]]
    selected_org_type <- trust_row$org_type[[1]]

    if (
      is.null(selected_org_id) ||
      is.null(selected_org_type) ||
      is.na(selected_org_id) ||
      is.na(selected_org_type)
    ) {
      return(empty_benchmark_df())
    }

    base <- get_nat_result_themes() %>%
      dplyr::filter(
        theme_id %in% selected_theme_ids,
        org_type == .env$selected_org_type
      )

    if (nrow(base) == 0) {
      return(empty_benchmark_df())
    }

    latest_year <- max(base$year, na.rm = TRUE)

    return(
      base %>%
        dplyr::filter(year == latest_year) %>%
        dplyr::group_by(org_id, org_name) %>%
        dplyr::summarise(
          score = if (all(is.na(.data[[score]]))) {
            NA_real_
          } else {
            mean(.data[[score]], na.rm = TRUE)
          },
          .groups = "drop"
        ) %>%
        dplyr::filter(!is.na(score)) %>%
        dplyr::mutate(
          dim_sub = org_name,
          selected = org_id == selected_org_id,
          benchmark_average = mean(score, na.rm = TRUE),
          year = latest_year,
          benchmark_scope = "trust_type"
        ) %>%
        dplyr::arrange(desc(score)) %>%
        dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope)
    )
  }


  # -----------------------------
  # Trust tab: All trusts
  # -----------------------------
  if (benchmark_view == "trust_all_trusts") {

    if (is.null(trust_sel)) {
      return(empty_benchmark_df())
    }

    selected_theme_ids <- get_selected_theme_ids(
      theme_sel,
      domain_sel,
      subdomain_sel
    )


    selected_org_id <- get_nat_result_themes() %>%
      dplyr::filter(org_name == trust_sel | org_id == trust_sel) %>%
      dplyr::slice(1) %>%
      dplyr::pull(org_id)

    if (length(selected_org_id) == 0 || is.na(selected_org_id[[1]])) {
      return(empty_benchmark_df())
    }

    base <- get_nat_result_themes() %>%
      dplyr::filter(theme_id %in% selected_theme_ids)

    if (nrow(base) == 0) {
      return(empty_benchmark_df())
    }

    latest_year <- max(base$year, na.rm = TRUE)

    return(
      base %>%
        dplyr::filter(year == latest_year) %>%
        dplyr::group_by(org_id, org_name) %>%
        dplyr::summarise(
          score = if (all(is.na(.data[[score]]))) {
            NA_real_
          } else {
            mean(.data[[score]], na.rm = TRUE)
          },
          .groups = "drop"
        ) %>%
        dplyr::filter(!is.na(score)) %>%
        dplyr::mutate(
          dim_sub = org_name,
          selected = org_id == selected_org_id[[1]],
          benchmark_average = mean(score, na.rm = TRUE),
          year = latest_year,
          benchmark_scope = "all_trusts"
        ) %>%
        dplyr::arrange(desc(score)) %>%
        dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope)
    )
  }

  team_selected <- (
    filter_family == "Organisational Structure" &&
      !is.null(directorate) &&
      directorate != "All" &&
      !is.null(team) &&
      team != "All"
  )

  if (!team_selected) {
    team <- NULL
  }

  # -----------------------------
  # Demographics benchmark
  # Shows all values for the selected demographic dimension
  # -----------------------------
  if (filter_family == "Protected Characteristics") {

    if (is.null(protected_dim) || length(protected_dim) == 0) {
      return(empty_benchmark_df())
    }

    selected_value <- if (
      is.null(protected_value) ||
      length(protected_value) == 0 ||
      is.na(protected_value) ||
      protected_value == "All"
    ) {
      NA_character_
    } else {
      protected_value[[1]]
    }

    base <- files$ox_theme_results %>%
      filter(
        theme == theme_sel,
        domain == domain_sel
      ) %>%
      apply_subdomain_filter(subdomain_sel) %>%
      filter(dim == .env$protected_dim)

    if (nrow(base) == 0) {
      return(empty_benchmark_df())
    }

    latest_year <- max(base$year, na.rm = TRUE)

    return(
      base %>%
        filter(year == latest_year) %>%
        group_by(dim_sub) %>%
        summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(!is.na(score)) %>%
        mutate(
          selected = !is.na(selected_value) & dim_sub == selected_value,
          benchmark_average = mean(score, na.rm = TRUE),
          year = latest_year
        ) %>%
        arrange(desc(score)) %>%
        select(dim_sub, score, selected, benchmark_average, year)
    )
  }

  # -----------------------------
  # Professions benchmark
  # Shows all values for the selected professional dimension
  # -----------------------------
  if (filter_family == "Professional Groups") {

    if (is.null(professional_dim) || length(professional_dim) == 0) {
      return(empty_benchmark_df())
    }

    selected_value <- if (
      is.null(professional_value) ||
      length(professional_value) == 0 ||
      is.na(professional_value) ||
      professional_value == "All"
    ) {
      NA_character_
    } else {
      professional_value[[1]]
    }

    base <- files$ox_theme_results %>%
      filter(
        theme == theme_sel,
        domain == domain_sel
      ) %>%
      apply_subdomain_filter(subdomain_sel) %>%
      filter(dim == .env$professional_dim)

    if (nrow(base) == 0) {
      return(empty_benchmark_df())
    }

    latest_year <- max(base$year, na.rm = TRUE)

    return(
      base %>%
        filter(year == latest_year) %>%
        group_by(dim_sub) %>%
        summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(!is.na(score)) %>%
        mutate(
          selected = !is.na(selected_value) & dim_sub == selected_value,
          benchmark_average = mean(score, na.rm = TRUE),
          year = latest_year
        ) %>%
        arrange(desc(score)) %>%
        select(dim_sub, score, selected, benchmark_average, year)
    )
  }

  if (filter_family != "Organisational Structure") {
    return(empty_benchmark_df())
  }

  # -----------------------------
  # Trust-level benchmark
  # Compares selected trust with trusts in same region and reporting type
  # -----------------------------
  if (is.null(directorate) || directorate == "All") {

    if (is.null(trust_sel)) {
      return(empty_benchmark_df())
    }

    selected_theme_ids <- get_selected_theme_ids(
      theme_sel,
      domain_sel,
      subdomain_sel
    )

    trust_meta <- files$nat_result_themes %>%
      filter(org_name == trust_sel | org_id == trust_sel) %>%
      summarise(
        selected_org_id = first(na.omit(org_id)),
        selected_type = first(na.omit(org_type_reporting_name)),
        selected_region = first(na.omit(region_name)),
        .groups = "drop"
      )

    if (nrow(trust_meta) == 0) {
      return(empty_benchmark_df())
    }

    selected_org_id <- trust_meta$selected_org_id[[1]]
    selected_type <- trust_meta$selected_type[[1]]
    selected_region <- trust_meta$selected_region[[1]]


    if (
      length(selected_type) == 0 ||
      length(selected_region) == 0 ||
      is.na(selected_type) ||
      is.na(selected_region)
    ) {
      return(empty_benchmark_df())
    }

    base <- files$nat_result_themes %>%
      filter(
        theme_id %in% selected_theme_ids,
        org_type_reporting_name == .env$selected_type,
        region_name == .env$selected_region
      )

    if (nrow(base) == 0) {
      return(empty_benchmark_df())
    }

    latest_year <- max(base$year, na.rm = TRUE)

    return(
      base %>%
        filter(year == latest_year) %>%
        group_by(org_id, org_name) %>%
        summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        filter(!is.na(score)) %>%
        mutate(
          dim_sub = org_name,
          selected = org_id == selected_org_id,
          benchmark_average = mean(score, na.rm = TRUE),
          year = latest_year
        ) %>%
        arrange(desc(score)) %>%
        select(dim_sub, score, selected, benchmark_average, year)
    )
  }

  base <- files$ox_theme_results %>%
    filter(theme == theme_sel, domain == domain_sel) %>%
    apply_subdomain_filter(subdomain_sel)

  latest_year <- max(base$year, na.rm = TRUE)

  base <- base %>%
    filter(year == latest_year)

  if (!is.null(team) && team != "All") {

    selected_team <- files$ox_teams_map %>%
      dplyr::filter(
        directorate == !!directorate,
        team_full == !!team | team_short == !!team
      ) %>%
      dplyr::slice(1)

    selected_team_names <- if (nrow(selected_team) > 0) {
      c(selected_team$team_short, selected_team$team_full) %>%
        stats::na.omit() %>%
        unique()



    } else {
      team
    }

    # -----------------------------
    # Team tab: All teams in selected directorate
    # -----------------------------
    # -----------------------------
    # Team tab: All teams in selected directorate
    # -----------------------------
    if (benchmark_view == "team_directorate") {

      directorate_team_names <- files$ox_teams_map %>%
        dplyr::filter(directorate == .env$directorate) %>%
        dplyr::transmute(team_name = team_short) %>%
        dplyr::bind_rows(
          files$ox_teams_map %>%
            dplyr::filter(directorate == .env$directorate) %>%
            dplyr::transmute(team_name = team_full)
        ) %>%
        dplyr::pull(team_name) %>%
        stats::na.omit() %>%
        unique()

      return(
        base %>%
          dplyr::filter(
            dim == "Team",
            dim_sub %in% directorate_team_names
          ) %>%
          dplyr::group_by(dim_sub) %>%
          dplyr::summarise(
            score = if (all(is.na(.data[[score]]))) {
              NA_real_
            } else {
              mean(.data[[score]], na.rm = TRUE)
            },
            .groups = "drop"
          ) %>%
          dplyr::filter(!is.na(score)) %>%
          dplyr::mutate(
            selected = dim_sub %in% selected_team_names,
            benchmark_average = mean(score, na.rm = TRUE),
            year = latest_year,
            benchmark_scope = "all_teams_directorate"
          ) %>%
          dplyr::arrange(desc(score)) %>%
          dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope)
      )
    }

    # -----------------------------
    # Team tab: All teams in trust
    # -----------------------------
    if (benchmark_view == "team_trust") {

      return(
        base %>%
          dplyr::filter(dim == "Team") %>%
          dplyr::group_by(dim_sub) %>%
          dplyr::summarise(
            score = if (all(is.na(.data[[score]]))) {
              NA_real_
            } else {
              mean(.data[[score]], na.rm = TRUE)
            },
            .groups = "drop"
          ) %>%
          dplyr::filter(!is.na(score)) %>%
          dplyr::mutate(
            selected = dim_sub %in% selected_team_names,
            benchmark_average = mean(score, na.rm = TRUE),
            year = latest_year,
            benchmark_scope = "all_teams_trust"
          ) %>%
          dplyr::arrange(desc(score)) %>%
          dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope)
      )
    }

    directorate_team_benchmark <- function() {

      team_row <- base %>%
        dplyr::filter(
          dim == "Team",
          dim_sub %in% selected_team_names
        ) %>%
        dplyr::group_by(dim_sub) %>%
        dplyr::summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(!is.na(score)) %>%
        dplyr::slice(1) %>%
        dplyr::mutate(
          dim_sub = team,
          selected = TRUE,
          benchmark_average = score,
          year = latest_year,
          benchmark_scope = "directorate_value"
        )

      directorate_row <- base %>%
        dplyr::filter(
          dim == "Directorate",
          dim_sub == .env$directorate
        ) %>%
        dplyr::group_by(dim_sub) %>%
        dplyr::summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(!is.na(score)) %>%
        dplyr::mutate(
          dim_sub = paste0(directorate, " Directorate"),
          selected = domain == .env$domain_sel,
          benchmark_average = score,
          year = latest_year,
          benchmark_scope = "directorate_value"
        )

      dplyr::bind_rows(team_row, directorate_row) %>%
        dplyr::arrange(desc(score)) %>%
        dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope)
    }

    if (
      nrow(selected_team) == 0 ||
      is.na(selected_team$benchmark_group[[1]])
    ) {
      return(directorate_team_benchmark())
    }

    benchmark_group <- selected_team$benchmark_group[[1]]

    benchmark_teams <- files$ox_teams_map %>%
      dplyr::filter(benchmark_group == !!benchmark_group) %>%
      dplyr::transmute(team_name = team_short) %>%
      dplyr::bind_rows(
        files$ox_teams_map %>%
          dplyr::filter(benchmark_group == !!benchmark_group) %>%
          dplyr::transmute(team_name = team_full)
      ) %>%
      dplyr::pull(team_name) %>%
      stats::na.omit() %>%
      unique()

    out <- base %>%
      dplyr::filter(dim == "Team", dim_sub %in% benchmark_teams) %>%
      dplyr::group_by(dim_sub) %>%
      dplyr::summarise(
        score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(!is.na(score)) %>%
      dplyr::mutate(
        selected = dim_sub %in% selected_team_names,
        benchmark_average = mean(score, na.rm = TRUE),
        year = latest_year,
        benchmark_scope = "benchmark_group"
      ) %>%
      dplyr::arrange(desc(score))

    if (nrow(out) <= 1) {
      return(directorate_team_benchmark())
    }

    return(
      out %>%
        dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope)
    )
  }

  base %>%
    filter(dim == "Directorate") %>%
    group_by(dim_sub) %>%
    summarise(
      score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(score)) %>%
    mutate(
      selected = dim_sub == directorate,
      benchmark_average = mean(score, na.rm = TRUE),
      year = latest_year
    ) %>%
    arrange(desc(score)) %>%
    select(dim_sub, score, selected, benchmark_average, year)
}


get_metric_data_df <- function(trust_sel,
                               theme_sel,
                               domain_sel,
                               subdomain_sel = "All",
                               filter_family = "Organisational Structure",
                               directorate = NULL,
                               team = NULL,
                               protected_dim = NULL,
                               protected_value = NULL,
                               professional_dim = NULL,
                               professional_value = NULL,
                               score = "score",
                               return_type = "gauge") {

  team_selected <- (
    filter_family == "Organisational Structure" &&
      !is.null(directorate) &&
      directorate != "All" &&
      !is.null(team) &&
      team != "All"
  )

  if (!team_selected) {
    team <- NULL
  }
  trust_only <- (
    filter_family == "Organisational Structure" &&
      (is.null(directorate) || identical(directorate, "All"))
  )

  group_label <- if (filter_family == "Organisational Structure" && !is.null(team) && team != "All") {
    "Teams"
  } else if (filter_family == "Organisational Structure" && !is.null(directorate) && directorate != "All") {
    "Directorates"
  } else if (filter_family == "Protected Characteristics") {
    protected_dim
  } else if (filter_family == "Professional Groups") {
    professional_dim
  } else {
    "Similar Trusts"
  }

  if (trust_only) {

    selected_theme_ids <- get_selected_theme_ids(theme_sel, domain_sel, subdomain_sel)

    trust_row <- files$nat_result_themes %>%
      filter(org_name == trust_sel | org_id == trust_sel) %>%
      slice(1)

    selected_org_id   <- trust_row$org_id[[1]]
    selected_org_type <- trust_row$org_type[[1]]

    comparison_scores <- files$nat_result_themes %>%
      filter(
        theme_id %in% selected_theme_ids,
        org_type == selected_org_type
      ) %>%
      group_by(year, org_id, org_name, org_type) %>%
      summarise(
        score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(score)) %>%
      group_by(year) %>%
      arrange(desc(score), .by_group = TRUE) %>%
      mutate(rank = row_number()) %>%
      ungroup()

    selected_series <- comparison_scores %>%
      filter(org_id == selected_org_id)

  } else {

    base <- files$ox_theme_results %>%
      filter(theme == theme_sel, domain == domain_sel) %>%
      apply_subdomain_filter(subdomain_sel)

    comparison_set <- apply_family_filter(
      df = base,
      filter_family = filter_family,
      directorate = directorate,
      team = team,
      protected_dim = protected_dim,
      protected_value = protected_value,
      professional_dim = professional_dim,
      professional_value = professional_value,
      comparison = TRUE
    )

    comparison_scores <- comparison_set %>%
      group_by(year, dim_sub) %>%
      summarise(
        score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(score)) %>%
      group_by(year) %>%
      arrange(desc(score), .by_group = TRUE) %>%
      mutate(rank = row_number()) %>%
      ungroup()

    selector <- if (filter_family == "Organisational Structure" && !is.null(team) && team != "All") {
      function(df) df %>% filter(dim_sub %in% get_team_aliases(directorate, team))
    } else {
      selected_dim_sub <- if (filter_family == "Organisational Structure" && !is.null(directorate) && directorate != "All") {
        directorate
      } else if (filter_family == "Protected Characteristics") {
        if (is.null(protected_value)) NA_character_ else protected_value
      } else if (filter_family == "Professional Groups") {
        if (is.null(professional_value)) NA_character_ else professional_value
      } else {
        NA_character_
      }

      function(df) df %>% filter(dim_sub == selected_dim_sub)
    }

    selected_series <- comparison_scores %>%
      group_by(year) %>%
      group_modify(~ {
        selected_row <- selector(.x) %>% slice(1)

        if (nrow(selected_row) == 0) {
          return(tibble(
            rank = NA_real_,
            score = NA_real_
          ))
        }

        tibble(
          rank = selected_row$rank[[1]],
          score = selected_row$score[[1]]
        )
      }) %>%
      ungroup()
  }

  if (return_type == "line") {
    return(selected_series %>% select(year, score))
  }

  if (nrow(selected_series) == 0 || all(is.na(selected_series$year))) {
    return(list(
      year = NA,
      top = NA,
      bottom = NA,
      val = NA,
      display_val = "No data"
    ))
  }

  latest_year <- max(selected_series$year, na.rm = TRUE)

  selected_row <- selected_series %>%
    filter(year == latest_year) %>%
    slice(1)

  if (nrow(selected_row) == 0 || is.na(selected_row$rank[[1]])) {
    return(list(
      year = latest_year,
      top = NA,
      bottom = NA,
      val = NA,
      display_val = "No data"
    ))
  }

  top_n <- comparison_scores %>%
    filter(year == latest_year) %>%
    nrow()

  list(
    year = latest_year,
    top = top_n,
    bottom = 1,
    val = rank_to_val(selected_row$rank[[1]], top_n),
    display_val = rank_label(
      selected_row$rank[[1]],
      top_n,
      group_label,
      extra_line = if (team_selected) {
        "(in your directorate who have data)"
      } else {
        NULL
      }
    )
  )
}
