# -----------------------------
# Build Data Helper Functions
# -----------------------------

# Instantiate an empty data frame for comparison data.
empty_comparison_df <- function() {
  tibble::tibble(
    dim_sub = character(),
    score = numeric(),
    selected = logical(),
    benchmark_average = numeric(),
    year = numeric(),
    benchmark_scope = character(),
    rank = numeric()
  )
}

finalise_comparison_df <- function(df, context) {
  out <- df %>%
    dplyr::filter(!is.na(score)) %>%
    dplyr::group_by(year) %>%
    dplyr::arrange(dplyr::desc(score), .by_group = TRUE) %>%
    dplyr::mutate(rank = dplyr::row_number()) %>%
    dplyr::ungroup()

  attr(out, "group_label") <- context$group_label
  attr(out, "team_selected") <- context$team_selected

  out
}

# Outputs a summary of which filters are currently active
resolve_filter_context <- function(filter_family = "Organisational Structure",
                                   directorate = NULL,
                                   team = NULL,
                                   protected_dim = NULL,
                                   protected_value = NULL,
                                   professional_dim = NULL,
                                   professional_value = NULL) {

  team_selected <- (
    filter_family == "Organisational Structure" &&
      !is.null(directorate) &&
      directorate != "All" &&
      !is.null(team) &&
      team != "All"
  )

  trust_only <- (
    filter_family == "Organisational Structure" &&
      (is.null(directorate) || identical(directorate, "All"))
  )

  if (!team_selected) {
    team <- NULL
  }

  selected_dim_sub <- if (
    filter_family == "Organisational Structure" &&
    !is.null(directorate) &&
    directorate != "All"
  ) {
    directorate
  } else if (filter_family == "Protected Characteristics") {
    if (is.null(protected_value)) NA_character_ else protected_value
  } else if (filter_family == "Professional Groups") {
    if (is.null(professional_value)) NA_character_ else professional_value
  } else {
    NA_character_
  }

  group_label <- if (
    filter_family == "Organisational Structure" &&
    !is.null(team) &&
    team != "All"
  ) {
    "Teams"
  } else if (
    filter_family == "Organisational Structure" &&
    !is.null(directorate) &&
    directorate != "All"
  ) {
    "Directorates"
  } else if (filter_family == "Protected Characteristics") {
    protected_dim
  } else if (filter_family == "Professional Groups") {
    professional_dim
  } else {
    "Similar Trusts"
  }

  list(
    filter_family = filter_family,
    trust_only = trust_only,
    team_selected = team_selected,
    directorate = directorate,
    team = team,
    protected_dim = protected_dim,
    protected_value = protected_value,
    professional_dim = professional_dim,
    professional_value = professional_value,
    selected_dim_sub = selected_dim_sub,
    group_label = group_label
  )
}

# Returns Question IDs related to a particular combination of theme / domain /
# sub-domain.
get_selected_q_ids <- function(theme_sel, domain_sel, subdomain_sel = "All") {
  get_theme_questions_map() %>%
    dplyr::mutate(subdomain = dplyr::coalesce(subdomain, "")) %>%
    dplyr::filter(
      theme == theme_sel,
      domain == domain_sel,
      if (is.null(subdomain_sel) || subdomain_sel == "All") TRUE else subdomain == subdomain_sel
    ) %>%
    dplyr::pull(q_id) %>%
    unique()
}

get_question_labels <- function(q_ids = NULL) {
  out <- get_question_scores_map() %>%
    dplyr::select(dplyr::any_of(c("q_id", "q_text_short", "q_text"))) %>%
    dplyr::distinct(q_id, .keep_all = TRUE)

  if (!"q_text_short" %in% names(out)) {
    out$q_text_short <- NA_character_
  }

  if (!"q_text" %in% names(out)) {
    out$q_text <- NA_character_
  }

  out <- out %>%
    dplyr::mutate(
      question_label = dplyr::coalesce(
        as.character(q_text_short),
        as.character(q_text),
        as.character(q_id)
      )
    ) %>%
    dplyr::select(q_id, question_label)

  if (!is.null(q_ids)) {
    out <- tibble::tibble(q_id = q_ids) %>%
      dplyr::left_join(out, by = "q_id") %>%
      dplyr::mutate(
        question_label = dplyr::coalesce(
          as.character(question_label),
          as.character(q_id)
        )
      )
  }

  out
}

# -----------------------------
# Shared Comparison Data
# -----------------------------

build_comparison_df <- function(theme_sel,
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

  context <- resolve_filter_context(
    filter_family = filter_family,
    directorate = directorate,
    team = team,
    protected_dim = protected_dim,
    protected_value = protected_value,
    professional_dim = professional_dim,
    professional_value = professional_value
  )

  team <- context$team

  if (is.null(benchmark_view)) {
    benchmark_view <- dplyr::case_when(
      filter_family == "Protected Characteristics" ~ "demographics_other",
      filter_family == "Professional Groups" ~ "professions_other",
      filter_family == "Organisational Structure" &&
        !is.null(context$team) &&
        context$team != "All" ~ "team_benchmark_group",
      filter_family == "Organisational Structure" &&
        !is.null(directorate) &&
        directorate != "All" ~ "directorate_other",
      TRUE ~ "trust_region_type"
    )
  }

  if (benchmark_view == "themes") {
    return(
      build_theme_benchmark_df(
        theme_sel = theme_sel,
        domain_sel = domain_sel,
        filter_family = filter_family,
        trust_sel = trust_sel,
        directorate = directorate,
        team = context$team,
        protected_dim = protected_dim,
        protected_value = protected_value,
        professional_dim = professional_dim,
        professional_value = professional_value,
        score = score
      ) %>%
        finalise_comparison_df(context)
    )
  }

  # -----------------------------
  # Trust-level comparisons
  # -----------------------------
  if (context$trust_only) {

    if (is.null(trust_sel)) {
      return(empty_comparison_df())
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
      return(empty_comparison_df())
    }

    selected_org_id <- trust_row$org_id[[1]]

    if (benchmark_view == "trust_type") {

      selected_org_type <- trust_row$org_type[[1]]

      if (is.null(selected_org_type) || is.na(selected_org_type)) {
        return(empty_comparison_df())
      }

      out <- get_nat_result_themes() %>%
        dplyr::filter(
          theme_id %in% selected_theme_ids,
          org_type == .env$selected_org_type
        ) %>%
        dplyr::group_by(year, org_id, org_name) %>%
        dplyr::summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          dim_sub = org_name,
          selected = org_id == selected_org_id,
          benchmark_scope = "trust_type"
        )

    } else if (benchmark_view == "trust_all_trusts") {

      out <- get_nat_result_themes() %>%
        dplyr::filter(theme_id %in% selected_theme_ids) %>%
        dplyr::group_by(year, org_id, org_name) %>%
        dplyr::summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          dim_sub = org_name,
          selected = org_id == selected_org_id,
          benchmark_scope = "all_trusts"
        )

    } else {

      trust_meta <- get_nat_result_themes() %>%
        dplyr::filter(org_name == trust_sel | org_id == trust_sel) %>%
        dplyr::summarise(
          selected_org_id = dplyr::first(stats::na.omit(org_id)),
          selected_type = dplyr::first(stats::na.omit(org_type_reporting_name)),
          selected_region = dplyr::first(stats::na.omit(region_name)),
          .groups = "drop"
        )

      selected_type <- trust_meta$selected_type[[1]]
      selected_region <- trust_meta$selected_region[[1]]

      if (
        length(selected_type) == 0 ||
        length(selected_region) == 0 ||
        is.na(selected_type) ||
        is.na(selected_region)
      ) {
        return(empty_comparison_df())
      }

      out <- get_nat_result_themes() %>%
        dplyr::filter(
          theme_id %in% selected_theme_ids,
          org_type_reporting_name == .env$selected_type,
          region_name == .env$selected_region
        ) %>%
        dplyr::group_by(year, org_id, org_name) %>%
        dplyr::summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          dim_sub = org_name,
          selected = org_id == selected_org_id,
          benchmark_scope = "trust_region_type"
        )
    }

    return(
      out %>%
        dplyr::group_by(year) %>%
        dplyr::mutate(benchmark_average = mean(score, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope) %>%
        finalise_comparison_df(context)
    )
  }

  # -----------------------------
  # Oxleas-level comparisons
  # -----------------------------
  base <- get_ox_theme_results() %>%
    dplyr::filter(
      theme == theme_sel,
      domain == domain_sel
    ) %>%
    apply_subdomain_filter(subdomain_sel)

  if (nrow(base) == 0) {
    return(empty_comparison_df())
  }

  # -----------------------------
  # Demographics
  # -----------------------------
  if (filter_family == "Protected Characteristics") {

    if (is.null(protected_dim) || length(protected_dim) == 0) {
      return(empty_comparison_df())
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

    return(
      base %>%
        dplyr::filter(dim == .env$protected_dim) %>%
        dplyr::group_by(year, dim_sub) %>%
        dplyr::summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          selected = !is.na(selected_value) & dim_sub == selected_value,
          benchmark_scope = "demographics_other"
        ) %>%
        dplyr::group_by(year) %>%
        dplyr::mutate(benchmark_average = mean(score, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope) %>%
        finalise_comparison_df(context)
    )
  }

  # -----------------------------
  # Professions
  # -----------------------------
  if (filter_family == "Professional Groups") {

    if (is.null(professional_dim) || length(professional_dim) == 0) {
      return(empty_comparison_df())
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

    return(
      base %>%
        dplyr::filter(dim == .env$professional_dim) %>%
        dplyr::group_by(year, dim_sub) %>%
        dplyr::summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          selected = !is.na(selected_value) & dim_sub == selected_value,
          benchmark_scope = "professions_other"
        ) %>%
        dplyr::group_by(year) %>%
        dplyr::mutate(benchmark_average = mean(score, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope) %>%
        finalise_comparison_df(context)
    )
  }

  if (filter_family != "Organisational Structure") {
    return(empty_comparison_df())
  }

  # -----------------------------
  # Directorate comparison
  # -----------------------------
  if (is.null(directorate) || directorate == "All") {
    return(empty_comparison_df())
  }

  if (is.null(context$team) || context$team == "All") {
    return(
      base %>%
        dplyr::filter(dim == "Directorate") %>%
        dplyr::group_by(year, dim_sub) %>%
        dplyr::summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          selected = dim_sub == directorate,
          benchmark_scope = "directorate_other"
        ) %>%
        dplyr::group_by(year) %>%
        dplyr::mutate(benchmark_average = mean(score, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope) %>%
        finalise_comparison_df(context)
    )
  }

  # -----------------------------
  # Team comparison
  # -----------------------------
  selected_team <- get_ox_teams_map() %>%
    dplyr::filter(
      directorate == .env$directorate,
      team_full == .env$team | team_short == .env$team
    ) %>%
    dplyr::slice(1)

  selected_team_names <- if (nrow(selected_team) > 0) {
    c(selected_team$team_short, selected_team$team_full) %>%
      stats::na.omit() %>%
      unique()
  } else {
    team
  }

  build_team_directorate_comparison <- function() {

    team_row <- base %>%
      dplyr::filter(
        dim == "Team",
        dim_sub %in% selected_team_names
      ) %>%
      dplyr::group_by(year, dim_sub) %>%
      dplyr::summarise(
        score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        dim_sub = team,
        selected = TRUE,
        benchmark_scope = "directorate_value"
      )

    directorate_row <- base %>%
      dplyr::filter(
        dim == "Directorate",
        dim_sub == .env$directorate
      ) %>%
      dplyr::group_by(year, dim_sub) %>%
      dplyr::summarise(
        score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        dim_sub = paste0(directorate, " Directorate"),
        selected = FALSE,
        benchmark_scope = "directorate_value"
      )

    dplyr::bind_rows(team_row, directorate_row) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(benchmark_average = mean(score, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope) %>%
      finalise_comparison_df(context)
  }

  if (benchmark_view == "team_directorate") {

    directorate_team_names <- get_ox_teams_map() %>%
      dplyr::filter(directorate == .env$directorate) %>%
      dplyr::transmute(team_name = team_short) %>%
      dplyr::bind_rows(
        get_ox_teams_map() %>%
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
        dplyr::group_by(year, dim_sub) %>%
        dplyr::summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          selected = dim_sub %in% selected_team_names,
          benchmark_scope = "all_teams_directorate"
        ) %>%
        dplyr::group_by(year) %>%
        dplyr::mutate(benchmark_average = mean(score, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope) %>%
        finalise_comparison_df(context)
    )
  }

  if (benchmark_view == "team_trust") {
    return(
      base %>%
        dplyr::filter(dim == "Team") %>%
        dplyr::group_by(year, dim_sub) %>%
        dplyr::summarise(
          score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::mutate(
          selected = dim_sub %in% selected_team_names,
          benchmark_scope = "all_teams_trust"
        ) %>%
        dplyr::group_by(year) %>%
        dplyr::mutate(benchmark_average = mean(score, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope) %>%
        finalise_comparison_df(context)
    )
  }

  if (
    nrow(selected_team) == 0 ||
    is.na(selected_team$benchmark_group[[1]])
  ) {
    return(build_team_directorate_comparison())
  }

  benchmark_group <- selected_team$benchmark_group[[1]]

  benchmark_teams <- get_ox_teams_map() %>%
    dplyr::filter(benchmark_group == !!benchmark_group) %>%
    dplyr::transmute(team_name = team_short) %>%
    dplyr::bind_rows(
      get_ox_teams_map() %>%
        dplyr::filter(benchmark_group == !!benchmark_group) %>%
        dplyr::transmute(team_name = team_full)
    ) %>%
    dplyr::pull(team_name) %>%
    stats::na.omit() %>%
    unique()

  out <- base %>%
    dplyr::filter(
      dim == "Team",
      dim_sub %in% benchmark_teams
    ) %>%
    dplyr::group_by(year, dim_sub) %>%
    dplyr::summarise(
      score = if (all(is.na(.data[[score]]))) NA_real_ else mean(.data[[score]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      selected = dim_sub %in% selected_team_names,
      benchmark_scope = "benchmark_group"
    ) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(benchmark_average = mean(score, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dim_sub, score, selected, benchmark_average, year, benchmark_scope) %>%
    finalise_comparison_df(context)

  latest_year <- suppressWarnings(max(out$year, na.rm = TRUE))

  if (
    is.infinite(latest_year) ||
    is.na(latest_year) ||
    out %>% dplyr::filter(year == latest_year) %>% nrow() <= 1
  ) {
    return(build_team_directorate_comparison())
  }

  out
}


# -----------------------------
# Question Table Data
# -----------------------------

# Builds dataset for the question table

build_question_table_df <- function(trust_sel,
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

  context <- resolve_filter_context(
    filter_family = filter_family,
    directorate = directorate,
    team = team,
    protected_dim = protected_dim,
    protected_value = protected_value,
    professional_dim = professional_dim,
    professional_value = professional_value
  )

  selected_q_ids <- get_selected_q_ids(
    theme_sel = theme_sel,
    domain_sel = domain_sel,
    subdomain_sel = subdomain_sel
  )

  question_labels <- get_question_labels(selected_q_ids)

  get_latest_response_counts <- function(df) {
    latest_year <- suppressWarnings(max(df$year, na.rm = TRUE))

    if (is.infinite(latest_year) || is.na(latest_year)) {
      return(
        tibble::tibble(
          q_id = unique(df$q_id),
          Responses = NA_real_
        )
      )
    }

    count_name <- paste0("Responses (", latest_year, ")")

    if (!"n" %in% names(df)) {
      return(
        tibble::tibble(q_id = unique(df$q_id)) %>%
          dplyr::mutate(!!count_name := NA_real_)
      )
    }

    df %>%
      dplyr::filter(year == latest_year) %>%
      dplyr::group_by(q_id) %>%
      dplyr::summarise(
        !!count_name := if (all(is.na(n))) {
          NA_real_
        } else {
          sum(n, na.rm = TRUE)
        },
        .groups = "drop"
      )
  }

  build_question_table <- function(question_rows, years) {
    summarised <- question_rows %>%
      dplyr::group_by(q_id, year) %>%
      dplyr::summarise(
        score = if (all(is.na(.data[[score]]))) {
          NA_real_
        } else {
          mean(.data[[score]], na.rm = TRUE)
        },
        has_n = if ("n" %in% names(question_rows)) {
          any(!is.na(.data[["n"]]))
        } else {
          FALSE
        },
        row_present = TRUE,
        .groups = "drop"
      )

    full <- tidyr::expand_grid(
      q_id = selected_q_ids,
      year = years
    ) %>%
      dplyr::left_join(question_labels, by = "q_id") %>%
      dplyr::left_join(summarised, by = c("q_id", "year")) %>%
      dplyr::mutate(
        missing_reason = dplyr::case_when(
          is.na(row_present) ~ "not_available",
          is.na(score) & !has_n ~ "not_available",
          is.na(score) & has_n ~ "suppressed",
          TRUE ~ "available"
        )
      )

    score_wide <- full %>%
      dplyr::select(q_id, question_label, year, score) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = score,
        names_sort = TRUE
      )

    reason_wide <- full %>%
      dplyr::select(q_id, year, missing_reason) %>%
      tidyr::pivot_wider(
        names_from = year,
        values_from = missing_reason,
        names_prefix = "missing_reason_",
        names_sort = TRUE
      )

    score_wide %>%
      dplyr::left_join(reason_wide, by = "q_id") %>%
      dplyr::left_join(get_latest_response_counts(question_rows), by = "q_id") %>%
      dplyr::arrange(q_id) %>%
      dplyr::select(-q_id) %>%
      dplyr::rename(Question = question_label)
  }

  if (context$trust_only) {
    selected_org_id <- get_nat_result_themes() %>%
      dplyr::filter(org_name == trust_sel | org_id == trust_sel) %>%
      dplyr::slice(1) %>%
      dplyr::pull(org_id) %>%
      dplyr::first()

    question_rows <- get_nat_result_scores() %>%
      dplyr::filter(
        org_id == selected_org_id,
        q_id %in% selected_q_ids
      )

    years <- sort(unique(get_nat_result_scores()$year))

    return(build_question_table(question_rows, years))
  }

  question_rows <- get_ox_q_aggregate_results() %>%
    dplyr::filter(q_id %in% selected_q_ids) %>%
    apply_family_filter(
      filter_family = context$filter_family,
      directorate = context$directorate,
      team = context$team,
      protected_dim = context$protected_dim,
      protected_value = context$protected_value,
      professional_dim = context$professional_dim,
      professional_value = context$professional_value,
      comparison = FALSE
    )

  years <- sort(unique(get_ox_q_aggregate_results()$year))

  build_question_table(question_rows, years)
}

# -----------------------------
# Benchmark Data - 'Themes in current topic'
# -----------------------------

# Builds 'themes in current topic' benchmark table option

build_theme_benchmark_df <- function(theme_sel,
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
# Benchmark Data - All other benchmark options
# -----------------------------

# Builds benchmark data for all options except 'themes in current topic'

build_benchmark_bar_df <- function(theme_sel,
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

  comparison_df <- build_comparison_df(
    theme_sel = theme_sel,
    domain_sel = domain_sel,
    subdomain_sel = subdomain_sel,
    benchmark_view = benchmark_view,
    filter_family = filter_family,
    directorate = directorate,
    team = team,
    trust_sel = trust_sel,
    protected_dim = protected_dim,
    protected_value = protected_value,
    professional_dim = professional_dim,
    professional_value = professional_value,
    score = score
  )

  if (nrow(comparison_df) == 0) {
    return(empty_comparison_df())
  }

  latest_year <- max(comparison_df$year, na.rm = TRUE)

  comparison_df %>%
    dplyr::filter(year == latest_year) %>%
    dplyr::arrange(dplyr::desc(score))
}

# -----------------------------
# Line & Gauge Chart Data
# -----------------------------

#Builds dataset used by the gauge chart (barometer) and line chart

build_gauge_bar_data_df <- function(trust_sel,
                                 theme_sel,
                                 domain_sel,
                                 subdomain_sel = "All",
                                 benchmark_view = NULL,
                                 filter_family = "Organisational Structure",
                                 directorate = NULL,
                                 team = NULL,
                                 protected_dim = NULL,
                                 protected_value = NULL,
                                 professional_dim = NULL,
                                 professional_value = NULL,
                                 score = "score",
                                 return_type = "gauge") {

  comparison_df <- build_comparison_df(
    theme_sel = theme_sel,
    domain_sel = domain_sel,
    subdomain_sel = subdomain_sel,
    benchmark_view = benchmark_view,
    filter_family = filter_family,
    directorate = directorate,
    team = team,
    trust_sel = trust_sel,
    protected_dim = protected_dim,
    protected_value = protected_value,
    professional_dim = professional_dim,
    professional_value = professional_value,
    score = score
  )

  if (return_type == "line") {
    return(
      comparison_df %>%
        dplyr::filter(selected) %>%
        dplyr::group_by(year) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup() %>%
        dplyr::select(year, score) %>%
        dplyr::arrange(year)
    )
  }

  if (nrow(comparison_df) == 0 || all(is.na(comparison_df$year))) {
    return(list(
      year = NA,
      top = NA,
      bottom = NA,
      val = NA,
      display_val = "No data"
    ))
  }

  latest_year <- max(comparison_df$year, na.rm = TRUE)

  latest_comparison <- comparison_df %>%
    dplyr::filter(year == latest_year)

  selected_row <- latest_comparison %>%
    dplyr::filter(selected) %>%
    dplyr::slice(1)

  if (nrow(selected_row) == 0 || is.na(selected_row$rank[[1]])) {
    return(list(
      year = latest_year,
      top = NA,
      bottom = NA,
      val = NA,
      display_val = "No data"
    ))
  }

  group_label <- attr(comparison_df, "group_label", exact = TRUE)
  if (is.null(group_label)) {
    group_label <- "Similar Trusts"
  }

  team_selected <- isTRUE(attr(comparison_df, "team_selected", exact = TRUE))

  top_n <- nrow(latest_comparison)

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


# -----------------------------
# .CSV Data
# -----------------------------

# Builds dataset for download .csv functionality

build_download_csv_df <- function(trust_sel,
                                  filter_family = "Organisational Structure",
                                  directorate = NULL,
                                  team = NULL,
                                  protected_dim = NULL,
                                  protected_value = NULL,
                                  professional_dim = NULL,
                                  professional_value = NULL,
                                  score = "score") {

  question_lookup <- get_question_labels() %>%
    dplyr::rename(question = question_label)

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

