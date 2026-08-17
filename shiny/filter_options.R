# -----------------------------
# Filter option helpers
# -----------------------------

# Generic function that takes:
#  - a df
#  - a col to return vals from
#  - a list of values to filter to
#  - adds 'All' to the returned set of values (used for "Select all" in filters)
#  - option to drop_blank which removes any filtered value of ""
#  - option to sort_values which sorts the output

filter_choice_values <- function(df,
                                 value_col,
                                 filters = list(),
                                 include_all = FALSE,
                                 drop_blank = TRUE,
                                 sort_values = TRUE) {
  out <- df

  for (filter_col in names(filters)) {
    filter_value <- filters[[filter_col]]

    if (!is.null(filter_value) && length(filter_value) > 0) {
      out <- out %>%
        dplyr::filter(.data[[filter_col]] %in% filter_value)
    }
  }

  values <- out %>%
    dplyr::pull(.data[[value_col]]) %>%
    stats::na.omit() %>%
    as.character() %>%
    unique()

  if (drop_blank) {
    values <- values[values != ""]
  }

  if (sort_values) {
    values <- sort(values)
  }

  if (include_all) {
    values <- c("All", values)
  }

  values
}


# -----------------------------
# Topic / theme hierarchy
# -----------------------------

filter_choices_topics <- function() {
  filter_choice_values(get_theme_questions_map(), "theme")
}

filter_choices_domains <- function(topic) {
  filter_choice_values(
    get_theme_questions_map(),
    "domain",
    filters = list(theme = topic)
  )
}

filter_choices_subdomains <- function(topic, domain) {
  values <- filter_choice_values(
    get_theme_questions_map(),
    "subdomain",
    filters = list(theme = topic, domain = domain)
  )

  if (length(values) == 0) {
    NULL
  } else {
    c("All", values)
  }
}


# -----------------------------
# Organisation
# -----------------------------

filter_choices_trusts <- function() {
  filter_choice_values(get_nat_result_themes(), "org_name")
}

filter_choices_directorates <- function() {
  filter_choice_values(
    get_ox_q_aggregate_results(),
    "dim_sub",
    filters = list(dim = "Directorate"),
    include_all = TRUE
  )
}

filter_choices_teams <- function(directorate) {
  if (is.null(directorate) || directorate == "All") {
    return("All")
  }

  filter_choice_values(
      get_ox_q_aggregate_results(),
      "dim_sub",
      filters = list(dim = "Team", directorate = directorate),
      include_all = TRUE
    )
  }


# -----------------------------
# Demographics / professions
# -----------------------------

filter_choices_dims <- function(filter_family) {
  filter_choice_values(
    get_dims_map(),
    "rename_dim",
    filters = list(filter_family = filter_family)
  )
}

filter_choices_protected_dims <- function() {
  filter_choices_dims("protected_characteristics")
}

filter_choices_professional_dims <- function() {
  filter_choices_dims("professional_groups")
}

filter_choices_dim_values <- function(dim_name) {
  filter_choice_values(
    get_ox_q_aggregate_results(),
    "dim_sub",
    filters = list(dim = dim_name)
  )
}
