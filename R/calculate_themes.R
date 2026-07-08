# This file will calculate theme scores for theme groupings created by Oxleas

prepare_theme_results_inputs <- function(files) {
  questions <- files$question_scores_map

  df_q <- files$ox_q_aggregate_results %>%
    left_join(questions, by = c("q_text" = "q_text")) %>%
    rename(q_id = q_id.y)

  themes <- files$theme_questions_map %>%
    mutate(subdomain = replace_na(subdomain, "No subdomain"))

  list(
    df_q = df_q,
    themes = themes,
    national_theme_results = files$ox_theme_results
  )

}

prepare_theme_results_inputs_nat <- function(files) {
  questions <- files$question_scores_map

  df_q <- files$nat_result_scores %>%
    left_join(questions, by = c("q_text" = "q_text")) %>%
    rename(q_id = q_id.y)

  themes <- files$theme_questions_map %>%
    filter(!theme %in% c("People's Promise", "Other")) %>%
    mutate(subdomain = replace_na(subdomain, "No subdomain"))

  list(df_q = df_q, themes = themes)
}

calculate_theme_results_nat <- function(files) {
  x <- prepare_theme_results_inputs_nat(files)

  x$df_q %>%
    left_join(x$themes, by = "q_id", relationship = "many-to-many") %>%
    filter(!is.na(theme_id)) %>%
    group_by(year, org_id, org_name, org_type, theme_id) %>%
    summarise(
      score = if (
        dplyr::n_distinct(q_id[!is.na(score)]) ==
        dplyr::n_distinct(.env$x$themes$q_id[.env$x$themes$theme_id == dplyr::first(theme_id)])
      ) mean(score, na.rm = TRUE) else NA_real_,
      .groups = "drop"
    )
}

calculate_theme_results_ox <- function(new_theme_results_inputs) {

  expected_questions <- new_theme_results_inputs$themes %>%
    dplyr::count(theme_id, name = "expected_n")

  new_theme_results_inputs$df_q %>%
    left_join(new_theme_results_inputs$themes, by = "q_id", relationship = "many-to-many") %>%
    filter(!is.na(theme_id)) %>%
    left_join(expected_questions, by = "theme_id") %>%
    group_by(year, theme_id, theme, domain, subdomain, dim, dim_sub) %>%
    summarise(
      score = if (
        dplyr::n_distinct(q_id[!is.na(score)]) == dplyr::first(expected_n)
      ) {
        mean(score, na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    select(year, theme, domain, subdomain, dim, dim_sub, score)
}

calculate_themes <- function(files) {
  files$ox_theme_results <- calculate_theme_results_ox(
    prepare_theme_results_inputs(files)
  )

  files$nat_result_themes <- bind_rows(
    files$nat_result_themes,
    calculate_theme_results_nat(files)
  )

  files
}

