# This file will calculate theme scores for theme groupings created by Oxleas

prepare_theme_results_inputs <- function(files) {
  questions <- files$question_scores_map

  df_q <- files$ox_q_aggregate_results %>%
    left_join(questions, by = c("q_text" = "q_text")) %>%
    rename(q_id = q_id.y)

  themes <- files$theme_questions_map %>%
    filter(!theme %in% c("People's Promise","Other")) %>%
    mutate(subdomain = replace_na(subdomain, "No subdomain"))

  list(
    df_q = df_q,
    themes = themes,
    national_theme_results = files$ox_theme_results
  )

}

calculate_theme_results <- function(new_theme_results_inputs) {
  new_theme_results_inputs$df_q %>%
    left_join(new_theme_results_inputs$themes, by = c("q_id" = "q_id"), relationship = "many-to-many") %>%
    group_by(year, theme, domain, subdomain, dim, dim_sub) %>%
    summarise(score = mean(score, na.rm = TRUE), .groups = "drop")
}

calculate_themes <- function(files) {
  calculate_theme_results(prepare_theme_results_inputs(files))
}


