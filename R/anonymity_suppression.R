# Suppresses individual questions in ox_q_aggregate_results where the number of
# respondents falling into either the positive / negative respondent group is
# below suppression_threshold (defined in config.R)

anonymity_suppression <- function(files,vars) {
  files$ox_q_aggregate_results <- files$ox_q_aggregate_results %>%
    mutate(
      pos = n * score,
      neg = n * (1 - score),
      score = if_else(pos <= vars$suppression_threshold | neg <= vars$suppression_threshold, NA_real_,score)
    ) %>%
    select(-c("pos","neg"))

  files$ox_q_option_results <- files$ox_q_option_results %>%
    filter(dim_sub %in% c("Comparator","Organisation") | dim == "Directorate")

  return(files)
}

