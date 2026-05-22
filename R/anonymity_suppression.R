
# Suppresses individual questions in ox_q_aggregate_results where the number of
# respondents falling into either the positive / negative respondent group is
# below suppression_threshold (defined in config.R)

anonymity_suppression <- function(df,vars) {
  df %>%
    mutate(
      pos = BaseSize * Score,
      neg = BaseSize * (1 - Score),
      Score = if_else(pos <= vars$suppression_threshold | neg <= vars$suppression_threshold, NA_real_,Score)
    ) %>%
    select(-c("pos","neg")) %>%
    return()
}
