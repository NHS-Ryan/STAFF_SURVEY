
df <- files$ox_q_aggregate_results %>%
  filter(
    !is.na(n),
    !is.na(score)
  )

test_that(
  "tests that supression_threshold has been applied to positive_scoring_suppressed.csv correctly",
  {

    expect_gte(min(df$n * df$score, na.rm = TRUE), suppression_threshold)
    expect_gte(min(df$n * (1 - df$score), na.rm = TRUE), suppression_threshold)
})


