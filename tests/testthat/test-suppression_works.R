
df <- data.frame(read_csv("../../data/positive_scoring_suppressed.csv"))

test_that(
  "tests that supression_threshold has been applied to positive_scoring_suppressed.csv correctly",
  {
  expect_true(min(df$BaseSize * df$Score,na.rm = TRUE) >= suppression_threshold)
  expect_true(min(df$BaseSize * (1 - df$Score),na.rm = TRUE) >= suppression_threshold)
})


