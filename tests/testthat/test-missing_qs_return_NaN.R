# This test not working currently needs fixing


# test_that("Theme scores return NaN when not all questions are present for a particular DimValue", {
#
#   expected_counts <- theme_inputs$themes %>%
#     group_by(theme, domain, subdomain) %>%
#     summarise(expected_n = n(), .groups = "drop")
#
#   actual_counts <- theme_inputs$df_q %>%
#     left_join(theme_inputs$themes, by = "q_id") %>%
#     group_by(Year, DimName, DimValue, theme, domain, subdomain) %>%
#     summarise(actual_n = n(), .groups = "drop")
#
#   check_df <- calculated_themes %>%
#     left_join(expected_counts, by = c("theme","domain","subdomain")) %>%
#     left_join(actual_counts, by = c("Year","DimName","DimValue","theme","domain","subdomain"))
#
#   expect_true(
#     all(
#       is.nan(check_df$score[check_df$actual_n < check_df$expected_n])
#     )
#   )
#
# })
