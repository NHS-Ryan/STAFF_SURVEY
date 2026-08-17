test_that("Trust-specific themes are calculated for Oxleas", {
  expected <- trust_specific_map %>%
    distinct(org_id, theme_id)

  actual <- nat_results %>%
    filter(!is.na(score)) %>%
    distinct(org_id, theme_id)

  missing <- expected %>%
    anti_join(actual, by = c("org_id", "theme_id"))

  expect_equal(nrow(missing), 0)
})
