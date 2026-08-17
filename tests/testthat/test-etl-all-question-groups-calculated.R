test_that("Question Groups themes have calculated values", {
  missing_scores <- nat_results %>%
    filter(
      theme_id %in% question_group_ids,
      is.na(score)
    )

  expect_equal(nrow(missing_scores), 0)
})
