test_that("filter_choice_values applies filters correctly", {
  df <- data.frame(
    dim = c("A", "A", "B"),
    val = c("x", "y", "z")
  )

  expect_identical(
    filter_choice_values(df, "val", filters = list(dim = "A")),
    c("x", "y")
  )
})
