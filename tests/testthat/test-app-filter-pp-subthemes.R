test_that("People's Promise sub-theme selection expected behaviour", {

  values <- filter_choices_subdomains("People's Promise", "We are a team")

  expect_type(values, "character")
  expect_gt(length(values), 1)
  expect_equal(values[[1]], "All")
  expect_true("Line management" %in% values)
  expect_false(any(is.na(values)))
  expect_false(any(values == ""))

})



