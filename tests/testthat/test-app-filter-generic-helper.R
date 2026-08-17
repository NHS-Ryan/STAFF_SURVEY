

test_that("Generic function for filters (filter_choice_values()) works as intended", {

  df <- data.frame(
    col1 = c(NA,"val2","val1","val1")
  )

  values <- filter_choice_values(df, "col1",include_all = TRUE)

  expect_all_false(is.na(values))                           # NA vals removed
  expect_identical(values,c("All","val1","val2"))           # Vals sorted
  expect_equal(sum(values == "val1"),1)                     # Duplicate vals removed
  expect_equal(sum(values =="All"),1)                       # include_all adds "All" option
  expect_equal(filter_choices_directorates()[[1]], "All")   # Test that "All" comes first
})


