config <- function(){

  # size of staff groups to exclude from individual question results
  suppression_threshold <- 3

  # List of years that we already have national staff survey dataset results
  # from
  nat_results_files <- list.files("data/national_results/")
  complete_years <-
    substr(nat_results_files,
         nchar(nat_results_files)-7,
         nchar(nat_results_files)-4)

  return(
    list(
      suppression_threshold = suppression_threshold,
      complete_years = complete_years
    )
  )
}
