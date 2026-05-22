

write_outputs <- function(files) {
  write.csv(files$nat_results,"data/national_results.csv")
  write.csv(files$ox_q_aggregate_results,"data/positive_scoring_suppressed.csv")
}


