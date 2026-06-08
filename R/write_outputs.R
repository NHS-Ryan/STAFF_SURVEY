# write final outputs to data/

write_outputs <- function(files) {
  write.csv(files$nat_result_themes,"data/nat_results_themes.csv")
  write.csv(files$nat_result_scores,"data/nat_results_scores.csv")
  write.csv(files$ox_q_aggregate_results,"data/ox_q_aggregate_results.csv")
  write.csv(files$ox_q_option_results,"data/ox_q_option_results.csv")
  write.csv(files$ox_theme_results,"data/ox_theme_results.csv")
}
