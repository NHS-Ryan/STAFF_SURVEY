# write final outputs to data/

sanitize_utf8_df <- function(df) {
  df %>%
    mutate(
      across(
        where(is.character),
        ~ iconv(.x, from = "", to = "UTF-8", sub = "")
      )
    )
}

write_outputs <- function(files) {
  write.csv(
    sanitize_utf8_df(files$nat_result_themes),
    "data/nat_result_themes.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  write.csv(
    sanitize_utf8_df(files$nat_result_scores),
    "data/nat_result_scores.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  write.csv(
    sanitize_utf8_df(files$ox_q_aggregate_results),
    "data/ox_q_aggregate_results.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  write.csv(
    sanitize_utf8_df(files$ox_q_option_results),
    "data/ox_q_option_results.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  write.csv(
    sanitize_utf8_df(files$ox_theme_results),
    "data/ox_theme_results.csv",
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
}

