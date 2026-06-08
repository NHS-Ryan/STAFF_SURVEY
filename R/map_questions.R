# Function to check whether there are new questions this year, and if so to
# prompt user to add them to the ox_teams.csv file with appropriate information.

map_questions <- function(files) {

  questions_current <- c(files$question_scores_map$q_text)

  questions_new <- bind_rows(

    files$nat_result_scores %>%
      select(q_text) %>%
      distinct(),

    files$ox_q_aggregate_results %>%
      select(q_text) %>%
      distinct(),

    files$ox_q_option_results %>%
      select(q_text) %>%
      distinct()

  ) %>%
    distinct()

  questions_diff <- setdiff(pull(questions_new),questions_current)

  if(length(questions_diff) > 0) {
    print(questions_diff)
    stop("New questions present: see above. Add new questions to questions.csv. Map to prior questions where appropriate. Then rerun main.R")
  }

}


