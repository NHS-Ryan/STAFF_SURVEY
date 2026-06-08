# Function to check whether there are options for questions this year, and if so
# to prompt user to add them to maps/question_options_map.csv file with
# appropriate information.

map_options <- function(files) {

  options_current <-
    files$question_options_map %>%
      select("q_text",
             "option_text")

  options_new <-
    files$ox_q_option_results %>%
      select("q_text",
             "option_text") %>%
      distinct()

  option_diff <-
    options_new %>%
    anti_join(options_current, by = c("q_text", "option_text"))

  if(length(option_diff$q_text) > 0) {
    print(option_diff,n=24)
    stop("New options present: see above. Add to question_options_map.csv with appropriate details then rerun main.R")
  }

}


