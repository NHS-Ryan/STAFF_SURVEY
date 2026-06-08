# Function to check whether there are new teams this year, and if to prompt user
# to add them to the ox_teams.csv file with appropriate information.

map_teams <- function(files) {

  teams_current <- c(files$ox_teams$team_full)

  teams_new <- bind_rows(

  files$ox_q_aggregate_results %>%
    filter(dim_sub == "BD3") %>%
    select(dim) %>%
    distinct(),

  files$ox_q_option_results %>%
    filter(dim_sub == "BD3") %>%
    select(dim) %>%
    distinct(),

  files$ox_theme_results %>%
    filter(dim_sub == "BD3") %>%
    select(dim) %>%
    distinct()

  ) %>%
    distinct()

  team_diff <- setdiff(pull(teams_new),teams_current)

  if(length(team_diff) > 0) {
    print(team_diff)
    stop("New teams present: see above. Add to ox_teams.csv with appropriate details then rerun main.R")
  }

}


