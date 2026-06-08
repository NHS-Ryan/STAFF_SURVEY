# Function to check whether there are new questions this year, and if so to
# prompt user to add them to the ox_teams.csv file with appropriate information.

map_themes <- function(files) {

  themes_current <- c(files$themes_map$theme_text)

  themes_new <- bind_rows(

    files$nat_result_themes %>%
      select(theme_text) %>%
      distinct(),

    files$ox_theme_results %>%
      select(theme_text) %>%
      distinct(),

  ) %>%
    distinct()

  themes_diff <- setdiff(pull(themes_new),themes_current)

  if(length(themes_diff) > 0) {
    print(themes_diff)
    stop("New themes present: see above. Add new themes to question_themes_map.csv. Map to prior themes where appropriate. Then rerun main.R")
  }

}
