import_raw_data <- function() {

# Script for importing raw data sets

#################################
# ~~~ NATIONAL STAFF SURVEY ~~~ #
#################################

# Source: national staff survey dataset: https://www.nhsstaffsurveys.com/results/local-results/
# and performs ETL from a .xlsx to a .csv in long format.

# Caveats: note that this script relies on the above file not changing structure
# If it does fail it's most likely because you need to adjust the 'sheets'
# variable.

###########
# EXTRACT #
###########

file <- "data-raw/NSS_BENCHMARK_REPORT.xlsx"
# Names of all sheets in file
sheets <- excel_sheets(file)
# Define which sheets to ignore: any sheets that have a col length not matching
# core worksheet length need to be removed.
sheets <- sheets[!sheets %in% c("Notes",
                                "Organisation Benchmark groups",
                                "ICBs",
                                "Community Surgical Services")]
nat_results <- data.frame()

# Iterate over sheets in file and bind to nat_results
# This is most likely part of script to fail so clear error messages provided
# via tryCatch().
nat_results <- tryCatch(
  map_dfr(sheets, function(x) {
    tryCatch(
      read_excel(file, sheet = x),
      error = function(e) {
        stop(paste("Read sheet fail:", x, "-", e$message))
      }
    )
  }),
  error = function(e) {
    stop(paste("Excel import fail:", e$message))
  }
)


#############
# TRANSFORM #
#############

# This takes nat_results and transforms it into long format with the following
# new fields:
# - valtype: type of value being held (val, n)
# - val: either number of responses (n) or score of the theme/question (val)
# - year: survey year
# - label: what the val or n relates to

nat_results <- nat_results %>%
  pivot_longer(cols = !starts_with("org"), # Make into long format
               names_to = "val_type",
               values_to = "val") %>%
  mutate(
    year = str_sub(val_type,-4),           # Extract year from val_type col
    val_type = str_sub(val_type,1,-6)      # Remaining part of val_type remains
  ) %>%
  filter(year != "_sig") %>%               # Some cols measure change in
                                           # significance. These are not needed.

  filter(                                  # Keeps only what is required:
    str_starts(val_type, "PP_") |          # People's Promise Results
    str_starts(val_type, "response_rate") |# Response rates
    str_starts(val_type, "M_") |           # Morale sub-themes
    str_starts(val_type, "E_") |           # Staff Engagement sub-themes
    (str_starts(val_type, "theme") & !str_detect(val_type, "q31a")) |
                                           # Morale & Staff Engagement themes
                                           # except themes broken down by q31
                                           # relating to MH / PH disabilities
    (str_starts(val_type, "q") & !str_detect(val_type, "16b_eth"))
                                           # Keep all questions apart from the
                                           # Q16b ethnicity details
  ) %>%

  mutate(                                  # if val_type ends in _n then create
                                           # create 'label' marked as 'n' if not
                                           # mark 'label' as 'val'
    label = if_else(
      str_ends(val_type, "_n"),
      str_remove(val_type, "_n$"),
      val_type
    ),
    val_type = if_else(str_ends(val_type, "_n"), "n", "val")
  )

###############################
# ~~~ OXLEAS STAFF SURVEY ~~~ #
###############################

# Source: Solaris dashboard files

ox_q_aggregate_results <- read_csv("data-raw/positive_scoring_oxleas.csv")
ox_q_option_results <- read_csv("data-raw/breakdown_report_oxleas.csv")
ox_theme_results <- read_csv("data-raw/people_promise_and_themes_oxleas.csv")

##########################
# ~~~ MAPPING TABLES ~~~ #
##########################

# Source: created locally by Oxleas

themes <- read_csv("maps/themes.csv")
ox_teams <- read_csv("maps/ox_teams.csv")
questions <- read_csv("maps/questions.csv")

##########################
# ~~~ RETURN OUTPUTS ~~~ #
##########################

# Returns files processed above to be processed further by onward functions
return(list(
  nat_results = nat_results,
  themes = themes,
  questions = questions,
  ox_teams = ox_teams,
  ox_q_aggregate_results = ox_q_aggregate_results,
  ox_q_option_results = ox_q_option_results,
  ox_theme_results = ox_theme_results
))
}

