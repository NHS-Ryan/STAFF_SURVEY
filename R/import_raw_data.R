import_raw_data <- function() {

# Script for importing all necessary data sets. Also performs transformation on
# national staff survey results into a long format .csv.

##########################
# ~~~ MAPPING TABLES ~~~ #
##########################

# Source: created locally by Oxleas
ox_teams_map <- read_csv("maps/ox_teams_map.csv")
theme_questions_map <- read_csv("maps/theme_questions_map.csv")
question_scores_map <- read_csv("maps/question_scores_map.csv")
themes_map <- read_csv("maps/themes_map.csv")
dims_map <- read_csv(("maps/dims_map.csv"))

# Note that this file is saved as .txt to discourage opening in Excel in a
# standard manner: some options are labelled as e.g. '1-2' which the standard
# excel parser interprets as a date. File should only be opened in Excel using
# the Data -> Import .csv / .txt option and setting the option_text column to be
# identified as text.
question_options_map <- read_csv(
  "maps/question_options_map.txt",
  col_types = cols(
    q_text = col_character(),
    option_text = col_character()
  )
)

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

# Also extract the notes sheet as a mapping table
nat_results_notes <-
  read_excel(file,"Notes") %>%
  rename_with(~c("id","id_description"),.cols=1:2) %>%
  filter(!is.na(id))%>%
  filter(!is.na(id_description))

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
  # joins to nat_results_notes data in order to make q_text available
  mutate(join_key = sub(".{5}$", "", sub("_n", "", val_type))) %>%
  left_join(
    nat_results_notes %>%
      transmute(
        join_key = sub(".{5}$", "", id),
        id_description
      ),
    by = "join_key",
    relationship = "many-to-many"
  ) %>%
  select(-join_key) %>%
  mutate(
    year = str_sub(val_type,-4),           # Extract year from val_type col
    val_type = str_sub(val_type,1,-6)      # Remaining part of val_type remains
  ) %>%
  filter(year != "_sig") %>%               # Some cols measure change in
                                           # significance. These are not needed.

  filter(                                  # Keeps only what is required:
    str_starts(val_type, "PP") |          # People's Promise Results
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
  ) %>%

  mutate (                                 # Rename values in line with theme_id
    label = case_when(
      label == 'theme_engagement' ~ 'E',
      label == 'theme_morale' ~ 'M',
      TRUE ~ label
    )
  ) %>%
  pivot_wider(
    names_from = val_type,
    values_from = val
  ) %>%
  rename(score = val, id_text = id_description) %>%
  mutate(id_text = trimws(gsub("[\r\n]", "", id_text))) %>%
  left_join(
    question_scores_map %>%
      transmute(
        q_text = trimws(gsub("[\r\n]", "", as.character(q_text))),
        q_id,
        q_type,
        trust_specific,
        down_good
      ),
    by = c("id_text" = "q_text")
  )

# Split nat_results into themes & scores dfs
nat_result_themes <- nat_results %>%
  filter(is.na(q_id)) %>%
  rename(theme_id = label, theme_text = id_text) %>%
  select(-c("q_id","q_type","trust_specific","down_good"))

nat_result_scores <- nat_results %>%
  filter(!is.na(q_id)) %>%
  rename(q_text = id_text) %>%
  mutate(
    score = if_else(down_good == 1, 1 - score, score, missing = score)
  ) %>%
  select(-down_good)

###############################
# ~~~ OXLEAS STAFF SURVEY ~~~ #
###############################

# Source: Solaris dashboard files
# Also uses dims_map dataframe which is produced by Oxleas.
# columns renamed for consistency across datasets & joined to dims_map so that
# dim names are clearer and dims that are not needed can be dropped.

ox_q_aggregate_results <- read_csv("data-raw/positive_scoring_rpg.csv") %>%
  rename(
    year = Year,
    original_q_id = QuestionNumber,
    q_text = QuestionText,
    dim = DimName,
    dim_sub = DimValue,
    n = BaseSize,
    score = Score
  ) %>%
  # Join to question_scores_map
  mutate(
    q_text_clean = trimws(gsub("[\r\n]", "", as.character(q_text)))
  ) %>%
  left_join(
    question_scores_map %>%
      transmute(
        q_text_clean = trimws(gsub("[\r\n]", "", as.character(q_text))),
        q_id
      ) %>%
      distinct(q_text_clean, .keep_all = TRUE),
    by = "q_text_clean"
  ) %>%
  select(-original_q_id, -q_text_clean) %>%
  # Join to dims_map
  mutate(dim = str_trim(dim)) %>%
  left_join(
    dims_map %>%
      mutate(include_dim = str_trim(include_dim)),
    by = c("dim" = "include_dim")
  ) %>%
  filter(is.na(dim) | dim == "" | !is.na(rename_dim)) %>%
  mutate(
    dim = case_when(
      is.na(dim) | dim == "" ~ dim,
      TRUE ~ rename_dim
    )
  ) %>%
  select(-rename_dim) %>%
  # Replace names with team_short from ox_teams_map
  left_join(
    ox_teams_map,
    by = c("dim_sub" = "team_full")
  ) %>%
  mutate(
    dim_sub = if_else(!is.na(team_short), team_short, dim_sub)
  ) %>%
  select(-c("team_short","filter_family"))


ox_q_option_results <- read_csv("data-raw/breakdown_report_rpg.csv") %>%
  rename(
    year = Year,
    original_q_id = QuestionNumber,
    q_text = QuestionText,
    option_id = OptionCode,
    option_text = OptionText,
    dim = DimName,
    dim_sub = DimValue,
    score = '%'
  ) %>%
  mutate(
    q_text_clean = trimws(gsub("[\r\n]", "", as.character(q_text)))
  ) %>%
  # Add in correct q_id from question_scores_map
  left_join(
    question_scores_map %>%
      transmute(
        q_text_clean = trimws(gsub("[\r\n]", "", as.character(q_text))),
        q_id
      ) %>%
      distinct(q_text_clean, .keep_all = TRUE),
    by = "q_text_clean"
  ) %>%
  select(-original_q_id, -q_text_clean) %>%
  # Join to dims_map
  mutate(dim = str_trim(dim)) %>%
  left_join(
    dims_map %>%
      mutate(include_dim = str_trim(include_dim)),
    by = c("dim" = "include_dim")
  ) %>%
  filter(is.na(dim) | dim == "" | !is.na(rename_dim)) %>%
  mutate(
    dim = case_when(
      is.na(dim) | dim == "" ~ dim,
      TRUE ~ rename_dim
    )
  ) %>%
  select(-c("rename_dim","filter_family"))

ox_theme_results <- read_csv("data-raw/people_promise_and_themes_rpg.csv") %>%
  rename(
    year = Year,
    original_theme_id = QuestionNumber,
    theme_text = QuestionText,
    dim = DimName,
    dim_sub = DimValue,
    n = BaseSize,
    score = Score
  ) %>%
  # Add correct theme_id from themes_map
  mutate(
    theme_text_clean = trimws(gsub("[\r\n]", "", as.character(theme_text)))
  ) %>%
  left_join(
    themes_map %>%
      transmute(
        theme_text_clean = trimws(gsub("[\r\n]", "", as.character(theme_text))),
        theme_id
      ) %>%
      distinct(theme_text_clean, .keep_all = TRUE),
    by = "theme_text_clean"
  ) %>%
  select(-original_theme_id, -theme_text_clean) %>%
  # Join to dims_map
  mutate(dim = str_trim(dim)) %>%
  left_join(
    dims_map %>%
      mutate(include_dim = str_trim(include_dim)),
    by = c("dim" = "include_dim")
  ) %>%
  filter(is.na(dim) | dim == "" | !is.na(rename_dim)) %>%
  mutate(
    dim = case_when(
      is.na(dim) | dim == "" ~ dim,
      TRUE ~ rename_dim
    )
  ) %>%
  select(-rename_dim) %>%
  # Replace names with team_short from ox_teams_map
  left_join(
    ox_teams_map,
    by = c("dim_sub" = "team_full")
  ) %>%
  mutate(
    dim_sub = if_else(!is.na(team_short), team_short, dim_sub)
  ) %>%
  select(-c("team_short","filter_family","directorate","benchmark_group","exclude"))

##########################
# ~~~ RETURN OUTPUTS ~~~ #
##########################

# Returns files processed above to be processed further by onward functions
return(list(
  nat_result_themes = nat_result_themes,
  nat_result_scores = nat_result_scores,
  nat_results_notes = nat_results_notes,
  themes_map = themes_map,
  theme_questions_map = theme_questions_map,
  question_scores_map = question_scores_map,
  question_options_map = question_options_map,
  dims_map = dims_map,
  ox_teams_map = ox_teams_map,
  ox_q_aggregate_results = ox_q_aggregate_results,
  ox_q_option_results = ox_q_option_results,
  ox_theme_results = ox_theme_results
))
}
