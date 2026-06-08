# Load required packages
library(tidyverse)
library(testthat)
library(readxl)
library(here)

# Load functions
source("R/config.R")
source("R/import_raw_data.R")
source("R/map_dims.R")
source("R/map_options.R")
source("R/map_questions.R")
source("R/map_teams.R")
source("R/map_themes.R")
source("R/anonymity_suppression.R")
source("R/calculate_themes.R")
source("R/write_outputs.R")

# Load environment variables
vars <- config()

# Import data
files <- import_raw_data()

# Check if there are new options, questions, teams or themes this year
# Each change will need to be resolved by editing files in maps/
# Note that map_dims() will automatically stop execution and give a message that
# certain dims are being dropped. These should be reviewed before continuing.
# Only those dims needed in the dashboard should be retained.
map_dims(files)
map_options(files)
map_questions(files)
map_teams(files)
map_themes(files)

# Calculate locally agreed theme results
files$ox_theme_results <- calculate_themes(files)

# Enforce anonymity suppression rules
files <-  anonymity_suppression(files, vars)

# Run tests
testthat::test_dir("tests/testthat")

# Save files
write_outputs(files)
