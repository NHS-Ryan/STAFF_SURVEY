# Load required packages
library(tidyverse)
library(testthat)
library(readxl)

# Load functions
source("R/config.R")
source("R/import_raw_data.R")
source("R/map_teams.R")
source("R/anonymity_suppression.R")
# source("R/calculate_themes.R")
# source("R/map_questions.R)
source("R/write_outputs.R")

# Run pipeline
vars <- config()
files <- import_raw_data()
map_teams(files)
# map_questions()
# calculate_themes()
files$ox_q_aggregate_results <- anonymity_suppression(
  files$ox_q_aggregate_results,
  vars
)
write_outputs(files)

# Run tests
testthat::test_dir("tests/testthat")

# remove(files,vars)
