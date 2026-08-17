
# Tests I need to develop:
# suppression too loose
#
# get_selected_q_ids() and get_question_labels()
#
# build_comparison_df() return shape and exactly one selected row
#
# selected comparison score ≈ question table mean
#
# benchmark bar selected row matches comparison selected row
#
# missing reason values are valid
#
# line/gauge return shapes
#
# Testing of visualisations: in particular feedback to user e.g.
# it says "similar trusts" when that combination is selected etc.


# Required libraries for source files
library(testthat)
library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(DBI)
library(RPostgres)
library(ggplot2)
library(ggforce)
library(tibble)
library(grid)
library(plotly)
library(DT)
library(scales)
library(markdown)
library(jsonlite)

# Source files that are being tested
source(file.path("shiny/app.R"), local = TRUE)
source(file.path("shiny/build_data.R"), local = TRUE)
source(file.path("shiny/config.R"), local = TRUE)
source(file.path("shiny/data_access.R"), local = TRUE)
source(file.path("shiny/data_load.R"), local = TRUE)
source(file.path("shiny/filter_logic.R"), local = TRUE)
source(file.path("shiny/filter_options.R"), local = TRUE)
source(file.path("shiny/helpers.R"), local = TRUE)
source(file.path("shiny/server_main.R"), local = TRUE)
source(file.path("shiny/ui_main.R"), local = TRUE)


# Run any test in folder starting with "test-app"
testthat::test_dir("tests/testthat", filter = "app")
