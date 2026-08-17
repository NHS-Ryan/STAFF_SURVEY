# -----------------------------
# Mount required packages
# -----------------------------

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

# -----------------------------
# Resolve app and project paths
# -----------------------------
# This just ensures that when launching in AWS environment or in local R
# Markdown that a consistent project root folder is identified.

APP_DIR <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)

# If app.R is sourced from repo root rather than run via runApp("shiny"),
# normalise APP_DIR to the shiny folder.
if (basename(APP_DIR) != "shiny" && dir.exists(file.path(APP_DIR, "shiny"))) {
  APP_DIR <- normalizePath(file.path(APP_DIR, "shiny"), winslash = "/", mustWork = TRUE)
}

PROJECT_ROOT <- normalizePath(file.path(APP_DIR, ".."), winslash = "/", mustWork = TRUE)

# Helper function to identify where app lives
app_file <- function(...) {
  file.path(APP_DIR, ...)
}

# -----------------------------
# Source app files
# -----------------------------

source(app_file("build_data.R"), local = TRUE)
source(app_file("config.R"), local = TRUE)
source(app_file("data_access.R"), local = TRUE)
source(app_file("data_load.R"), local = TRUE)
source(app_file("filter_logic.R"), local = TRUE)
source(app_file("filter_options.R"), local = TRUE)
source(app_file("helpers.R"), local = TRUE)
source(app_file("server_main.R"), local = TRUE)
source(app_file("ui_main.R"), local = TRUE)


# -----------------------------
# Launch App
# -----------------------------

ui <- build_ui()
server <- build_server()

shiny::shinyApp(ui = ui, server = server)
