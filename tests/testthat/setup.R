library(tidyverse)
library(readxl)
library(here)

setwd(here::here())

r_files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]$")
r_files <- r_files[basename(r_files) != "main.R"]

lapply(r_files, source)

vars <- config()
files <- import_raw_data()
files <- anonymity_suppression(files,vars)
suppression_threshold <- vars$suppression_threshold
theme_inputs <- prepare_theme_results_inputs(files)
calculated_themes <- calculate_themes(files)
