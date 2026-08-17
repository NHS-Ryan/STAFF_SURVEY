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

calculated_themes <- calculate_themes(files)

nat_results <- calculated_themes$nat_result_themes

trust_specific_map <- files$theme_questions_map %>%
  filter(domain == "Trust-specific questions")

question_group_ids <- files$theme_questions_map %>%
  filter(domain == "Question Groups") %>%
  distinct(theme_id) %>%
  pull(theme_id)
