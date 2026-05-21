
# Script for importing raw data sets


# NOTES
# Takes national staff survey dataset found here: https://www.nhsstaffsurveys.com/results/local-results/
# and performs ETL from a .xlsx to a

# Caveats: note that this script will fail if the shape of the file at the above link changes.
# Most likely you will need to adjust the 'EXTRACT' section of this script and change the
# sheets that are being excluded. The most important thing to check is that each sheet has the same
# number of columns.

# required libraries
library(tidyverse)
library(readxl)


###########
# EXTRACT #
###########

file <- "data-raw/NSS_BENCHMARK_REPORT.xlsx"
sheets <- excel_sheets(file)
sheets <- sheets[!sheets %in% c("Notes","Organisation Benchmark groups","ICBs","Community Surgical Services")]
nat_results <- data.frame()

for (x in sheets) {
  nat_results <- rbind(
    nat_results,
    read_excel("data-raw/NSS_BENCHMARK_REPORT.xlsx", sheet = x)
  )
}

#############
# TRANSFORM #
#############

nat_results <- nat_results %>%
  pivot_longer(cols = !starts_with("org"),
               names_to = "val_type",
               values_to = "val") %>%
  mutate(
    year = str_sub(val_type,-4),
    val_type = str_sub(val_type,1,-6)
  ) %>%
  filter(year != "_sig") %>%
  filter(
    str_starts(val_type, "PP_") |
    str_starts(val_type, "response_rate") |
    str_starts(val_type, "M_") |
    str_starts(val_type, "E_") |
    (str_starts(val_type, "theme") & !str_detect(val_type, "q31a")) |
    (str_starts(val_type, "q") & !str_detect(val_type, "16b_eth"))
  ) %>%
  mutate(
    label = if_else(
      str_ends(val_type, "_n"),
      str_remove(val_type, "_n$"),
      val_type
    ),
    val_type = if_else(str_ends(val_type, "_n"), "n", "val")
  )

########
# Load #
########

write.csv(nat_results,"data/national_results.csv")


