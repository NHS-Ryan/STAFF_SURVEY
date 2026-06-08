# Function to check whether there any dims have been mapped incorrectly for this
# year or if any dims are being inappropriatey dropped. User will be prompted to
# review dropped dims and manually give feedback that these are okay to remove.
# If edits are required dims_map.csv should be edited.

map_dims <- function(files) {

  #---------------------------------
  # 1. Check rename_dim validity
  #---------------------------------

  dims_current <- files$ox_q_aggregate_results %>%
    pull(dim) %>%
    unique() %>%
    na.omit()

  dims_new <- files$dims_map %>%
    pull(rename_dim) %>%
    unique() %>%
    na.omit()

  dims_diff <- setdiff(dims_new, dims_current)

  if (length(dims_diff) > 0) {
    message("The following rename_dim values do not exist in ox_q_aggregate_results$dim:")
    message(paste0("- ", sort(dims_diff), collapse = "\n"))

    stop(
      "Process stopped: invalid rename_dim values detected. Edit dims_map.csv and rerun main.R.",
      call. = FALSE
    )
  }

  #---------------------------------
  # 2. Check dropped dimensions
  #---------------------------------

  raw_dims <- readr::read_csv(
    "data-raw/positive_scoring_rpg.csv",
    col_types = readr::cols(.default = readr::col_character())
  ) %>%
    dplyr::pull(DimName) %>%
    stringr::str_trim() %>%
    unique()

  mapped_dims <- files$dims_map %>%
    dplyr::pull(include_dim) %>%
    stringr::str_trim() %>%
    unique()

  dropped_dims <- setdiff(
    raw_dims[!is.na(raw_dims) & raw_dims != ""],
    mapped_dims[!is.na(mapped_dims) & mapped_dims != ""]
  )

  if (length(dropped_dims) > 0) {
    message(
      paste0(
        "The following dimensions are being dropped:\n- ",
        paste(sort(dropped_dims), collapse = "\n- ")
      )
    )

    message(
      "Process stopped: dropped dimensions detected (see above).\nIf dims dropped inappropriately review dims_map.csv and edit as appropriate. If okay continue to run main.R pipe after map_dims()"
    )
  } else {
    message("No dimensions dropped. Continuing.")
  }
}
