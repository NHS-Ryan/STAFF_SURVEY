test_that("eligible raw values are not over-suppressed", {

  # Find project root robustly from tests/testthat or project root
  project_root <- normalizePath(
    if (dir.exists("data-raw")) "." else file.path(getwd(), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )

  raw_path <- file.path(project_root, "data-raw", "positive_scoring_rpg.csv")
  q_map_path <- file.path(project_root, "maps", "question_scores_map.csv")
  dims_map_path <- file.path(project_root, "maps", "dims_map.csv")
  teams_map_path <- file.path(project_root, "maps", "ox_teams_map.csv")

  expect_true(file.exists(raw_path), info = paste("Missing raw file:", raw_path))
  expect_true(file.exists(q_map_path), info = paste("Missing question map:", q_map_path))
  expect_true(file.exists(dims_map_path), info = paste("Missing dims map:", dims_map_path))
  expect_true(file.exists(teams_map_path), info = paste("Missing teams map:", teams_map_path))

  question_scores_map_test <- read_clean_csv(q_map_path)
  dims_map_test <- read_clean_csv(dims_map_path)
  ox_teams_map_test <- read_clean_csv(teams_map_path)

  raw_df <- read_clean_csv(raw_path) %>%
    dplyr::rename(
      year = Year,
      original_q_id = QuestionNumber,
      q_text = QuestionText,
      raw_dim = DimName,
      raw_dim_sub = DimValue,
      raw_n = BaseSize,
      raw_score = Score
    ) %>%
    dplyr::mutate(
      year = as.integer(year),
      raw_n = as.numeric(raw_n),
      raw_score = as.numeric(raw_score),
      raw_score = dplyr::if_else(raw_score > 1, raw_score / 100, raw_score),
      positive_n = raw_n * raw_score,
      negative_n = raw_n * (1 - raw_score),
      q_text_clean = trimws(gsub("[\r\n]", "", as.character(q_text)))
    ) %>%
    dplyr::left_join(
      question_scores_map_test %>%
        dplyr::transmute(
          q_text_clean = trimws(gsub("[\r\n]", "", as.character(q_text))),
          q_id = as.character(q_id)
        ) %>%
        dplyr::distinct(q_text_clean, .keep_all = TRUE),
      by = "q_text_clean"
    ) %>%
    dplyr::mutate(dim = stringr::str_trim(raw_dim)) %>%
    dplyr::left_join(
      dims_map_test %>%
        dplyr::mutate(include_dim = stringr::str_trim(include_dim)),
      by = c("dim" = "include_dim")
    ) %>%
    dplyr::filter(is.na(dim) | dim == "" | !is.na(rename_dim)) %>%
    dplyr::mutate(
      dim = dplyr::case_when(
        is.na(dim) | dim == "" ~ dim,
        TRUE ~ rename_dim
      ),
      dim_sub = raw_dim_sub
    ) %>%
    dplyr::left_join(
      ox_teams_map_test,
      by = c("dim_sub" = "team_full")
    ) %>%
    dplyr::mutate(
      dim_sub = dplyr::if_else(!is.na(team_short), team_short, dim_sub)
    ) %>%
    dplyr::select(
      year,
      original_q_id,
      q_id,
      q_text,
      raw_dim,
      raw_dim_sub,
      dim,
      dim_sub,
      raw_n,
      raw_score,
      positive_n,
      negative_n
    )

  output_df <- files$ox_q_aggregate_results %>%
    dplyr::transmute(
      year = as.integer(year),
      q_id = as.character(q_id),
      dim = as.character(dim),
      dim_sub = as.character(dim_sub),
      output_n = as.numeric(n),
      output_score = as.numeric(score),
      row_present = TRUE
    )

  eligible_raw <- raw_df %>%
    dplyr::filter(
      !is.na(q_id),
      !is.na(raw_n),
      !is.na(raw_score),
      positive_n > suppression_threshold,
      negative_n > suppression_threshold
    )

  checked <- eligible_raw %>%
    dplyr::left_join(
      output_df,
      by = c("year", "q_id", "dim", "dim_sub")
    ) %>%
    dplyr::mutate(
      issue = dplyr::case_when(
        is.na(row_present) ~ "No matching ETL output row",
        is.na(output_score) ~ "Matched row exists but output score is NA",
        TRUE ~ "OK"
      )
    )

  failures <- checked %>%
    dplyr::filter(issue != "OK") %>%
    dplyr::arrange(issue, year, q_id, dim, dim_sub)

  failure_summary <- failures %>%
    dplyr::count(issue, name = "n_failures")

  cat("\nSuppression diagnostic summary:\n")
  print(failure_summary)

  cat("\nExample likely over-suppressed rows: matched ETL row exists but score is NA\n")
  print(
    failures %>%
      dplyr::filter(issue == "Matched row exists but output score is NA") %>%
      dplyr::select(
        issue,
        year,
        original_q_id,
        q_id,
        q_text,
        raw_dim,
        raw_dim_sub,
        dim,
        dim_sub,
        raw_n,
        raw_score,
        positive_n,
        negative_n,
        output_n,
        output_score
      ) %>%
      dplyr::slice_head(n = 10),
    n = 10,
    width = Inf
  )

  cat("\nExample join failures: eligible raw rows not found in ETL output\n")
  print(
    failures %>%
      dplyr::filter(issue == "No matching ETL output row") %>%
      dplyr::select(
        issue,
        year,
        original_q_id,
        q_id,
        q_text,
        raw_dim,
        raw_dim_sub,
        dim,
        dim_sub,
        raw_n,
        raw_score,
        positive_n,
        negative_n
      ) %>%
      dplyr::slice_head(n = 10),
    n = 10,
    width = Inf
  )

  expect_equal(
    nrow(failures),
    0,
    info = paste(
      "Eligible rows appear to be missing or over-suppressed. Count:",
      nrow(failures),
      "| Breakdown:",
      paste(
        paste(failure_summary$issue, failure_summary$n_failures, sep = " = "),
        collapse = "; "
      )
    )
  )
})
