# Pulls necessary data from EC2 location into PostgreSQL

required_pkgs <- c(
  "DBI", "RPostgres", "readr", "dplyr",
  "stringi", "stringr", "jsonlite"
)

missing_pkgs <- required_pkgs[
  !vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_pkgs) > 0) {
  stop(
    "Missing R packages: ",
    paste(missing_pkgs, collapse = ", "),
    "\nInstall them before running this script."
  )
}

args <- commandArgs(trailingOnly = TRUE)

target_schema <- if (length(args) >= 1) args[[1]] else "test"

source_root <- if (length(args) >= 2) {
  args[[2]]
} else {
  entered <- readline(
    paste0(
      "Enter the folder containing data/ and maps/\n",
      "Example: /opt/staff-survey/import_source\n> "
    )
  )

  if (entered == "") {
    stop("No source folder supplied.")
  } else {
    entered
  }
}

source_root <- normalizePath(source_root, winslash = "/", mustWork = TRUE)

data_dir <- file.path(source_root, "data")
maps_dir <- file.path(source_root, "maps")

message("Target schema: ", target_schema)
message("Source root: ", source_root)
message("Data folder: ", data_dir)
message("Maps folder: ", maps_dir)

csv_files <- c(
  if (dir.exists(data_dir)) {
    list.files(data_dir, pattern = "\\.(csv|txt)$", full.names = TRUE)
  } else {
    character(0)
  },
  if (dir.exists(maps_dir)) {
    list.files(maps_dir, pattern = "\\.(csv|txt)$", full.names = TRUE)
  } else {
    character(0)
  }
)

csv_files <- sort(csv_files)

if (length(csv_files) == 0) {
  stop("No CSV files found in data/ or maps/ under: ", source_root)
}

table_name_from_file <- function(path) {
  tools::file_path_sans_ext(basename(path))
}

table_names <- vapply(csv_files, table_name_from_file, character(1))

if (anyDuplicated(table_names)) {
  stop(
    "Duplicate table names found after removing .csv extension: ",
    paste(unique(table_names[duplicated(table_names)]), collapse = ", ")
  )
}

secret_id <- Sys.getenv(
  "STAFF_SURVEY_DB_SECRET",
  unset = "staff-survey/prod/postgres/app"
)

message("Secret ID: ", secret_id)

secret_string <- system2(
  "aws",
  args = c(
    "secretsmanager", "get-secret-value",
    "--secret-id", secret_id,
    "--query", "SecretString",
    "--output", "text"
  ),
  stdout = TRUE
)

secret <- jsonlite::fromJSON(paste(secret_string, collapse = "\n"))

con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host = secret$host,
  port = as.integer(secret$port),
  dbname = secret$dbname,
  user = secret$username,
  password = secret$password
)

on.exit(DBI::dbDisconnect(con), add = TRUE)

read_clean_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::select(-dplyr::matches("^\\.\\.\\.[0-9]+$")) |>
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~ .x |>
          stringi::stri_enc_toutf8(is_unknown_8bit = TRUE) |>
          stringr::str_trim()
      )
    )
}

DBI::dbExecute(
  con,
  paste(
    "CREATE SCHEMA IF NOT EXISTS",
    DBI::dbQuoteIdentifier(con, target_schema)
  )
)

import_log <- lapply(seq_along(csv_files), function(i) {
  path <- csv_files[[i]]
  table_name <- table_names[[i]]

  message("Importing ", path, " -> ", target_schema, ".", table_name)

  df <- read_clean_csv(path)

  DBI::dbWriteTable(
    con,
    name = DBI::Id(schema = target_schema, table = table_name),
    value = df,
    overwrite = TRUE,
    row.names = FALSE
  )

  data.frame(
    table_schema = target_schema,
    table_name = table_name,
    source_file = path,
    rows = nrow(df),
    columns = ncol(df),
    stringsAsFactors = FALSE
  )
})

import_log <- dplyr::bind_rows(import_log)

print(import_log)

message("Import complete.")
