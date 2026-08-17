read_clean_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE) %>%
    dplyr::select(-matches("^\\.\\.\\.[0-9]+$")) %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        ~ .x %>%
          stringi::stri_enc_toutf8(is_unknown_8bit = TRUE) %>%
          stringr::str_trim()
      )
    )
}

load_csv_files <- function() {
  list(
    theme_questions_map    = read_clean_csv(file.path(PROJECT_ROOT, "maps", "theme_questions_map.csv")),
    ox_teams_map           = read_clean_csv(file.path(PROJECT_ROOT, "maps","ox_teams_map.csv")),
    nat_result_themes      = read_clean_csv(file.path(PROJECT_ROOT, "data", "nat_result_themes.csv")),
    nat_result_scores      = read_clean_csv(file.path(PROJECT_ROOT, "data", "nat_result_scores.csv")),
    ox_q_aggregate_results = read_clean_csv(file.path(PROJECT_ROOT, "data", "ox_q_aggregate_results.csv")),
    ox_theme_results       = read_clean_csv(file.path(PROJECT_ROOT, "data", "ox_theme_results.csv")),
    dims_map               = read_clean_csv(file.path(PROJECT_ROOT, "maps","dims_map.csv")),
    question_scores_map = read_clean_csv(file.path(PROJECT_ROOT, "maps","question_scores_map.csv"))
  )
}


load_postgres_files <- function(schema = Sys.getenv("STAFF_SURVEY_DB_SCHEMA", "test")) {

  secret_id <- Sys.getenv(
    "STAFF_SURVEY_DB_SECRET",
    unset = "staff-survey/prod/postgres/app"
  )

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

  read_pg_table <- function(table_name) {
    DBI::dbReadTable(
      con,
      DBI::Id(schema = schema, table = table_name)
    )
  }

  list(
    theme_questions_map    = read_pg_table("theme_questions_map"),
    themes_map             = read_pg_table("themes_map"),
    ox_teams_map           = read_pg_table("ox_teams_map"),
    nat_result_themes      = read_pg_table("nat_result_themes"),
    nat_result_scores      = read_pg_table("nat_result_scores"),
    ox_q_aggregate_results = read_pg_table("ox_q_aggregate_results"),
    ox_q_option_results    = read_pg_table("ox_q_option_results"),
    ox_theme_results       = read_pg_table("ox_theme_results"),
    dims_map               = read_pg_table("dims_map"),
    question_scores_map    = read_pg_table("question_scores_map"),
    question_options_map   = read_pg_table("question_options_map")
  )
}

data_backend <- Sys.getenv("STAFF_SURVEY_DATA_BACKEND", "csv")

files <- if (identical(data_backend, "postgres")) {
  load_postgres_files()
} else {
  load_csv_files()
}

