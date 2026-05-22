library(shiny)
library(shinydashboard)
library(DBI)
library(RPostgres)
library(plotly)
library(ggforce)
library(glue)
library(DT)
library(dplyr)

con <- dbConnect(
  Postgres(),
  host = "shiny-db.cvg2ouogqzvp.eu-north-1.rds.amazonaws.com",
  port = 5432,
  dbname = "postgres",
  user = "postgres",
  password = "gtrnnppe73877#BBT"
)

org_choices <- dbGetQuery(con, "
  SELECT organisation
  FROM nhs_orgs
  ORDER BY organisation
")$organisation

groups <- dbGetQuery(con, "
  SELECT DISTINCT
    \"group\"
  FROM nhs_ss_response_level_data
  WHERE \"group\" NOT IN ('Job Role', 'Team','Organisation')
")$group

directorates <- dbGetQuery(con, "
  SELECT DISTINCT
    subgroup
  FROM nhs_ss_response_level_data
  WHERE \"group\" = 'Directorate'
")$subgroup

value_to_angle <- function(val) {
  pi * (1 - val / 100) - pi / 2
}


