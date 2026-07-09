# shiny::addResourcePath("www", "shiny/www")

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

source("config.R")
source("data_access.R")
source("helpers.R")
source("ui_main.R")
source("server_main.R")

ui <- build_ui()
server <- build_server()

shinyApp(ui, server)


