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

source("shiny/config.R")
source("shiny/data_access.R")
source("shiny/helpers.R")
source("shiny/ui_main.R")
source("shiny/server_main.R")

ui <- build_ui()
server <- build_server()

shinyApp(ui, server)


