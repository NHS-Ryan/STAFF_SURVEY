library(shiny)
library(dplyr)
library(readr)
library(stringr)


# To do list:
#
#
#
#
#

# -----------------------------
# Helper function for reading text safely
# -----------------------------
read_clean_csv <- function(path) {
  read_csv(path, show_col_types = FALSE) %>%
    select(-matches("^\\.\\.\\.[0-9]+$")) %>%
    mutate(
      across(
        where(is.character),
        ~ .x %>%
          stringi::stri_enc_toutf8(is_unknown_8bit = TRUE) %>%
          str_trim()
      )
    )
}

# -----------------------------
# Data
# -----------------------------
files <- list(
  ox_q_aggregate_results = read_clean_csv("data/ox_q_aggregate_results.csv"),
  nat_results_themes = read_clean_csv("data/nat_results_themes.csv"),
  ox_theme_results = read_clean_csv("data/ox_theme_results.csv"),
  theme_questions_map = read_clean_csv("maps/theme_questions_map.csv")
)



# -----------------------------
# Helper
# -----------------------------
all_and_sort <- function(x) {
  cleaned_values <- x %>%
    na.omit() %>%
    unique() %>%
    sort()

  c("All", cleaned_values)
}

protected_characteristic_dims <- c(
  "Age",
  "Ethnicities Grouped",
  "Ethnicities Detailed",
  "Gender",
  "Gender Identity same as Birth Sex",
  "Long Term Condition",
  "Religion",
  "Sexuality",
  "Socio-economic Class (NS-SEC)"
)

professional_group_dims <- c(
  "Occupations Grouped",
  "Occupations Detailed"
)

# -----------------------------
# UI
# -----------------------------
ui <- fluidPage(
  fluidRow(
    column(
      width = 12,
      h2("NHS Staff Survey"),
      div(
        style = "margin-top: -10px; margin-bottom: 15px; color: #666; font-size: 15px;",
        textOutput("active_filters_text", inline = TRUE)
      )
    )
  ),

  sidebarLayout(
    sidebarPanel(

      selectInput(
        inputId = "filter_family",
        label = "What group of staff's responses do you want to look at?",
        choices = c(
          "Organisational Structure",
          "Protected Characteristics",
          "Professional Groups"
        ),
        selected = "Organisational Structure"
      ),

      uiOutput("dynamic_filters"),

      tags$hr(),

      h4("What set of themes do you want to group questions by?"),
      uiOutput("theme_grouping_filters")
    ),

    mainPanel(
      div(
        style = "padding: 20px; border: 1px solid #ddd; min-height: 400px;",
        h4("Visualisations"),
        verbatimTextOutput("filter_state")
      )
    )
  )
)


# -----------------------------
# Server
# -----------------------------

server <- function(input, output, session) {

  # -----------------------------
  # Theme / Domain / Sub-domain
  # -----------------------------
  theme_choices <- reactive({
    files$theme_questions_map %>%
      pull(theme) %>%
      na.omit() %>%
      unique() %>%
      sort()
  })

  get_domain_choices <- function(selected_theme) {
    files$theme_questions_map %>%
      filter(theme == selected_theme) %>%
      pull(domain) %>%
      na.omit() %>%
      unique() %>%
      sort()
  }

  get_subdomain_choices <- function(selected_theme, selected_domain) {
    subdomain_values <- files$theme_questions_map %>%
      filter(
        theme == selected_theme,
        domain == selected_domain
      ) %>%
      pull(subdomain) %>%
      na.omit() %>%
      unique()

    subdomain_values <- subdomain_values[subdomain_values != ""] %>%
      sort()

    if (length(subdomain_values) == 0) {
      return(NULL)
    }

    c("All", subdomain_values)
  }

  # -----------------------------
  # Organisational Structure
  # -----------------------------
  trust_choices <- reactive({
    files$nat_results_themes %>%
      pull(org_name) %>%
      na.omit() %>%
      unique() %>%
      sort()
  })

  directorate_choices <- reactive({
    files$ox_q_aggregate_results %>%
      filter(dim == "Directorate") %>%
      pull(dim_sub) %>%
      all_and_sort()
  })

  team_choices <- reactive({
    req(input$directorate)

    if (input$directorate == "All") {
      return("All")
    }

    files$ox_q_aggregate_results %>%
      filter(
        dim == "Team",
        directorate == input$directorate
      ) %>%
      pull(dim_sub) %>%
      all_and_sort()
  })

  # -----------------------------
  # Protected Characteristics
  # -----------------------------
  protected_value_choices <- reactive({
    req(input$protected_dim)

    files$ox_q_aggregate_results %>%
      filter(dim == input$protected_dim) %>%
      pull(dim_sub) %>%
      all_and_sort()
  })

  # -----------------------------
  # Professional Groups
  # -----------------------------
  professional_value_choices <- reactive({
    req(input$professional_dim)

    files$ox_q_aggregate_results %>%
      filter(dim == input$professional_dim) %>%
      pull(dim_sub) %>%
      all_and_sort()
  })

  # Dynamic UI --------------------------------------------------------------

  output$theme_grouping_filters <- renderUI({

    available_themes <- theme_choices()

    if (length(available_themes) == 0) {
      return(NULL)
    }

    current_theme <- if (!is.null(input$selected_theme) &&
                         input$selected_theme %in% available_themes) {
      input$selected_theme
    } else {
      available_themes[1]
    }

    available_domains <- get_domain_choices(current_theme)

    current_domain <- if (!is.null(input$selected_domain) &&
                          input$selected_domain %in% available_domains) {
      input$selected_domain
    } else {
      available_domains[1]
    }

    available_subdomains <- get_subdomain_choices(current_theme, current_domain)

    filter_controls <- list(
      selectInput(
        inputId = "selected_theme",
        label = "Theme",
        choices = available_themes,
        selected = current_theme
      ),

      selectInput(
        inputId = "selected_domain",
        label = "Domain",
        choices = available_domains,
        selected = current_domain
      )
    )

    if (!is.null(available_subdomains)) {
      filter_controls <- append(
        filter_controls,
        list(
          selectInput(
            inputId = "selected_subdomain",
            label = "Sub-domain",
            choices = available_subdomains,
            selected = "All"
          )
        )
      )
    }

    tagList(filter_controls)
  })

  output$dynamic_filters <- renderUI({

    if (input$filter_family == "Organisational Structure") {

      current_trust <- if (!is.null(input$trust) &&
                           input$trust %in% trust_choices()) {
        input$trust
      } else {
        "Oxleas NHS Foundation Trust"
      }

      organisational_filters <- list(
        selectInput(
          inputId = "trust",
          label = "Trust",
          choices = trust_choices(),
          selected = current_trust
        )
      )

      if (current_trust == "Oxleas NHS Foundation Trust") {

        current_directorate <- if (!is.null(input$directorate) &&
                                   input$directorate %in% directorate_choices()) {
          input$directorate
        } else {
          "All"
        }

        organisational_filters <- append(
          organisational_filters,
          list(
            selectInput(
              inputId = "directorate",
              label = "Directorate",
              choices = directorate_choices(),
              selected = current_directorate
            )
          )
        )

        if (current_directorate != "All") {

          current_team <- if (!is.null(input$team) &&
                              input$team %in% team_choices()) {
            input$team
          } else {
            "All"
          }

          organisational_filters <- append(
            organisational_filters,
            list(
              selectInput(
                inputId = "team",
                label = "Team",
                choices = team_choices(),
                selected = current_team
              )
            )
          )
        }
      }

      tagList(
        tags$hr(),
        organisational_filters
      )

    } else if (input$filter_family == "Protected Characteristics") {

      tagList(
        tags$hr(),

        radioButtons(
          inputId = "protected_dim",
          label = "Protected characteristic",
          choices = protected_characteristic_dims,
          selected = "Age"
        ),

        selectInput(
          inputId = "protected_value",
          label = "Value",
          choices = "All",
          selected = "All"
        )
      )

    } else if (input$filter_family == "Professional Groups") {

      tagList(
        tags$hr(),

        radioButtons(
          inputId = "professional_dim",
          label = "Professional group",
          choices = professional_group_dims,
          selected = "Occupations Grouped"
        ),

        selectInput(
          inputId = "professional_value",
          label = "Value",
          choices = "All",
          selected = "All"
        )
      )
    }
  })




  observe({
    updateSelectInput(
      session,
      inputId = "selected_theme",
      choices = theme_choices(),
      selected = theme_choices()[1]
    )
  })

# Keep selectInput choices up to date ------------------------------------

  observeEvent(input$protected_dim, {
    updateSelectInput(
      session,
      inputId = "protected_value",
      choices = protected_value_choices(),
      selected = "All"
    )
  }, ignoreNULL = FALSE)

  observeEvent(input$professional_dim, {
    updateSelectInput(
      session,
      inputId = "professional_value",
      choices = professional_value_choices(),
      selected = "All"
    )
  }, ignoreNULL = FALSE)

  # Preview only for now ----------------------------------------------------

  output$filter_state <- renderPrint({
    list(
      filter_family = input$filter_family,
      trust = input$trust,
      directorate = input$directorate,
      team = input$team,
      protected_dim = input$protected_dim,
      protected_value = input$protected_value,
      professional_dim = input$professional_dim,
      professional_value = input$professional_value,
      selected_theme = input$selected_theme,
      selected_domain = input$selected_domain,
      selected_subdomain = input$selected_subdomain
    )
  })

  output$active_filters_text <- renderText({

    active_parts <- c()

    if (input$filter_family == "Organisational Structure") {

      if (!is.null(input$trust)) {
        active_parts <- c(active_parts, paste("Trust:", input$trust))
      }

      if (!is.null(input$trust) &&
          input$trust == "Oxleas NHS Foundation Trust" &&
          !is.null(input$directorate)) {
        active_parts <- c(active_parts, paste("Directorate:", input$directorate))
      }

      if (!is.null(input$trust) &&
          input$trust == "Oxleas NHS Foundation Trust" &&
          !is.null(input$directorate) &&
          input$directorate != "All" &&
          !is.null(input$team)) {
        active_parts <- c(active_parts, paste("Team:", input$team))
      }
    }

    if (input$filter_family == "Protected Characteristics") {
      if (!is.null(input$protected_dim)) {
        active_parts <- c(active_parts, paste("Group:", input$protected_dim))
      }

      if (!is.null(input$protected_value) && input$protected_value != "All") {
        active_parts <- c(active_parts, paste("Value:", input$protected_value))
      }
    }

    if (input$filter_family == "Professional Groups") {
      if (!is.null(input$professional_dim)) {
        active_parts <- c(active_parts, paste("Group:", input$professional_dim))
      }

      if (!is.null(input$professional_value) && input$professional_value != "All") {
        active_parts <- c(active_parts, paste("Value:", input$professional_value))
      }
    }

    if (!is.null(input$selected_theme)) {
      active_parts <- c(active_parts, paste("Theme:", input$selected_theme))
    }

    if (!is.null(input$selected_domain)) {
      active_parts <- c(active_parts, paste("Domain:", input$selected_domain))
    }

    if (!is.null(input$selected_subdomain) &&
        input$selected_subdomain != "All") {
      active_parts <- c(active_parts, paste("Sub-domain:", input$selected_subdomain))
    }

    if (length(active_parts) == 0) {
      return("No filters applied")
    }

    paste(active_parts, collapse = "  |  ")
  })

}

shinyApp(ui, server)
