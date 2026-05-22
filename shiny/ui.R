ui <- dashboardPage(
  dashboardHeader(title = "NHS Staff Survey Dashboard"),
  dashboardSidebar(
    selectizeInput(
      "org_select",
      "Select Organisation",
      choices = org_choices,
      selected = "Oxleas NHS Foundation Trust",
      multiple = FALSE,
      options = list(placeholder = 'Type to search...')
    ),
    uiOutput("staff_group_ui"),
    uiOutput("directorate_ui"),
    uiOutput("team_ui"),
    uiOutput("generic_group_ui"),
    uiOutput("theme_select_ui"),
    selectInput(
      "benchmarking_toggle",
      "Turn on benchmarking?",
      choices = c("No", "Yes"),
      selected = "No"
    )
    
  ),
  dashboardBody(
    h2("What themes do you want to use to explore the data?"),
    tabsetPanel(
      id = "theme_tabs",
      selected = "NHS IMPACT",
      
      tabPanel("NHS IMPACT",
               h4(HTML("Here you can explore the NHS Staff Survey using the NHS IMPACT (Improving Patient Care Together) domains. 
                 To find out more about NHS IMPACT click <a href='https://www.england.nhs.uk/nhsimpact/' target='_blank'>here</a>.<br><br>")),
               fluidRow(
                 column(
                   width = 8,
                   uiOutput("impact_trend"),
                   uiOutput("impact_plot_description")
                 ),
                 column(
                   width = 4,
                   plotOutput("impact_gauge", height = "300px"),
                   uiOutput("impact_gauge_description")
                 )
               ),
               DT::dataTableOutput("impact_table")
      ),
      
      tabPanel("People's Promise",
               h4(HTML("Here you can explore the NHS Staff Survey using the 7 themes of the NHS People's Promise. 
                 To find more about the People's Promise click 
                 <a href='https://www.england.nhs.uk/our-nhs-people/online-version/lfaop/our-nhs-people-promise/' target='_blank'>here</a>.")),
               fluidRow(
                 column(
                   width = 8,
                   uiOutput("promise_trend"),
                   uiOutput("promise_plot_description")
                 ),
                 column(
                   width = 4,
                   plotOutput("promise_gauge", height = "300px"),
                   uiOutput("promise_gauge_description")
                 )
               ),
               DT::dataTableOutput("promise_table")
      ),
      
      tabPanel("Oxleas Values",
               h4(HTML("Here you can explore the NHS Staff Survey using Oxleas' Values as a base. Oxleas has 4 values: 
                 We're Kind, We're Fair, We Listen & We Care. To accompany these values Oxleas have also developed 
                 3 behavioural values: We Aim (things we will do), We Will Not (the things we will avoid doing) & 
                 We Listen (things we aspire to do). To find out more about Oxleas' Values click 
                 <a href='https://oxleas.nhs.uk/our-strategy-and-values/' target='_blank'>here</a>.")),
               fluidRow(
                 column(
                   width = 8,
                   uiOutput("values_trend"),
                   uiOutput("values_plot_description")
                 ),
                 column(
                   width = 4,
                   plotOutput("values_gauge", height = "300px"),
                   uiOutput("values_gauge_description")
                 )
               ),
               DT::dataTableOutput("values_table")
      ),
      
      tabPanel("Patient Safety",
               h4(HTML("Here you can explore the NHS Staff Survey focusing on Patient Safety.")),
               fluidRow(
                 column(
                   width = 8,
                   uiOutput("ps_trend"),
                   uiOutput("ps_plot_description")
                 ),
                 column(
                   width = 4,
                   plotOutput("ps_gauge", height = "300px"),
                   uiOutput("ps_gauge_description")
                 )
               ),
               DT::dataTableOutput("ps_table"),
      ),
      tabPanel("Other Themes",
               h4("Explore additional NHS Staff Survey themes."),
               fluidRow(
                 column(
                   width = 8,
                   uiOutput("other_trend"),
                   uiOutput("other_plot_description")
                 ),
                 column(
                   width = 4,
                   plotOutput("other_gauge", height = "300px"),
                   uiOutput("other_gauge_description")
                 )
               ),
               DT::dataTableOutput("other_table")
      )
    )
  )
)
