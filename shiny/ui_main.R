build_ui <- function() {

  fluidPage(
    tags$head(
      tags$style(HTML("
    .viz-row {
      margin-left: -6px !important;
      margin-right: -6px !important;
      margin-bottom: 8px !important;
    }

    .nhs-badge {
      display: inline-block;
      background-color: #005EB8;
      color: white;
      font-weight: 700;
      font-style: italic;
      padding: 4px 10px;
      margin-right: 6px;
      border-radius: 3px;
      font-size: 0.9em;
      line-height: 1;
    }

    .viz-row > [class*='col-sm-'] {
      padding-left: 6px !important;
      padding-right: 6px !important;
    }

    .viz-card {
      border: 1px solid #ddd;
      border-radius: 8px;
      padding: 10px;
      margin: 0;
      background-color: #ffffff;
      box-shadow: 0 1px 3px rgba(0,0,0,0.08);
      height: 100%;
    }

    .notes-sources-panel p {
      margin-top: 0;
      margin-bottom: 16px;
    }

    .notes-meta {
      margin-top: 18px;
      font-size: 1.5rem;
    }

    .notes-meta-row {
      display: flex;
      align-items: center;
      gap: 8px;
      margin-bottom: 10px;
    }

    .notes-meta-label {
      font-weight: 600;
      color: #333;
    }

    .github-profile-link {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      color: #156082;
      font-weight: 600;
      text-decoration: none;
    }

    .github-profile-link:hover {
      text-decoration: underline;
    }

    .github-icon {
      width: 20px;
      height: 20px;
      fill: currentColor;
      flex: 0 0 auto;
    }

    .notes-contributors-block {
      margin-top: 14px;
    }

    .notes-contributors-block .notes-meta-label {
      margin-bottom: 4px;
    }

    .notes-contributors {
      line-height: 1.5;
    }

    .notes-sources-panel h4 {
      margin-bottom: 4px;
    }

    .notes-section-heading {
      margin-top: 18px;
      margin-bottom: 4px;
    }
    .creator-link {
      display: flex;
      align-items: center;
      gap: 8px;
      margin-top: 18px;
      font-size: 1.5rem;
    }

    .github-profile-link {
      display: inline-flex;
      align-items: center;
      gap: 6px;
      color: #156082;
      font-weight: 600;
      text-decoration: none;
    }

    .github-profile-link:hover {
      text-decoration: underline;
    }

    .github-icon {
      width: 20px;
      height: 20px;
      fill: currentColor;
      flex: 0 0 auto;
    }


    .notes-sources-panel {
      border: 1px solid #ddd;
      border-radius: 8px;
      background-color: #ffffff;
      padding: 20px;
      margin-top: 12px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.06);
      font-size: 1.6rem;
      line-height: 1.5;
    }

    .selection-summary {
      margin-top: -6px;
      margin-bottom: 12px;
      font-size: 2.0rem;
      display: flex;
      flex-wrap: wrap;
      align-items: center;
      gap: 10px;
    }

    .selection-heading {
      font-weight: 600;
      color: #156082;  /* same blue as your UI */
    }

    .selection-divider {
      color: #bbb;
      margin: 0 4px;
    }

    .selection-group {
      display: inline-flex;
      align-items: center;
      gap: 4px;
    }

    .selection-label {
      font-weight: 600;
      color: #333;
      font-size: 2.0rem;
    }

    .viz-card-title {
      font-weight: 300;
      margin-bottom: 6px;
      font-size: 2.0rem;
      color: #333;
    }

    .control-label {
      font-weight: 600;
      margin-top: 10px;
      font-size: 2.0rem;
    }

    .suppressed-cell {
      display: inline-block;
      width: 2.0rem;
      height: 2.0rem;
      line-height: 1.7rem;
      text-align: center;
      vertical-align: middle;

      border-radius: 50%;
      background-color: #eef7fb;
      border: 1px solid #9ccfe3;
      color: #156082;

      font-weight: 700;
      font-size: 1.5rem;
      font-family: Arial, sans-serif;

      cursor: help;
    }

    html {
      scroll-behavior: smooth;
    }

    .gauge-scroll-link {
      text-align: center;
      margin-top: 6px;
      font-size: 1.4rem;
    }

    .gauge-scroll-link a {
      color: #156082;
      font-weight: 600;
      text-decoration: none;
      cursor: pointer;
    }

    .gauge-scroll-link a:hover {
      text-decoration: underline;
    }

    .sidebar-filter-panel {
      border: 1px solid #ddd;
      border-radius: 8px;
      background-color: #ffffff;
      padding: 12px 12px 8px 12px;
      margin-bottom: 12px;
      box-shadow: 0 1px 3px rgba(0,0,0,0.06);
    }

    .sidebar-filter-panel .form-group {
      margin-bottom: 10px;
    }

    .app-header {
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 16px;
      width: 100%;
    }

    .app-title-wrap {
      flex: 1 1 auto;
      min-width: 0;
    }

    .app-title {
      margin: 0;
      line-height: 1.05;
      font-size: clamp(2.4rem, 5vw, 4rem);
      white-space: normal;
    }

    .app-logo {
      flex: 0 0 auto;
      height: clamp(65px, 10vw, 100px);
      width: auto;
    }

    .benchmark-help {
      margin-left: 8px;
      width: 1.7rem;
      height: 1.7rem;
      line-height: 1.45rem;
      font-size: 1.2rem;
    }

    .inline-help-tooltip {
      position: relative;
      display: inline-block;
      margin-left: 8px;
    }

    .inline-help-tooltip::after {
      content: attr(data-tooltip);
      position: absolute;
      left: 50%;
      bottom: 125%;
      transform: translateX(-50%);
      background: #333;
      color: white;
      padding: 6px 9px;
      border-radius: 4px;
      white-space: nowrap;
      font-size: 1.2rem;
      font-weight: 400;
      opacity: 0;
      pointer-events: none;
      transition: opacity 0.08s ease-in-out;
      z-index: 9999;
    }

    .inline-help-tooltip:hover::after {
      opacity: 1;
    }

    .gauge-help-row {
      text-align: center;
      margin-top: -8px;
      margin-bottom: 4px;
    }

    #dynamic_filters .shiny-input-radiogroup {
      margin-top: 22px;
    }

    #filter_family option:disabled {
      color: #999;
      background-color: #f3f3f3;
    }

    .advanced-options-wrapper {
      padding: 12px 12px 8px 12px;
    }

    .advanced-options-wrapper {
      padding: 12px 12px 8px 12px;
    }

    .advanced-options-toggle {
      position: relative;
      display: block;
      padding: 0 28px 0 0;

      font-weight: 600;
      font-size: 2.0rem;
      color: #333;
      text-decoration: none;
      line-height: 1.4;
    }

    .advanced-options-toggle:hover,
    .advanced-options-toggle:focus {
      color: #333;
      text-decoration: none;
    }

    .advanced-options-arrow {
      position: absolute;
      right: 19px;
      top: 50%;

      width: 0;
      height: 0;

      border-left: 4px solid transparent;
      border-right: 4px solid transparent;
      border-top: 5px solid #333;

      transform: translateY(-50%);
    }

    .advanced-options-body {
      margin-top: 10px;
      padding-top: 12px;
      border-top: 1px solid #eee;
    }

    .advanced-options-heading {
      font-weight: 600;
      margin-bottom: 6px;
      color: #333;
      font-size: 1.4rem;
    }

    .advanced-options-help {
      font-size: 1.2rem;
      color: #666;
      margin-top: 6px;
      line-height: 1.35;
    }

    .download-data-btn {
      width: 100%;
      margin-top: 6px;
    }


    .advanced-options-body {
      margin-top: 10px;
      padding-top: 12px;
      border-top: 1px solid #eee;
    }

    .advanced-options-heading {
      font-weight: 600;
      margin-bottom: 6px;
      color: #333;
      font-size: 1.4rem;
    }

    .advanced-options-help {
      font-size: 1.2rem;
      color: #666;
      margin-top: 6px;
      line-height: 1.35;
    }

    .download-data-btn {
      width: 100%;
      margin-top: 6px;
    }

    .benchmark-tabs {
      margin-bottom: 10px;
    }

    .benchmark-tabs .nav-tabs {
      margin-bottom: 8px;
    }

    .benchmark-tabs .nav > li > a {
      padding: 6px 10px;
      font-size: 1.3rem;
    }


  "))
    ),
    fluidRow(
      class = "viz-row",
      column(
        width = 12,
        div(
          class = "app-header",

          div(
            class = "app-title-wrap",
            h1(
              class = "app-title",
              span(class = "nhs-badge", "NHS"),
              " Staff Survey"
            )
          ),

          tags$img(
            src = "www/logo.png",
            class = "app-logo"
          )
        )
      )
    ),

    tabsetPanel(
      id = "main_tabs",

      tabPanel(
        title = "Dashboard",

        sidebarLayout(
          sidebarPanel(
            div(
              class = "sidebar-filter-panel",
              uiOutput("trust_filter")
            ),

            div(
              class = "sidebar-filter-panel",
              uiOutput("theme_grouping_filters")
            ),

            conditionalPanel(
              condition = "input.trust == 'Oxleas NHS Foundation Trust'",
              div(
                class = "sidebar-filter-panel",

                uiOutput("explore_by_filter"),

                uiOutput("dynamic_filters")
              )
            ),

            div(
              class = "sidebar-filter-panel advanced-options-wrapper",

              actionLink(
                inputId = "toggle_advanced_options",
                label = tagList(
                  span("Advanced options"),
                  span(class = "advanced-options-arrow")
                ),
                class = "advanced-options-toggle"
              ),

              conditionalPanel(
                condition = "input.toggle_advanced_options % 2 == 1",

                div(
                  class = "advanced-options-body",

                  div(
                    class = "advanced-options-heading",
                    "Export data"
                  ),

                  downloadButton(
                    outputId = "download_full_comparison_csv",
                    label = "Download all questions as CSV",
                    class = "download-data-btn"
                  ),

                  div(
                    class = "advanced-options-help",
                    "Includes all survey questions for the selection: including any benchmark groups."
                  )
                )
              )
            )
          ),

          mainPanel(
            div(
              style = "padding: 20px; border: 1px solid #ddd; min-height: 400px;",

              fluidRow(
                class = "viz-row",
                column(
                  width = 7,
                  div(
                    class = "viz-card",
                    div(class = "viz-card-title", "Barometer"),
                    plotOutput("gauge_test", height = "360px"),
                    div(
                      class = "gauge-scroll-link",
                      tags$a(
                        href = "#question-table-section",
                        "View the questions used to create this result"
                      )
                    )
                  )
                ),
                column(
                  width = 5,
                  div(
                    class = "viz-card",
                    div(class = "viz-card-title", "Results over time"),
                    plotlyOutput("line_chart", height = "360px"),
                    div(
                      class = "gauge-scroll-link",
                      tags$a(
                        href = "#question-table-section",
                        "View the questions used to create this result"
                      )
                    )
                  )
                )
              ),

              fluidRow(
                class = "viz-row",
                column(
                  width = 12,
                  div(
                    class = "viz-card",
                    div(class = "viz-card-title", uiOutput("benchmark_title", inline = TRUE)),

                    div(
                      class = "benchmark-tabs",
                      uiOutput("benchmark_tabs_ui")
                    ),

                    plotlyOutput("benchmark_bar", height = "360px")
                  )
                )
              ),

              fluidRow(
                id = "question-table-section",
                class = "viz-row",
                column(
                  width = 12,
                  div(
                    class = "viz-card",
                    div(class = "viz-card-title", "Questions in this theme"),
                    DTOutput("question_table")
                  )
                )
              )
            )
          )
        )
      ),

      tabPanel(
        title = "Notes & Sources",

        div(
          class = "notes-sources-panel",
          shiny::includeMarkdown("shiny/notes_sources.Rmd")
        )
      )
    )
  )
}
