`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

build_server <- function() {
  function(input, output, session) {


    # -----------------------------
    # Messsages when there's nothing to display in Plotly
    # -----------------------------
    empty_plotly_message <- function(message) {
      plotly::plot_ly() %>%
        plotly::layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            list(
              text = message,
              x = 0.5,
              y = 0.5,
              xref = "paper",
              yref = "paper",
              showarrow = FALSE,
              align = "center",
              font = list(size = 16, color = "#333")
            )
          ),
          margin = list(l = 20, r = 20, t = 20, b = 20)
        )
    }

    # -----------------------------
    # Theme / Domain / Sub-domain
    # -----------------------------
    theme_choices <- reactive({
      get_theme_questions_map() %>%
        pull(theme) %>%
        na.omit() %>%
        unique() %>%
        sort()
    })

    get_domain_choices <- function(selected_theme) {
      get_theme_questions_map() %>%
        filter(theme == selected_theme) %>%
        pull(domain) %>%
        na.omit() %>%
        unique() %>%
        sort()
    }

    get_subdomain_choices <- function(selected_theme, selected_domain) {
      subdomain_values <- get_theme_questions_map() %>%
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
      get_trust_choices()
    })

    is_oxleas_selected <- reactive({
      !is.null(input$trust) &&
        input$trust == "Oxleas NHS Foundation Trust"
    })

    active_filter_family <- reactive({

      unavailable_topic <- !is.null(input$selected_theme) &&
        input$selected_theme %in% c("People's Promise", "Other")

      if (!is_oxleas_selected()) {
        return("Organisational Structure")
      }

      if (unavailable_topic) {
        return("Organisational Structure")
      }

      if (is.null(input$filter_family)) {
        "Organisational Structure"
      } else {
        input$filter_family
      }
    })

    active_directorate <- reactive({
      if (
        is_oxleas_selected() &&
        active_filter_family() == "Organisational Structure"
      ) {
        input$directorate
      } else {
        NULL
      }
    })

    active_team <- reactive({
      if (
        is_oxleas_selected() &&
        active_filter_family() == "Organisational Structure" &&
        !is.null(active_directorate()) &&
        active_directorate() != "All"
      ) {
        input$team
      } else {
        NULL
      }
    })

    directorate_choices <- reactive({
      get_ox_q_aggregate_results() %>%
        filter(dim == "Directorate") %>%
        pull(dim_sub) %>%
        all_and_sort()
    })

    team_choices <- reactive({
      req(active_directorate())

      if (active_directorate() == "All") {
        return("All")
      }

      get_ox_q_aggregate_results() %>%
        filter(
          dim == "Team",
          directorate == active_directorate()
        ) %>%
        pull(dim_sub) %>%
        all_and_sort()
    })

    # -----------------------------
    # Protected Characteristics
    # -----------------------------
    protected_value_choices <- reactive({
      req(input$protected_dim)

      get_ox_q_aggregate_results() %>%
        filter(dim == input$protected_dim) %>%
        pull(dim_sub) %>%
        sort_only()
    })


    # -----------------------------
    # Gauge Chart Inputs
    # -----------------------------
    gauge_inputs <- reactive({
      req(input$trust, input$selected_theme, input$selected_domain)

      get_metric_data_df(
        trust_sel = input$trust,
        theme_sel = input$selected_theme,
        domain_sel = input$selected_domain,
        subdomain_sel = input$selected_subdomain,
        filter_family = active_filter_family(),
        directorate = active_directorate(),
        team = active_team(),
        protected_dim = input$protected_dim,
        protected_value = input$protected_value,
        professional_dim = input$professional_dim,
        professional_value = input$professional_value,
        score = "score",
        return_type = "gauge"
      )
    })

    # -----------------------------
    # Professional Groups
    # -----------------------------
    professional_value_choices <- reactive({
      req(input$professional_dim)

      get_ox_q_aggregate_results() %>%
        filter(dim == input$professional_dim) %>%
        pull(dim_sub) %>%
        sort_only()
    })

    # -----------------------------
    # Line Chart
    # -----------------------------

    # Render line chart
    output$line_chart <- renderPlotly({
      req(input$trust, input$selected_theme, input$selected_domain)

      df <- get_metric_data_df(
        trust_sel = input$trust,
        theme_sel = input$selected_theme,
        domain_sel = input$selected_domain,
        subdomain_sel = input$selected_subdomain,
        filter_family = active_filter_family(),
        directorate = active_directorate(),
        team = active_team(),
        protected_dim = input$protected_dim,
        protected_value = input$protected_value,
        professional_dim = input$professional_dim,
        professional_value = input$professional_value,
        score = "score",
        return_type = "line"
      ) %>%
        mutate(
          year = as.integer(year),
          score = as.numeric(score),
          plot_score = if (input$selected_theme %in% c("People's Promise", "Other")) {
            score / 10
          } else {
            score
          }
        ) %>%
        filter(!is.na(year), !is.na(plot_score))

      shiny::validate(
        shiny::need(nrow(df) > 0, "No line chart data available for this selection")
      )

      y_min <- max(
        0,
        floor((min(df$plot_score, na.rm = TRUE) - 0.05) * 100) / 100
      )

      y_max <- min(
        1,
        ceiling((max(df$plot_score, na.rm = TRUE) + 0.05) * 100) / 100
      )

      plot_ly(
        data = df,
        x = ~year,
        y = ~plot_score,
        type = "scatter",
        mode = "lines+markers",
        line = list(
          color = "#C1E5F5",
          width = 3
        ),
        marker = list(
          color = "#156082",
          size = 8
        ),
        hovertemplate = "Year: %{x}<br>Score: %{y:.1%}<extra></extra>"
      ) %>%
        layout(
          showlegend = FALSE,
          xaxis = list(
            title = "",
            tickmode = "array",
            tickvals = sort(unique(df$year)),
            showgrid = FALSE
          ),
          yaxis = list(
            title = "",
            range = c(y_min, y_max),
            tickformat = ".0%",
            showgrid = FALSE
          )
        )
    })

    # ----------------------------------
    # Benchmark Tooltip for Trust Level
    # ----------------------------------
    trust_benchmark_tooltip <- reactive({

      if (is.null(input$trust) || length(input$trust) == 0) {
        return(NULL)
      }

      trust_sel <- input$trust[[1]]

      trust_rows <- files$nat_result_themes %>%
        dplyr::filter(org_name == .env$trust_sel | org_id == .env$trust_sel)

      selected_type <- trust_rows$org_type_reporting_name %>%
        stats::na.omit() %>%
        as.character()

      selected_region <- trust_rows$region_name %>%
        stats::na.omit() %>%
        as.character()

      if (length(selected_type) == 0 || length(selected_region) == 0) {
        return(NULL)
      }

      tooltip_text <- paste0(
        selected_type[[1]],
        " in ",
        stringr::str_to_title(selected_region[[1]])
      )

      # Fix double-escaped ampersands if they are already stored as &amp;
      tooltip_text <- stringr::str_replace_all(tooltip_text, "&amp;", "&")

      tooltip_text
    })

    team_benchmark_group_available <- reactive({

      if (
        active_filter_family() != "Organisational Structure" ||
        is.null(active_directorate()) ||
        active_directorate() == "All" ||
        is.null(active_team()) ||
        active_team() == "All"
      ) {
        return(FALSE)
      }

      selected_team <- files$ox_teams_map %>%
        dplyr::filter(
          directorate == active_directorate(),
          team_full == active_team() | team_short == active_team()
        ) %>%
        dplyr::slice(1)

      if (
        nrow(selected_team) == 0 ||
        is.na(selected_team$benchmark_group[[1]])
      ) {
        return(FALSE)
      }

      benchmark_group <- selected_team$benchmark_group[[1]]

      benchmark_teams <- files$ox_teams_map %>%
        dplyr::filter(benchmark_group == !!benchmark_group) %>%
        dplyr::transmute(team_name = team_short) %>%
        dplyr::bind_rows(
          files$ox_teams_map %>%
            dplyr::filter(benchmark_group == !!benchmark_group) %>%
            dplyr::transmute(team_name = team_full)
        ) %>%
        dplyr::pull(team_name) %>%
        stats::na.omit() %>%
        unique()

      base <- files$ox_theme_results %>%
        dplyr::filter(
          theme == input$selected_theme,
          domain == input$selected_domain
        ) %>%
        apply_subdomain_filter(input$selected_subdomain)

      if (nrow(base) == 0) {
        return(FALSE)
      }

      latest_year <- max(base$year, na.rm = TRUE)

      n_with_data <- base %>%
        dplyr::filter(
          year == latest_year,
          dim == "Team",
          dim_sub %in% benchmark_teams
        ) %>%
        dplyr::group_by(dim_sub) %>%
        dplyr::summarise(
          score = if (all(is.na(score))) NA_real_ else mean(score, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(!is.na(score)) %>%
        nrow()

      n_with_data > 1
    })


    output$benchmark_title <- renderUI({

      title_info <- benchmark_title()

      is_trust_level <- (
        active_filter_family() == "Organisational Structure" &&
          (is.null(active_directorate()) || active_directorate() == "All")
      )

      tooltip_text <- if (
        is_trust_level &&
        benchmark_view() == "trust_region_type"
      ) {
        trust_benchmark_tooltip()
      } else {
        NULL
      }

      if (!is.null(tooltip_text) && tooltip_text != "") {
        tagList(
          span(title_info$text),
          span(
            class = "suppressed-cell benchmark-help inline-help-tooltip",
            `data-tooltip` = tooltip_text,
            "?"
          )
        )
      } else {
        span(title_info$text)
      }
    })

    # BENCHMARK DYNAMIC CONTEXT
    benchmark_context <- reactive({

      if (active_filter_family() == "Protected Characteristics") {
        return("demographics")
      }

      if (active_filter_family() == "Professional Groups") {
        return("professions")
      }

      if (
        active_filter_family() == "Organisational Structure" &&
        !is.null(active_team()) &&
        active_team() != "All"
      ) {
        return("team")
      }

      if (
        active_filter_family() == "Organisational Structure" &&
        !is.null(active_directorate()) &&
        active_directorate() != "All"
      ) {
        return("directorate")
      }

      "trust"
    })


    benchmark_tab_choices <- reactive({

      switch(
        benchmark_context(),

        trust = c(
          "Trust type" = "trust_type",
          "Region & type" = "trust_region_type",
          "All trusts" = "trust_all_trusts",
          "Themes in current topic" = "themes"
        ),

        demographics = c(
          "Other demographics" = "demographics_other",
          "Themes in current topic" = "themes"
        ),

        professions = c(
          "Other professions" = "professions_other",
          "Themes in current topic" = "themes"
        ),

        directorate = c(
          "Other directorates" = "directorate_other",
          "Themes in current topic" = "themes"
        ),

        team = c(
          "All teams in directorate" = "team_directorate",
          "Other benchmark teams" = "team_benchmark_group",
          "All teams in trust" = "team_trust",
          "Themes in current topic" = "themes"
        )
      )
    })


    output$benchmark_tabs_ui <- renderUI({

      choices <- benchmark_tab_choices()

      tab_list <- lapply(seq_along(choices), function(i) {
        tabPanel(
          title = names(choices)[[i]],
          value = unname(choices[[i]])
        )
      })

      do.call(
        tabsetPanel,
        c(
          list(
            id = "benchmark_view",
            type = "tabs",
            selected = unname(choices[[1]])
          ),
          unname(tab_list)
        )
      )
    })


    benchmark_view <- reactive({

      choices <- benchmark_tab_choices()
      current <- input$benchmark_view

      if (is.null(current) || !current %in% unname(choices)) {
        unname(choices[[1]])
      } else {
        current
      }
    })

    # ----------------------------------
    # Dynamic Title for Benchmark Graph
    # ----------------------------------
    benchmark_title <- reactive({

      title_text <- switch(
        benchmark_view(),

        trust_type = "Results for trusts of the same type",
        trust_region_type = "Results for similar trusts in your region",
        trust_all_trusts = "Results for all trusts",
        themes = "Results by theme",

        demographics_other = paste0("Results for other ", input$protected_dim),
        professions_other = paste0("Results for other ", input$professional_dim),

        directorate_other = "Results for other Directorates",

        team_directorate = "Results for all Teams in this Directorate",

        team_benchmark_group = if (team_benchmark_group_available()) {
          "Results for similar Teams"
        } else {
          "Results for selected Team and Directorate"
        },

        team_trust = "Results for all Teams in this Trust",

        "Benchmark results"
      )

      list(
        text = title_text,
        tooltip = NULL
      )
    })


    # -----------------------------
    # Benchmark Bar Chart
    # -----------------------------
    output$benchmark_bar <- renderPlotly({

      shiny::req(input$trust, input$selected_theme, input$selected_domain, cancelOutput = TRUE)

      if (
        benchmark_view() == "team_benchmark_group" &&
        active_filter_family() == "Organisational Structure" &&
        !is.null(active_team()) &&
        active_team() != "All"
      ) {

        selected_team <- files$ox_teams_map %>%
          dplyr::filter(
            directorate == active_directorate(),
            team_full == active_team() | team_short == active_team()
          ) %>%
          dplyr::slice(1)

        no_benchmark_group <- (
          nrow(selected_team) == 0 ||
            is.na(selected_team$benchmark_group[[1]])
        )

        if (no_benchmark_group) {
          return(
            empty_plotly_message(
              "This team is unique! We have not allocated any teams to benchmark it against."
            )
          )
        }
      }

      df <- get_benchmark_bar_df(
        theme_sel = input$selected_theme,
        domain_sel = input$selected_domain,
        subdomain_sel = input$selected_subdomain,
        benchmark_view = benchmark_view(),
        filter_family = active_filter_family(),
        directorate = active_directorate(),
        team = active_team(),
        trust_sel = input$trust,
        protected_dim = input$protected_dim,
        protected_value = input$protected_value,
        professional_dim = input$professional_dim,
        professional_value = input$professional_value,
        score = "score"
      )


      shiny::validate(
        shiny::need(nrow(df) > 0, "No benchmark group available for this selection"),
        shiny::need(
          all(c("dim_sub", "score", "selected") %in% names(df)),
          "Benchmark data is not in the expected format"
        )
      )

      df <- df %>%
        mutate(
          dim_sub = as.character(dim_sub),
          score = as.numeric(score),
          plot_score = if (input$selected_theme %in% c("People's Promise", "Other")) {
            score / 10
          } else {
            score
          },
          selected = dplyr::coalesce(as.logical(selected), FALSE),
          bar_colour = dplyr::if_else(selected, "#156082", "#C1E5F5")
        ) %>%
        filter(
          !is.na(dim_sub),
          !is.na(plot_score),
          is.finite(plot_score)
        ) %>%
        arrange(desc(plot_score))

      if (nrow(df) == 0) {
        return(
          empty_plotly_message(
            "No benchmark data available for this selection."
          )
        )
      }

      shiny::validate(
        shiny::need(nrow(df) > 0, "No benchmark group available for this selection")
      )

      x_min_pct <- max(
        0,
        floor((min(df$plot_score, na.rm = TRUE) - 0.05) * 100)
      )

      x_max_pct <- min(
        100,
        ceiling((max(df$plot_score, na.rm = TRUE) + 0.01) * 100)
      )

      if (x_min_pct >= x_max_pct) {
        x_min_pct <- max(0, x_min_pct - 1)
        x_max_pct <- min(100, x_max_pct + 1)
      }

      x_min <- x_min_pct / 100
      x_max <- x_max_pct / 100

      x_tick_pct <- pretty(c(x_min_pct, x_max_pct), n = 5)

      x_tick_pct <- x_tick_pct[
        x_tick_pct >= x_min_pct &
          x_tick_pct <= x_max_pct
      ]

      x_tick_pct <- sort(unique(c(x_min_pct, x_tick_pct, x_max_pct)))

      x_tick_vals <- x_tick_pct / 100
      x_tick_text <- paste0(x_tick_pct, "%")

      plot_ly(
        data = df,
        x = ~plot_score,
        y = ~dim_sub,
        type = "bar",
        orientation = "h",
        marker = list(color = ~bar_colour),
        hovertemplate = "%{y}<br>Score: %{x:.1%}<extra></extra>"
      ) %>%
        layout(
          showlegend = FALSE,
          xaxis = list(
            title = "",
            range = c(x_min, x_max),
            tickmode = "array",
            tickvals = x_tick_vals,
            ticktext = x_tick_text
          ),
          yaxis = list(
            title = "",
            categoryorder = "array",
            categoryarray = rev(df$dim_sub),
            automargin = TRUE,
            ticks = "outside",
            ticklen = 8,
            tickcolor = "rgba(0,0,0,0)"
          ),
          margin = list(
            l = 220,
            b = 60
          )
        )
    })

    # -----------------------------
    # Gauge Chart
    # -----------------------------
    output$gauge_test <- renderPlot({
      gauge_vals <- gauge_inputs()

      req(
        !is.null(gauge_vals),
        !is.null(gauge_vals$top),
        !is.null(gauge_vals$bottom)
      )

      if (is.null(gauge_vals$val) || is.na(gauge_vals$val)) {
        gauge_plot(
          val = NA_real_,
          display_val = "'No data'"
        )
      } else {
        needle_val <- calc_needle(
          top = gauge_vals$top,
          bottom = gauge_vals$bottom,
          val = gauge_vals$val
        )

        gauge_plot(
          val = needle_val,
          display_val = gauge_vals$display_val
        )
      }
    })



    # -----------------------------
    # Dynamic UI
    # -----------------------------
    output$theme_grouping_filters <- renderUI({

      # map input back to raw values for logic
      raw_theme_choices <- theme_choices()

      ui_theme_choices <- raw_theme_choices %>%
        dplyr::recode("Other" = "Engagement & Morale")

      default_theme <- if ("Patient Safety" %in% raw_theme_choices) {
        "Patient Safety"
      } else {
        raw_theme_choices[1]
      }

      current_theme <- if (
        !is.null(input$selected_theme) &&
        input$selected_theme %in% raw_theme_choices
      ) {
        input$selected_theme
      } else {
        default_theme
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
          label = "Topic",
          choices = setNames(raw_theme_choices, ui_theme_choices),
          selected = current_theme
        ),

        selectInput(
          inputId = "selected_domain",
          label = "Themes in this topic",
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
              label = "Sub-theme",
              choices = available_subdomains,
              selected = "All"
            )
          )
        )
      }

      tagList(filter_controls)
    })

    # Trust UI Output
    output$trust_filter <- renderUI({

      current_trust <- if (
        !is.null(input$trust) &&
        input$trust %in% trust_choices()
      ) {
        input$trust
      } else {
        "Oxleas NHS Foundation Trust"
      }

      selectInput(
        inputId = "trust",
        label = "Trust",
        choices = trust_choices(),
        selected = current_trust
      )
    })

    # Dynamic filter logic for PP or Morale & Engagement are selected so that
    # Occupations & Demographics options are not available (not calculated in
    # national datasest I am using currently)
    output$explore_by_filter <- renderUI({

      unavailable_topic <- !is.null(input$selected_theme) &&
        input$selected_theme %in% c("People's Promise", "Other")

      if (unavailable_topic) {

        tagList(
          div(
            class = "form-group shiny-input-container",

            tags$label(
              class = "control-label",
              tagList(
                "Explore by...",
                span(
                  class = "suppressed-cell benchmark-help inline-help-tooltip",
                  `data-tooltip` = "Demographics and Professions data are not available for this topic",
                  "?"
                )
              )
            ),

            selectInput(
              inputId = "filter_family",
              label = NULL,
              choices = c(
                "Directorates" = "Organisational Structure"
              ),
              selected = "Organisational Structure"
            )
          )
        )

      } else {

        selectInput(
          inputId = "filter_family",
          label = "Explore by...",
          choices = c(
            "Directorates" = "Organisational Structure",
            "Demographics" = "Protected Characteristics",
            "Professions" = "Professional Groups"
          ),
          selected = if (
            !is.null(input$filter_family) &&
            input$filter_family %in% c(
              "Organisational Structure",
              "Protected Characteristics",
              "Professional Groups"
            )
          ) {
            input$filter_family
          } else {
            "Organisational Structure"
          }
        )
      }
    })

    output$dynamic_filters <- renderUI({

      if (!is_oxleas_selected()) {
        return(NULL)
      }

      if (active_filter_family() == "Organisational Structure") {

        current_directorate <- if (
          !is.null(active_directorate()) &&
          active_directorate() %in% directorate_choices()
        ) {
          active_directorate()
        } else {
          "All"
        }

        organisational_filters <- list(
          selectInput(
            inputId = "directorate",
            label = "Directorate",
            choices = directorate_choices(),
            selected = current_directorate
          )
        )

        if (current_directorate != "All") {

          current_team <- if (
            !is.null(active_team()) &&
            active_team() %in% team_choices()
          ) {
            active_team()
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

        tagList(
          organisational_filters
        )

      } else if (active_filter_family() == "Protected Characteristics") {

        tagList(
          radioButtons(
            inputId = "protected_dim",
            label = NULL,
            choices = get_protected_characteristic_dims(),
            selected = "Age"
          ),

          div(
            style = "margin-top: 10px;",
            selectInput(
              inputId = "protected_value",
              label = "Value",
              choices = character(0),
              selected = character(0)
            )
          )
        )

      } else if (active_filter_family() == "Professional Groups") {

        tagList(
          radioButtons(
            inputId = "professional_dim",
            label = NULL,
            choices = get_professional_group_dims(),
            selected = "Occupations Grouped"
          ),

          div(
            style = "margin-top: 10px;",
            selectInput(
              inputId = "professional_value",
              label = "Value",
              choices = "All",
              selected = "All"
            )
          )
        )
      }
    })

    # -----------------------------
    # Keep selectInput choices up to date
    # -----------------------------
    observeEvent(input$protected_dim, {
      vals <- protected_value_choices()

      default_val <- if ("51-65" %in% vals) {
        "51-65"
      } else {
        vals[1]
      }

      updateSelectInput(
        session,
        inputId = "protected_value",
        choices = vals,
        selected = default_val
      )
    }, ignoreNULL = FALSE)

    observeEvent(input$professional_dim, {
      vals <- professional_value_choices()
      updateSelectInput(
        session,
        inputId = "professional_value",
        choices = vals,
        selected = vals[1]
      )
    }, ignoreNULL = FALSE)

    output$question_table <- DT::renderDT({
      req(input$trust, input$selected_theme, input$selected_domain)

      df <- get_question_table_df(
        trust_sel = input$trust,
        theme_sel = input$selected_theme,
        domain_sel = input$selected_domain,
        subdomain_sel = input$selected_subdomain,
        filter_family = active_filter_family(),
        directorate = active_directorate(),
        team = active_team(),
        protected_dim = input$protected_dim,
        protected_value = input$protected_value,
        professional_dim = input$professional_dim,
        professional_value = input$professional_value,
        score = "score"
      )

      year_cols <- names(df)[stringr::str_detect(names(df), "^\\d{4}$")]

      suppression_msg <- htmltools::htmlEscape(
        "Data removed to preserve respondent anonymity. See notes & sources tab for more details"
      )

      not_available_msg <- htmltools::htmlEscape(
        "This question was not included in this annual survey."
      )

      df_display <- df

      df_display <- df_display %>%
        dplyr::select(-dplyr::starts_with("missing_reason_"))


      for (yr in year_cols) {

        reason_col <- paste0("missing_reason_", yr)

        df_display[[yr]] <- mapply(
          function(value, reason) {

            if (!is.na(value)) {
              return(sprintf("%.1f%%", as.numeric(value) * 100))
            }

            tooltip <- if (!is.na(reason) && reason == "not_available") {
              not_available_msg
            } else {
              suppression_msg
            }

            paste0(
              '<span class="suppressed-cell" title="',
              tooltip,
              '">?</span>'
            )
          },
          df[[yr]],
          if (reason_col %in% names(df)) df[[reason_col]] else NA_character_,
          USE.NAMES = FALSE
        )
      }
      DT::datatable(
        df_display,
        rownames = FALSE,
        escape = FALSE,
        options = list(
          dom = "tip",
          pageLength = 20,
          scrollX = TRUE
        )
      )
    })

    output$download_full_comparison_csv <- downloadHandler(

      filename = function() {
        fname <- paste0(
          "staff_survey_data_",
          sample(100000:999999, 1),
          ".csv"
        )

        cat("\nDOWNLOAD filename called:", fname, "\n")
        fname
      },

      contentType = "text/csv",

      content = function(file) {

        cat("\nDOWNLOAD content started\n")
        cat("Target temp file:", file, "\n")

        export_df <- tryCatch(
          {
            cat("About to call get_full_comparison_download_df()\n")

            out <- get_full_comparison_download_df(
              trust_sel = input$trust,
              filter_family = active_filter_family(),
              directorate = active_directorate(),
              team = active_team(),
              protected_dim = input$protected_dim,
              protected_value = input$protected_value,
              professional_dim = input$professional_dim,
              professional_value = input$professional_value,
              score = "score"
            )

            cat("Export helper returned successfully\n")
            cat("Rows:", nrow(out), "\n")
            cat("Columns:", paste(names(out), collapse = ", "), "\n")

            out
          },
          error = function(e) {
            cat("EXPORT HELPER ERROR:\n")
            cat(conditionMessage(e), "\n")

            tibble::tibble(
              error = "Export helper failed",
              message = conditionMessage(e)
            )
          }
        )

        cat("About to write Shiny temp CSV\n")

        tryCatch(
          {
            readr::write_csv(export_df, file, na = "")

            cat("Temp CSV write completed\n")
            cat("Temp file exists:", file.exists(file), "\n")
            cat("Temp file size:", file.info(file)$size, "\n")

            debug_file <- file.path(getwd(), "download_debug_last.csv")
            file.copy(file, debug_file, overwrite = TRUE)

            cat("Debug copy path:", debug_file, "\n")
            cat("Debug copy exists:", file.exists(debug_file), "\n")
            cat("Debug copy size:", file.info(debug_file)$size, "\n")
            cat("DOWNLOAD content finished\n")
          },
          error = function(e) {
            cat("CSV WRITE ERROR:\n")
            cat(conditionMessage(e), "\n")

            fallback_df <- tibble::tibble(
              error = "CSV write failed",
              message = conditionMessage(e)
            )

            readr::write_csv(fallback_df, file, na = "")

            debug_file <- file.path(getwd(), "download_debug_last.csv")
            file.copy(file, debug_file, overwrite = TRUE)

            cat("Fallback debug copy written:", debug_file, "\n")
          }
        )
      }
    )

    outputOptions(output, "download_full_comparison_csv", suspendWhenHidden = FALSE)


    # -----------------------------
    # Debug info
    # -----------------------------
    output$filter_state <- renderPrint({
      list(
        filter_family = active_filter_family(),
        directorate = active_directorate(),
        team = active_team(),
        protected_dim = input$protected_dim,
        protected_value = input$protected_value,
        professional_dim = input$professional_dim,
        professional_value = input$professional_value,
        selected_theme = input$selected_theme,
        selected_domain = input$selected_domain,
        selected_subdomain = input$selected_subdomain
      )
    })

    output$active_filters_block <- renderUI({

      parts <- list()

      # -----------------------------
      # Organisation filters
      # -----------------------------
      if (active_filter_family() == "Organisational Structure") {

        if (!is.null(input$trust)) {
          parts <- append(parts, list(
            span(class = "selection-group",
                 span(class = "selection-label", "Trust: "),
                 input$trust)
          ))
        }

        if (!is.null(active_directorate()) && active_directorate() != "All") {
          parts <- append(parts, list(
            span(class = "selection-group",
                 span(class = "selection-label", "Directorate: "),
                 active_directorate())
          ))
        }

        if (!is.null(active_team()) && active_team() != "All") {
          parts <- append(parts, list(
            span(class = "selection-group",
                 span(class = "selection-label", "Team: "),
                 active_team())
          ))
        }
      }

      # -----------------------------
      # Demographics
      # -----------------------------
      if (active_filter_family() == "Protected Characteristics") {

        if (!is.null(input$protected_dim)) {
          parts <- append(parts, list(
            span(class = "selection-group",
                 span(class = "selection-label", "Characteristic: "),
                 input$protected_dim)
          ))
        }

        if (!is.null(input$protected_value) && input$protected_value != "All") {
          parts <- append(parts, list(
            span(class = "selection-group",
                 span(class = "selection-label", "Value: "),
                 input$protected_value)
          ))
        }
      }

      # -----------------------------
      # Professions
      # -----------------------------
      if (active_filter_family() == "Professional Groups") {

        if (!is.null(input$professional_dim)) {
          parts <- append(parts, list(
            span(class = "selection-group",
                 span(class = "selection-label", "Profession group: "),
                 input$professional_dim)
          ))
        }

        if (!is.null(input$professional_value) && input$professional_value != "All") {
          parts <- append(parts, list(
            span(class = "selection-group",
                 span(class = "selection-label", "Value: "),
                 input$professional_value)
          ))
        }
      }

      # -----------------------------
      # Theme hierarchy (always show)
      # -----------------------------
      if (!is.null(input$selected_theme)) {
        parts <- append(parts, list(
          span(class = "selection-group",
               span(class = "selection-label", "Topic: "),
               input$selected_theme)
        ))
      }

      if (!is.null(input$selected_domain)) {
        parts <- append(parts, list(
          span(class = "selection-group",
               span(class = "selection-label", "Theme: "),
               input$selected_domain)
        ))
      }

      if (!is.null(input$selected_subdomain) && input$selected_subdomain != "All") {
        parts <- append(parts, list(
          span(class = "selection-group",
               span(class = "selection-label", "Sub-theme: "),
               input$selected_subdomain)
        ))
      }

      do.call(tagList, parts)
    })

  }

}


