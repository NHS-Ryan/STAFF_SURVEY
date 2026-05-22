server <- function(input, output, session) {
  
  get_team_simple <- function(team_raw) {
    if (is.null(team_raw) || identical(team_raw, "All")) return(team_raw)
    out <- dbGetQuery(con, "SELECT team_simple FROM oxleas_esr_teams WHERE team = $1 LIMIT 1",
                      params = list(team_raw))$team_simple
    if (length(out) == 1 && !is.na(out)) out else team_raw
  }
  
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  
  output$selected_org <- renderPrint({
    input$org_select
  })
  
  observe({
    print(paste("theme_tabs value is:", input$theme_tabs))
  })
  
  output$staff_group_ui <- renderUI({
    req(input$org_select)
    if (input$org_select == "Oxleas NHS Foundation Trust") {
      selectizeInput(
        "staff_group_select",
        "What staff group do you want to explore?",
        choices = c("Organisation", "Directorate", groups),
        selected = "Organisation",
        multiple = FALSE,
        options = list(placeholder = 'Type to search staff group...')
      )
    } else {
      NULL
    }
  })
  
  output$directorate_ui <- renderUI({
    req(input$staff_group_select)
    if (input$staff_group_select == "Directorate") {
      selectizeInput(
        "directorate_select",
        "Select Directorate",
        choices = c(directorates),
        selected = NULL,
        multiple = FALSE,
        options = list(placeholder = 'Type to search directorate...')
      )
    } else {
      NULL
    }
  })
  
  output$team_ui <- renderUI({
    req(input$staff_group_select == "Directorate", input$directorate_select)
    
    teams_query <- dbGetQuery(con, sprintf("
    SELECT team, team_simple
    FROM oxleas_esr_teams
    WHERE directorate = %s
    ORDER BY team_simple, team
  ", dbQuoteString(con, input$directorate_select)))
    
    # Named vector: names (labels shown) = team_simple, values (internal) = team
    team_choices <- setNames(teams_query$team, teams_query$team_simple)
    
    selectizeInput(
      "team_select",
      "Select Team",
      choices = c("All" = "All", team_choices),
      selected = NULL,
      multiple = FALSE,
      options = list(placeholder = 'Type to search team...')
    )
  })
  
  
  output$generic_group_ui <- renderUI({
    req(input$staff_group_select)
    other_groups <- c("Age", "Ethnic Group", "Ethnicity", "Gender", "Religion", "Sexuality", "Staff Group")
    if (input$staff_group_select %in% other_groups) {
      
      query <- sprintf("
        SELECT DISTINCT subgroup
        FROM nhs_ss_response_level_data
        WHERE \"group\" = '%s'
        ORDER BY subgroup
        LIMIT 100
      ", input$staff_group_select)
      
      subgroups <- dbGetQuery(con, query)$subgroup
      
      selectizeInput(
        "generic_subgroup_select",
        sprintf("Select %s", input$staff_group_select),
        choices = subgroups,
        selected = NULL,
        multiple = FALSE,
        options = list(placeholder = 'Type to search...')
      )
    } else {
      return(NULL)
    }
  })
  
  output$theme_select_ui <- renderUI({
    req(input$theme_tabs)
    valid_tabs <- c("NHS IMPACT", "People's Promise", "Oxleas Values","Patient Safety","Other Themes")
    if (!(input$theme_tabs %in% valid_tabs)) return(NULL)
    
    safe_theme <- dbQuoteString(con, input$theme_tabs)
    
    query <- sprintf("
      SELECT DISTINCT domain
      FROM nhs_ss_themes
      WHERE theme = %s
      ORDER BY domain
    ", safe_theme)
    
    themes_df <- dbGetQuery(con, query)
    
    selectizeInput(
      "select_theme",
      label = "Select theme",
      choices = themes_df$domain,
      selected = NULL,
      multiple = FALSE,
      options = list(placeholder = 'Type to search themes...')
    )
  })
  
  render_theme_plot <- function(tab_name, output_id) {
    output[[output_id]] <- renderUI({
      req(input$org_select, input$staff_group_select, input$select_theme, input$theme_tabs == tab_name)
      plotlyOutput(paste0(output_id, "_real"), height = "300px")
    })
    
    output[[paste0(output_id, "_real")]] <- renderPlotly({
      req(input$org_select, input$staff_group_select, input$select_theme, input$theme_tabs == tab_name)
      
      # Determine group and subgroup
      if (input$staff_group_select == "Directorate") {
        if (!is.null(input$team_select) && input$team_select != "All") {
          filter_group <- "Team"
          filter_subgroup <- input$team_select
        } else {
          filter_group <- "Directorate"
          filter_subgroup <- input$directorate_select
        }
      } else if (input$staff_group_select == "Organisation") {
        filter_group <- "Organisation"
        filter_subgroup <- NULL
      } else {
        filter_group <- input$staff_group_select
        filter_subgroup <- input$generic_subgroup_select
      }
      
      org_info <- dbGetQuery(
        con,
        "SELECT ods_code, benchmarking_group FROM nhs_orgs WHERE organisation = $1 LIMIT 1",
        params = list(input$org_select)
      )
      req(nrow(org_info) == 1)
      ods_code <- org_info$ods_code
      benchmark_type <- org_info$benchmarking_group
      
      if (filter_group == "Team" && !is.null(filter_subgroup)) {
        benchmark_type <- dbGetQuery(con, "
        SELECT directorate FROM oxleas_esr_teams WHERE team = $1 LIMIT 1",
                                     params = list(filter_subgroup))$directorate
      }
      
      q_ids <- dbGetQuery(
        con,
        "SELECT id FROM nhs_ss_themes WHERE theme = $1 AND domain = $2",
        params = list(tab_name, input$select_theme)
      )$id
      
      q_ids_str <- paste(sprintf("'%s'", q_ids), collapse = ", ")
      subgroup_clause <- if (!is.null(filter_subgroup)) "AND subgroup = $3" else ""
      
      main_query <- sprintf("
      SELECT year, AVG(percent_positive) AS avg_positive
      FROM nhs_ss_percent_positive
      WHERE \"group\" = $1 AND ods_code = $2 %s AND q_id IN (%s)
      GROUP BY year ORDER BY year", subgroup_clause, q_ids_str)
      
      if (!is.null(filter_subgroup)) {
        main_df <- dbGetQuery(con, main_query, params = list(filter_group, ods_code, filter_subgroup))
      } else {
        main_df <- dbGetQuery(con, main_query, params = list(filter_group, ods_code))
      }
      print(paste("main_df rows:", nrow(main_df)))
      
      main_df <- main_df %>% mutate(type = "Your Selection")
      
      if (input$benchmarking_toggle == "Yes") {
        subdomain_clause <- if (tab_name %in% c("People's Promise", "Oxleas Values", "Other Themes")) {
          "AND subdomain IS NULL"
        } else {
          ""
        }
        
        bench_query <- sprintf("
    SELECT year, stat_type, value
    FROM nhs_ss_all_bench
    WHERE theme = $1 AND domain = $2 AND benchmarking_group = $3
    %s
  ", subdomain_clause)
        
        benchmark_df <- dbGetQuery(con, bench_query,
                                   params = list(tab_name, input$select_theme, benchmark_type))
        
        if (nrow(benchmark_df) > 0) {
          benchmark_df <- benchmark_df %>%
            mutate(value = as.numeric(value)) %>%
            tidyr::pivot_wider(names_from = stat_type, values_from = value) %>%
            tidyr::pivot_longer(cols = c("min", "median", "max"),
                                names_to = "type", values_to = "avg_positive") %>%
            mutate(avg_positive = as.numeric(avg_positive))
        } else {
          benchmark_df <- NULL
        }
      } else {
        benchmark_df <- NULL
      }
      
      
      combined_df <- bind_rows(main_df, benchmark_df) %>%
        mutate(year = as.character(year))
      
      valid_y <- combined_df$avg_positive[!is.na(combined_df$avg_positive)]
      
      # ✅ If no valid y-values, show a message instead of a graph
      if (length(valid_y) == 0) {
        return(plot_ly() %>%
                 layout(
                   title = list(
                     text = "No data found for this selection",
                     x = 0.5,
                     xanchor = "center"
                   ),
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE)
                 ))
      }
      
      y_min <- max(0, min(valid_y) - 2)
      y_max <- min(100, max(valid_y) + 2)
      
      
      colors <- c(
        "Your Selection" = "#4472c4",
        "min" = "#C00000",
        "max" = "#70AD47",
        "median" = "#ED7D31"
      )
      
      p <- plot_ly()
      
      for (line_type in unique(combined_df$type)) {
        line_data <- combined_df %>% filter(type == line_type)
        p <- add_trace(
          p,
          data = line_data,
          x = ~year,
          y = ~avg_positive,
          type = "scatter",
          mode = "lines+markers",
          name = tools::toTitleCase(line_type),
          line = list(color = colors[[line_type]] %||% "#999999"),
          marker = list(color = colors[[line_type]]),  # 👈 This ensures dot matches line
          text = ~paste0(round(avg_positive, 1), "%"),
          hoverinfo = "text+x"
        )
      }
      
      p <- layout(
        p,
        title = list(
          text = "Average % Positive Responses for Questions Below",
          x = 0,
          xanchor = "left"
        ),
        xaxis = list(title = "", showgrid = FALSE),
        yaxis = list(
          title = "",
          tickvals = seq(y_min, y_max, by = 5),
          ticktext = paste0(seq(round(y_min,0), round(y_max,0), by = 5), "%"),
          range = c(y_min, y_max),
          showgrid = FALSE
        ),
        legend = list(orientation = "h", x = 0, y = -0.2)
      )
      
      return(p)
    })
  }
  
  
  render_theme_description <- function(tab_name) {
    renderUI({
      req(
        input$org_select,
        input$staff_group_select,
        input$select_theme,
        input$theme_tabs == tab_name
      )
      
      HTML(paste0(
        "<p><strong>What is this graph telling me?</strong> This graph shows the average of the questions below by year for <em>",
        input$theme_tabs, " - ", input$select_theme,
        "</em> within <em>", input$org_select,
        if (!is.null(input$team_select) && input$team_select != "All") {
          paste0(" – ", get_team_simple(input$team_select))
        } else if (!is.null(input$directorate_select) && input$staff_group_select == "Directorate") {
          paste0(" – ", input$directorate_select)
        } else if (!is.null(input$generic_subgroup_select)) {
          paste0(" – ", input$generic_subgroup_select)
        } else {
          ""
        }
      ))
    })
  }
  
  
  render_barometer_description <- function(tab_name) {
    renderUI({
      req(
        input$org_select,
        input$staff_group_select,
        input$team_select,
        input$select_theme,
        input$theme_tabs == tab_name
      )
      
      HTML(paste0(
        "<p><strong>What is this graph telling me?</strong> This graph shows where your current selection falls between the highest and lowest performing <em>",
        if (input$directorate && input$team_select != "All") {
          paste0(" – ", get_team_simple(input$team_select))
        } else if (!is.null(input$directorate_select) && input$staff_group_select == "Directorate") {
          paste0(" – ", input$directorate_select)
        } else if (!is.null(input$generic_subgroup_select)) {
          paste0(" – ", input$generic_subgroup_select)
        } else {
          ""
        }
      ))
    })
  }
  
  
  render_benchmark_description <- function(tab_name) {
    renderUI({
      req(
        input$org_select,
        input$staff_group_select,
        input$select_theme,
        input$theme_tabs == tab_name
      )
      
      # Build the "current selection" string (no italics)
      selection_text <- paste0(
        input$theme_tabs, " - ", input$select_theme,
        " for ", input$org_select,
        if (!is.null(input$team_select) && input$team_select != "All") {
          paste0(" – ", get_team_simple(input$team_select))
        } else if (!is.null(input$directorate_select) && input$staff_group_select == "Directorate") {
          paste0(" – ", input$directorate_select)
        } else if (!is.null(input$generic_subgroup_select)) {
          paste0(" – ", input$generic_subgroup_select)
        } else {
          ""
        }
      )
      
      # Determine benchmark population (no italics)
      benchmark_group <- if (input$staff_group_select == "Directorate" &&
                             !is.null(input$team_select) && input$team_select != "All") {
        if (!is.null(input$directorate_select)) {
          paste0("teams in ", input$directorate_select)
        } else {
          "teams in the selected directorate"
        }
      } else {
        "NHS Trusts"
      }
      
      # Final HTML with colored words
      HTML(paste0(
        "<p><strong>What is this graph telling me?</strong> This graph shows where ",
        selection_text,
        " falls between the <span style='color:#C00000;'>lowest</span> and <span style='color:#4472c4;'>highest</span> performing ",
        benchmark_group, ".</p>"
      ))
    })
  }
  
  
  
  
  
  render_benchmark_gauge <- function(tab_name, output_id) {
    cat("🟢 Gauge function triggered for:", tab_name, "\n")
    output[[output_id]] <- renderPlot({
      req(input$org_select, input$staff_group_select, input$select_theme, input$theme_tabs == tab_name)
      
      # Determine group + subgroup
      if (input$staff_group_select == "Directorate") {
        if (!is.null(input$team_select) && input$team_select != "All") {
          filter_group <- "Team"
          filter_subgroup <- input$team_select
        } else {
          filter_group <- "Directorate"
          filter_subgroup <- input$directorate_select
        }
      } else if (input$staff_group_select == "Organisation") {
        filter_group <- "Organisation"
        filter_subgroup <- NULL
      } else {
        filter_group <- input$staff_group_select
        filter_subgroup <- input$generic_subgroup_select
      }
      
      org_info <- dbGetQuery(con, "
      SELECT ods_code, benchmarking_group
      FROM nhs_orgs
      WHERE organisation = $1
      LIMIT 1
    ", params = list(input$org_select))
      req(nrow(org_info) == 1)
      
      ods_code <- org_info$ods_code
      bench_group <- org_info$benchmarking_group
      latest_year <- dbGetQuery(con, "SELECT MAX(year) AS year FROM nhs_ss_all_bench")$year
      
      # Get selection
      selection_query <- "
      SELECT AVG(percent_positive) AS avg
      FROM nhs_ss_percent_positive r
      JOIN nhs_ss_themes t ON r.q_id = t.id
      WHERE year = $1
        AND ods_code = $2
        AND t.theme = $3
        AND t.domain = $4
    "
      if (!is.null(filter_subgroup)) {
        selection_query <- paste0(selection_query, " AND r.\"group\" = $5 AND r.subgroup = $6")
        selection <- dbGetQuery(con, selection_query,
                                params = list(latest_year, ods_code, input$theme_tabs, input$select_theme, filter_group, filter_subgroup))$avg
      } else {
        selection_query <- paste0(selection_query, " AND r.\"group\" = $5")
        selection <- dbGetQuery(con, selection_query,
                                params = list(latest_year, ods_code, input$theme_tabs, input$select_theme, filter_group))$avg
      }
      req(length(selection) == 1 && !is.na(selection))
      
      # Determine benchmark group
      benchmark_type <- if (filter_group == "Team" && !is.null(filter_subgroup)) {
        dir <- dbGetQuery(con, "
        SELECT directorate
        FROM oxleas_esr_teams
        WHERE team = $1
        LIMIT 1
      ", params = list(filter_subgroup))$directorate
        req(length(dir) == 1)
        dir
      } else {
        bench_group
      }
      
      # Adjust benchmark query to optionally filter by subdomain IS NULL
      subdomain_clause <- if (tab_name %in% c("People's Promise", "Oxleas Values")) {
        "AND subdomain IS NULL"
      } else {
        ""
      }
      
      bench_query <- sprintf("
      SELECT stat_type, value
      FROM nhs_ss_all_bench
      WHERE year = $1
        AND theme = $2
        AND domain = $3
        AND benchmarking_group = $4
        %s
    ", subdomain_clause)
      
      benchmark <- dbGetQuery(con, bench_query,
                              params = list(latest_year, input$theme_tabs, input$select_theme, benchmark_type)) %>%
        tidyr::pivot_wider(names_from = stat_type, values_from = value)
      
      req(all(c("min", "max") %in% names(benchmark)))
      
      min_val <- benchmark$min
      max_val <- benchmark$max
      
      min_val <- as.numeric(benchmark$min[[1]])
      max_val <- as.numeric(benchmark$max[[1]])
      selection <- as.numeric(selection[[1]])
      
      value <- ((as.numeric(selection) - as.numeric(min_val)) / 
                  (as.numeric(max_val) - as.numeric(min_val))) * 100
      value <- min(max(value, 0), 100)
      
      # Optional debug
      print(paste("selection:", selection, 
                  "min:", min_val, 
                  "max:", max_val, 
                  "computed value:", value))
      
      # Build gauge
      gauge_data <- tibble(
        start_val = c(0, 25, 50, 75),
        end_val   = c(25, 50, 75, 100),
        fill      = c("#4472c4", "#70AD47", "#ED7D31", "#C00000")
      ) %>%
        mutate(
          start_angle = value_to_angle(end_val),
          end_angle = value_to_angle(start_val),
          x0 = 0, y0 = 0, r0 = 0.7, r = 1
        )
      
      needle_angle <- value_to_angle(value - 50)
      needle_df <- tibble(
        x = c(0, cos(needle_angle)),
        y = c(0, sin(needle_angle))
      )
      
      ggplot() +
        geom_arc_bar(
          data = gauge_data,
          aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = start_angle, end = end_angle, fill = fill),
          color = "black"
        ) +
        geom_segment(
          data = needle_df,
          aes(x = x[1], y = y[1], xend = x[2], yend = y[2]),
          color = "black", linewidth = 1.2,
          arrow = arrow(length = unit(0.03, "npc"))
        ) +
        geom_point(aes(0, 0), size = 3, color = "black") +
        coord_fixed() +
        scale_fill_identity() +
        theme_void() +
        theme(panel.background = element_blank())
    })
  }
  
  
  
  render_theme_table <- function(tab_name) {
    renderDataTable({
      req(
        input$org_select,
        input$staff_group_select,
        input$select_theme,
        input$theme_tabs == tab_name
      )
      
      # Determine filters
      if (input$staff_group_select == "Directorate") {
        if (!is.null(input$team_select) && input$team_select != "All") {
          filter_group <- "Team"
          filter_subgroup <- input$team_select
        } else {
          filter_group <- "Directorate"
          filter_subgroup <- input$directorate_select
        }
      } else if (input$staff_group_select == "Organisation") {
        filter_group <- "Organisation"
        filter_subgroup <- NULL
      } else {
        filter_group <- input$staff_group_select
        filter_subgroup <- input$generic_subgroup_select
      }
      
      req(!is.null(filter_group))
      
      ods_code <- dbGetQuery(con, "
      SELECT ods_code FROM nhs_orgs WHERE organisation = $1 LIMIT 1
    ", params = list(input$org_select))$ods_code
      req(length(ods_code) == 1)
      
      # Subgroup clause
      subgroup_clause <- if (!is.null(filter_subgroup)) glue_sql("AND r.subgroup = {filter_subgroup}", .con = con) else SQL("")
      
      # SQL query
      query <- glue_sql("
      SELECT *
      FROM crosstab(
        $$
          SELECT
            re.q_per_positive AS Question,
            r.year,
            AVG(r.percent_positive)::numeric(5,2)
          FROM nhs_ss_percent_positive r
          JOIN nhs_ss_themes t ON r.q_id = t.id
          JOIN nhs_ss_questions q ON q.q_id = r.q_id
          JOIN nhs_ss_responses re ON re.q_id = r.q_id
          WHERE t.theme = {tab_name}
            AND t.domain = {input$select_theme}
            AND r.\"group\" = {filter_group}
            {subgroup_clause}
            AND r.ods_code = {ods_code}
          GROUP BY re.q_per_positive, r.year
          ORDER BY re.q_per_positive, r.year
        $$,
        $$ SELECT DISTINCT year FROM nhs_ss_percent_positive ORDER BY year $$
      ) AS ct (
        Question TEXT,
        \"2021\" NUMERIC,
        \"2022\" NUMERIC,
        \"2023\" NUMERIC,
        \"2024\" NUMERIC
      );
    ", .con = con)
      
      df <- dbGetQuery(con, query)
      
      df <- df |> 
        dplyr::mutate(across(
          .cols = c("2021", "2022", "2023", "2024"),
          .fns = ~ ifelse(is.na(.), "", paste0(round(., 1), "%"))
        ))
      
      DT::datatable(df, options = list(pageLength = 10))
    })
  }
  
  
  render_theme_plot("NHS IMPACT", "impact_trend")
  render_theme_plot("People's Promise", "promise_trend")
  render_theme_plot("Oxleas Values", "values_trend")
  render_theme_plot("Patient Safety", "ps_trend")
  render_theme_plot("Other Themes", "other_trend")
  
  output$impact_table <- render_theme_table("NHS IMPACT")
  output$promise_table <- render_theme_table("People's Promise")
  output$values_table <- render_theme_table("Oxleas Values")
  output$ps_table <- render_theme_table("Patient Safety")
  output$other_table <- render_theme_table("Other Themes")
  
  render_benchmark_gauge("NHS IMPACT", "impact_gauge")
  render_benchmark_gauge("People's Promise", "promise_gauge")
  render_benchmark_gauge("Oxleas Values", "values_gauge")
  render_benchmark_gauge("Patient Safety", "ps_gauge")
  render_benchmark_gauge("Other Themes", "other_gauge")
  
  output$impact_plot_description <- render_theme_description("NHS IMPACT")
  output$promise_plot_description <- render_theme_description("People's Promise")
  output$values_plot_description <- render_theme_description("Oxleas Values")
  output$ps_plot_description     <- render_theme_description("Patient Safety")
  output$other_plot_description <- render_theme_description("Other Themes")
  
  output$impact_gauge_description  <- render_benchmark_description("NHS IMPACT")
  output$promise_gauge_description <- render_benchmark_description("People's Promise")
  output$values_gauge_description  <- render_benchmark_description("Oxleas Values")
  output$ps_gauge_description      <- render_benchmark_description("Patient Safety")
  output$other_gauge_description <- render_benchmark_description("Other Themes")
  
  
}
