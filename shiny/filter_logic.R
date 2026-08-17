
get_team_aliases <- function(directorate_sel, team_sel = NULL) {
  team_map <- get_ox_teams_map() %>%
    filter(directorate == directorate_sel)

  if (!is.null(team_sel) && team_sel != "All") {
    team_map <- team_map %>%
      filter(team_full == team_sel | team_short == team_sel)
  }

  c(team_map$team_full, team_map$team_short) %>%
    na.omit() %>%
    unique()
}




get_selected_theme_ids <- function(theme_sel, domain_sel, subdomain_sel = "All") {
  tqm <- get_theme_questions_map() %>%
    mutate(subdomain = coalesce(subdomain, ""))

  if (is.null(subdomain_sel) || subdomain_sel == "All") {
    domain_rows <- tqm %>%
      filter(theme == theme_sel, domain == domain_sel)

    blank_row <- domain_rows %>%
      filter(subdomain == "") %>%
      pull(theme_id) %>%
      unique()

    if (length(blank_row) > 0) {
      blank_row
    } else {
      domain_rows %>%
        pull(theme_id) %>%
        unique()
    }
  } else {
    tqm %>%
      filter(
        theme == theme_sel,
        domain == domain_sel,
        subdomain == subdomain_sel
      ) %>%
      pull(theme_id) %>%
      unique()
  }
}



apply_subdomain_filter <- function(df, subdomain_sel = "All") {
  if (!is.null(subdomain_sel) && subdomain_sel != "All") {
    return(df %>% filter(subdomain == subdomain_sel))
  }

  has_blank_subdomain <- df %>%
    filter(is.na(subdomain) | subdomain == "" | subdomain == "No subdomain") %>%
    nrow() > 0

  if (has_blank_subdomain) {
    df %>%
      filter(is.na(subdomain) | subdomain == "" | subdomain == "No subdomain")
  } else {
    df
  }
}



apply_family_filter <- function(df,
                                filter_family,
                                directorate = NULL,
                                team = NULL,
                                protected_dim = NULL,
                                protected_value = NULL,
                                professional_dim = NULL,
                                professional_value = NULL,
                                comparison = FALSE) {

  if (filter_family == "Organisational Structure") {
    if (!is.null(team) && team != "All") {
      if (comparison) {
        valid_team_names <- get_team_aliases(directorate)
        return(df %>% filter(dim == "Team", dim_sub %in% valid_team_names))
      } else {
        selected_team_aliases <- get_team_aliases(directorate, team)
        return(df %>% filter(dim == "Team", dim_sub %in% selected_team_aliases))
      }
    }

    if (!is.null(directorate) && directorate != "All") {
      selected_directorate <- directorate

      if (comparison) {
        return(df %>% filter(dim == "Directorate"))
      } else {
        return(df %>% filter(
          dim == "Directorate",
          dim_sub == .env$selected_directorate
        ))
      }
    }

    return(df)
  }

  if (filter_family == "Protected Characteristics") {
    df <- df %>% filter(dim == protected_dim)

    if (!comparison && !is.null(protected_value) && protected_value != "All") {
      df <- df %>% filter(dim_sub == protected_value)
    }

    return(df)
  }

  if (filter_family == "Professional Groups") {
    df <- df %>% filter(dim == professional_dim)

    if (!comparison && !is.null(professional_value) && professional_value != "All") {
      df <- df %>% filter(dim_sub == professional_value)
    }

    return(df)
  }

  df
}


