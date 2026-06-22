rank_to_val <- function(rank, total) {
  total - rank + 1
}

all_and_sort <- function(x) {
  c("All", sort(unique(na.omit(x))))
}

sort_only <- function(x) {
  sort(unique(na.omit(x)))
}


# Code to make a Gauge Plot aka Barometer
gauge_plot <- function(val, display_val = val) {
  has_val <- !is.null(val) && length(val) == 1 && !is.na(val)

  if (has_val) {
    val <- pmin(pmax(val, 0), 100)
  }

  if (is.null(display_val) || length(display_val) != 1 || is.na(display_val)) {
    display_val <- "'No data'"
  }

  value_to_gauge_angle <- function(value) {
    value <- pmin(pmax(value, 0), 100)
    (pi - (value / 100) * pi) - pi / 2
  }

  value_to_needle_angle <- function(value) {
    value <- pmin(pmax(value, 0), 100)
    pi - (value / 100) * pi
  }

  gauge_data <- tibble(
    start_val = c(0, 25, 50, 75),
    end_val   = c(25, 50, 75, 100),
    fill      = c("#4472c4", "#70AD47", "#ED7D31", "#C00000")
  ) %>%
    mutate(
      start_angle = value_to_gauge_angle(end_val),
      end_angle   = value_to_gauge_angle(start_val),
      x0 = 0,
      y0 = 0,
      r0 = 0.7,
      r = 1
    )

  p <- ggplot() +
    geom_arc_bar(
      data = gauge_data,
      aes(x0 = x0, y0 = y0, r0 = r0, r = r, start = start_angle, end = end_angle, fill = fill),
      colour = "black"
    ) +
    geom_point(aes(0, 0), size = 3, colour = "black") +
    annotate(
      "text",
      x = 0,
      y = -0.28,
      label = display_val,
      size = 11,
      fontface = "bold",
      parse = TRUE
    ) +
    coord_fixed(clip = "off") +
    scale_fill_identity() +
    scale_x_continuous(
      limits = c(-1.05, 1.05),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(-0.75, 1.05),
      expand = c(0, 0)
    ) +
    theme_void() +
    theme(
      panel.background = element_blank(),
      plot.background = element_blank(),
      plot.margin = margin(0, 0, 0, 0)
    )

  if (has_val) {
    needle_angle <- value_to_needle_angle(val)

    needle_df <- tibble(
      x = 0,
      y = 0,
      xend = 0.85 * cos(needle_angle),
      yend = 0.85 * sin(needle_angle)
    )

    p <- p +
      geom_segment(
        data = needle_df,
        aes(x = x, y = y, xend = xend, yend = yend),
        colour = "black",
        linewidth = 1.2,
        arrow = arrow(length = unit(0.03, "npc"))
      )
  }

  p
}


calc_needle <- function(top, bottom, val) {
  ((val - bottom) / (top - bottom)) * 100
}

# Gives a suffix for rank values (1st,2nd, 3rd etc)
ordinal_suffix <- function(n) {
  n <- abs(as.integer(n))

  if ((n %% 100) %in% c(11, 12, 13)) {
    return("th")
  }

  switch(
    as.character(n %% 10),
    "1" = "st",
    "2" = "nd",
    "3" = "rd",
    "th"
  )
}


rank_label <- function(rank, total, label, extra_line = NULL) {
  if (is.na(rank) || is.na(total)) {
    return("'No data'")
  }

  rank_text <- paste0(rank, "^'", ordinal_suffix(rank), "'")
  middle_text <- paste0("scriptstyle('... of ", total, " ", label, "')")

  if (is.null(extra_line) || is.na(extra_line) || extra_line == "") {
    return(
      paste0("atop(", rank_text, ",", middle_text, ")")
    )
  }

  paste0(
    "atop(",
    rank_text,
    ",atop(",
    middle_text,
    ",scriptscriptstyle('", extra_line, "')",
    "))"
  )
}
