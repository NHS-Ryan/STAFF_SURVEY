# Helper to allow iteration over various test cases
expect_comparison_matches_question_table <- function(theme_sel,
                                                     domain_sel,
                                                     subdomain_sel = "All",
                                                     benchmark_view,
                                                     directorate = NULL,
                                                     team = NULL,
                                                     year = NULL) {

  comp <- build_comparison_df(
    theme_sel = theme_sel,
    domain_sel = domain_sel,
    subdomain_sel = subdomain_sel,
    benchmark_view = benchmark_view,
    filter_family = "Organisational Structure",
    directorate = directorate,
    team = team,
    trust_sel = "Oxleas NHS Foundation Trust",
    score = "score"
  )

  ques <- build_question_table_df(
    trust_sel = "Oxleas NHS Foundation Trust",
    theme_sel = theme_sel,
    domain_sel = domain_sel,
    subdomain_sel = subdomain_sel,
    filter_family = "Organisational Structure",
    directorate = directorate,
    team = team,
    score = "score"
  )

  selected_comp <- comp %>%
    dplyr::filter(year == !!year, selected)

  question_score <- mean(ques[[as.character(year)]], na.rm = TRUE)

  print(selected_comp)

  cat(
    "\nTest case:",
    theme_sel, "|",
    domain_sel, "|",
    subdomain_sel, "|",
    "directorate:", directorate, "|",
    "team:", team, "|",
    "year:", year, "\n"
  )

  cat("Comparison score:", selected_comp$score[[1]], "\n")
  cat("Question table mean:", question_score, "\n")
  cat("Difference:", selected_comp$score[[1]] - question_score, "\n\n")

  expect_equal(nrow(selected_comp), 1)

  expect_equal(
    round(selected_comp$score[[1]] * 100, 1),
    round(question_score * 100, 1),
    info = paste(
      "Mismatch for",
      theme_sel,
      "|",
      domain_sel,
      "|",
      subdomain_sel,
      "| year",
      year
    )
  )

}


test_that("comparison selected scores match question table averages", {

  cases <- list(
    list(
      theme_sel = "People's Promise",
      domain_sel = "We are a team",
      subdomain_sel = "All",
      benchmark_view = "directorate_other",
      directorate = "Adult Acute & Crisis Mental Health (L3)",
      team = NULL,
      year = 2025
    ),
    list(
      theme_sel = "People's Promise",
      domain_sel = "We are a team",
      subdomain_sel = "Line management",
      benchmark_view = "directorate_other",
      directorate = "Adult Acute & Crisis Mental Health (L3)",
      team = NULL,
      year = 2025
    ),
    list(
      theme_sel = "Patient Safety",
      domain_sel = "Raising Concerns",
      subdomain_sel = "All",
      benchmark_view = "trust_region_type",
      directorate = "All",
      team = NULL,
      year = 2025
    ),
    list(
      theme_sel = "Patient Safety",
      domain_sel = "Raising Concerns",
      subdomain_sel = "All",
      benchmark_view = "trust_region_type",
      directorate = "All",
      team = NULL,
      year = 2025
    ),
    list(
      theme_sel = "NHS IMPACT",
      domain_sel = "Using Quality Management System",
      subdomain_sel = "All",
      benchmark_view = "team_trust",
      directorate = "Offender Healthcare (L3)",
      team = "Bristol Mh, Pharm, Primary Care & Mgmt & Admin",
      year = 2025
    ),
    list(
      theme_sel = "Oxleas Values",
      domain_sel = "We are Kind",
      subdomain_sel = "All",
      benchmark_view = "team_trust",
      directorate = "All",
      team = "All",
      year = 2025
    ),
    list(
      theme_sel = "Other",
      domain_sel = "Engagement",
      subdomain_sel = "All",
      benchmark_view = "team_trust",
      directorate = "All",
      team = "All",
      year = 2025
    ),
    list(
      theme_sel = "Other",
      domain_sel = "Morale",
      subdomain_sel = "Stressors",
      benchmark_view = "team_trust",
      directorate = "All",
      team = "All",
      year = 2025
    ),
    list(
      theme_sel = "Other",
      domain_sel = "Morale",
      subdomain_sel = "Work pressure",
      benchmark_view = "team_trust",
      directorate = "All",
      team = "All",
      year = 2025
    ),
    list(
      theme_sel = "Oxleas Values",
      domain_sel = "We are Fair",
      subdomain_sel = "All",
      benchmark_view = "team_trust",
      directorate = "All",
      team = "All",
      year = 2025
    ),
    list(
      theme_sel = "Oxleas Values",
      domain_sel = "We are Fair",
      subdomain_sel = "We listen",
      benchmark_view = "team_trust",
      directorate = "All",
      team = "All",
      year = 2025
    ),
    list(
      theme_sel = "Patient Safety",
      domain_sel = "Personal Safety",
      subdomain_sel = "All",
      benchmark_view = "team_trust",
      directorate = "All",
      team = "All",
      year = 2025
    ),
    list(
      theme_sel = "People's Promise",
      domain_sel = "We are a team",
      subdomain_sel = "All",
      benchmark_view = "team_trust",
      directorate = "All",
      team = "All",
      year = 2025
    )
  )

  for (case in cases) {
    do.call(expect_comparison_matches_question_table, case)
  }
})
