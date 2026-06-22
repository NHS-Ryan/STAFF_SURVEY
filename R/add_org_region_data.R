# Function to add in region & type data from ODS to nat_result_scores &
# nat_result_themes

add_org_region_data <- function(files) {

  region_lookup <- files$trust_lookup %>%
    dplyr::select(org_id, region_name) %>%
    dplyr::distinct(org_id, .keep_all = TRUE) %>%
    dplyr::mutate(
      region_name = stringr::str_remove(region_name, " COMMISSIONING REGION$")
    )

  org_type_lookup <- dplyr::bind_rows(
    files$nat_result_themes %>%
      dplyr::select(org_id, org_type_reporting_name),
    files$nat_result_scores %>%
      dplyr::select(org_id, org_type_reporting_name)
  ) %>%
    dplyr::filter(
      !is.na(org_id),
      org_id != "",
      !is.na(org_type_reporting_name),
      org_type_reporting_name != ""
    ) %>%
    dplyr::distinct(org_id, .keep_all = TRUE)

  add_region_and_type <- function(df) {
    df %>%
      dplyr::select(-dplyr::any_of("region_name")) %>%
      dplyr::left_join(
        region_lookup,
        by = "org_id"
      ) %>%
      dplyr::left_join(
        org_type_lookup,
        by = "org_id",
        suffix = c("", "_lookup")
      ) %>%
      dplyr::mutate(
        org_type_reporting_name = dplyr::coalesce(
          org_type_reporting_name,
          org_type_reporting_name_lookup
        )
      ) %>%
      dplyr::select(-org_type_reporting_name_lookup)
  }

  files$nat_result_themes <- add_region_and_type(files$nat_result_themes)
  files$nat_result_scores <- add_region_and_type(files$nat_result_scores)

  files
}
