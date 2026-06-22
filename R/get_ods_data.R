get_ods_data <- function(files) {

  base <- "https://directory.spineservices.nhs.uk/ORD/2-0-0"

  org_cache <- new.env(parent = emptyenv())

  get_org <- function(code) {
    if (exists(code, envir = org_cache, inherits = FALSE)) {
      return(get(code, envir = org_cache))
    }

    Sys.sleep(0.25)

    out <- httr2::request(paste0(base, "/organisations/", code)) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()

    assign(code, out, envir = org_cache)

    out
  }

  `%||%` <- function(x, y) {
    if (is.null(x) || length(x) == 0) y else x
  }

  first_or_na <- function(x) {
    if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])
  }

  scalarise <- function(x) {
    if (is.null(x)) {
      NA_character_
    } else if (is.list(x)) {
      paste(unlist(x, use.names = TRUE), collapse = " | ")
    } else {
      as.character(x)[1]
    }
  }

  list_to_row <- function(x) {
    tibble::as_tibble_row(purrr::map(x, scalarise))
  }

  ensure_cols <- function(df, cols) {
    for (col in cols) {
      if (!col %in% names(df)) {
        df[[col]] <- NA_character_
      }
    }

    df
  }

  pick_field <- function(df, exact = character(), pattern = NULL) {
    exact_hit <- intersect(exact, names(df))

    if (length(exact_hit) > 0) {
      return(as.character(df[[exact_hit[1]]][1]))
    }

    if (!is.null(pattern)) {
      pattern_hit <- names(df)[stringr::str_detect(names(df), pattern)]

      if (length(pattern_hit) > 0) {
        return(as.character(df[[pattern_hit[1]]][1]))
      }
    }

    NA_character_
  }

  parse_target <- function(target_chr) {
    x <- stringr::str_split_fixed(target_chr, "\\s*\\|\\s*", 5)

    tibble::tibble(
      target_root = x[, 1],
      target_authority = x[, 2],
      target_code = x[, 3],
      target_role_id = x[, 4],
      target_unique_role_id = x[, 5]
    )
  }

  get_roles_tbl <- function(org) {
    purrr::map_dfr(org$Roles$Role %||% list(), list_to_row) %>%
      ensure_cols(c("id", "uniqueRoleId", "primaryRole", "Date", "Status"))
  }

  get_rels_tbl <- function(org) {
    rels <- purrr::map_dfr(org$Rels$Rel %||% list(), list_to_row) %>%
      ensure_cols(c("id", "uniqueRelId", "Date", "Status", "Target"))

    if (nrow(rels) == 0 || all(is.na(rels$Target))) {
      return(
        tibble::tibble(
          id = character(),
          uniqueRelId = character(),
          Date = character(),
          Status = character(),
          Target = character(),
          target_root = character(),
          target_authority = character(),
          target_code = character(),
          target_role_id = character(),
          target_unique_role_id = character()
        )
      )
    }

    rels %>%
      dplyr::bind_cols(parse_target(.$Target))
  }

  get_roles_lookup <- function() {
    roles_raw <- httr2::request(paste0(base, "/roles")) %>%
      httr2::req_perform() %>%
      httr2::resp_body_json()

    role_items <- roles_raw$Roles$Role %||%
      roles_raw$roles$role %||%
      roles_raw$Roles %||%
      roles_raw$roles %||%
      list()

    purrr::map_dfr(role_items, function(x) {
      row <- list_to_row(x)

      tibble::tibble(
        role_id = pick_field(
          row,
          exact = c("id", "Id", "role_id", "roleId", "RoleId"),
          pattern = "(^|\\.)id$|role.*id"
        ),
        role_name = pick_field(
          row,
          exact = c("Name", "name", "role_name", "roleName", "RoleName", "display", "Display"),
          pattern = "name|display"
        )
      )
    }) %>%
      dplyr::filter(!is.na(role_id)) %>%
      dplyr::distinct(role_id, .keep_all = TRUE)
  }

  roles_lookup <- get_roles_lookup()

  extract_trust_lookup_clean <- function(code) {
    trust_org <- get_org(code)$Organisation

    trust_roles <- get_roles_tbl(trust_org)
    trust_rels  <- get_rels_tbl(trust_org)

    active_trust_rels <- trust_rels %>%
      dplyr::filter(!is.na(Status), Status == "Active")

    # --------------------
    # ICB
    # --------------------
    icb_code <- active_trust_rels %>%
      dplyr::filter(target_role_id == "RO261") %>%
      dplyr::pull(target_code) %>%
      first_or_na()

    icb_name <- if (!is.na(icb_code)) {
      get_org(icb_code)$Organisation$Name
    } else {
      NA_character_
    }

    # --------------------
    # Region
    # --------------------
    region_name <- NA_character_

    if (!is.na(icb_code)) {
      icb_org <- get_org(icb_code)$Organisation

      region_code <- get_rels_tbl(icb_org) %>%
        dplyr::filter(!is.na(Status), Status == "Active", target_role_id == "RO209") %>%
        dplyr::pull(target_code) %>%
        first_or_na()

      if (!is.na(region_code)) {
        region_name <- get_org(region_code)$Organisation$Name
      }
    }

    # --------------------
    # ODS primary role / type
    # --------------------
    primary_role_id <- trust_roles %>%
      dplyr::filter(primaryRole == "TRUE") %>%
      dplyr::pull(id) %>%
      first_or_na()

    ods_type <- roles_lookup %>%
      dplyr::filter(role_id == primary_role_id) %>%
      dplyr::pull(role_name) %>%
      first_or_na()

    tibble::tibble(
      org_id = trust_org$OrgId$extension,
      org_name = trust_org$Name,
      status = trust_org$Status,
      type = ods_type,
      icb_name = icb_name,
      region_name = region_name
    )
  }

  codes <- files$nat_result_themes %>%
    dplyr::distinct(org_id) %>%
    dplyr::filter(!is.na(org_id), org_id != "") %>%
    dplyr::pull(org_id)

  trust_lookup <- purrr::map_dfr(codes, function(code) {
    tryCatch(
      extract_trust_lookup_clean(code),
      error = function(e) {
        tibble::tibble(
          org_id = code,
          org_name = NA_character_,
          status = NA_character_,
          type = NA_character_,
          icb_name = NA_character_,
          region_name = NA_character_
        )
      }
    )
  }) %>%
    dplyr::filter(type == "NHS TRUST")

  files$trust_lookup <- trust_lookup

  files
}
