#' Get initial data about query for "narrow" tables (with no lookup table)
#' @keywords internal
return_narrow_table_info <- function(lookup, lookup_year, rs = NULL) {
  fn <- "return_narrow_table_info"
  ul <- toupper(lookup)
  rs <- ifnull(rs, "NC")
  
  assert_that(
    lookup != "oa",
    msg = paste0(
      "You can't download an OA lookup table without supplying `within_level`.",
      "\nExample: `lookup('oa', within_level = 'msoa', 'Tendring 001')`"
    )
  )
  s1 <- opengeo_schema |>
    dplyr::filter(if_any("service_name", \(x) gregg(x, "^{ul}.*_{rs}"))) |>
    janitor::remove_empty("cols")
  assert_that(nrow(s1) > 0, msg = no_table_msg(fn))
  s1_years <- as.numeric(stringr::str_extract(s1[["service_name"]], "\\d{4}"))
  lookup_year <- ifnull(lookup_year, max(s1_years))
  lu_code_field <- return_field_code(lookup, cd_colnames(s1), lookup_year, fn)
  assert_that(!is.null(lu_code_field), msg = no_lu_field_msg(fn))
  
  s2 <- dplyr::filter(s1, !if_any(any_of(lu_code_field), is.na)) |>
    janitor::remove_empty("cols")
  
  if (is_interactive()) cli_alert_info("Using {.val {lu_code_field}}")
  list(
    schema = s2,
    lookup_code = lu_code_field,
    within_code = NULL
  )
}

#' Get initial data about query
#' @keywords internal
return_lookup_table_info <- function(
    lookup,
    within_level,
    lookup_year,
    within_year,
    joinable) {
  fn <- "return_lookup_table_info"
  ul <- toupper(lookup)
  
  s1 <- opengeo_schema |>
    dplyr::filter(if_any("service_name", \(x) gregg(x, "{ul}.*_LU"))) |>
    janitor::remove_empty("cols")
  s1_names <- cd_colnames(s1)
    
  if (joinable) {
    rx <- res_codes_regex()
    sp <- opengeo_schema |>
      dplyr::filter(if_any("service_name", \(x) gregg(x, "^{ul}.*_{rx}"))) |>
      janitor::remove_empty("cols")
    assert_that(nrow(sp) > 0, msg = no_table_msg(fn))
    s1_names <- intersect(s1_names, cd_colnames(sp))
  }
    
  lu_code_field <- return_field_code(lookup, s1_names, lookup_year)
  assert_that(!is.null(lu_code_field), msg = no_lu_field_msg(fn))
  s2 <- dplyr::filter(s1, !if_any(any_of(lu_code_field), is.na)) |>
    janitor::remove_empty("cols")
  assert_that(nrow(s2) > 0, msg = no_table_msg(fn))
  
  wn_code_field <- return_field_code(within_level, cd_colnames(s2), within_year)
  if (is_interactive()) {
    cli_alert_info("Using {.val {lu_code_field}}, {.val {wn_code_field}}")
  }
  list(
    schema = s2,
    lookup_code = lu_code_field,
    within_code = wn_code_field
  )
}
# Helper functions --------------------------------

#' This is such a crucial function to the whole package! But so simple.
#' @keywords internal
build_flat_query <- function(var, vec) {
  y <- stringr::str_flatten(glue("'{unique(vec)}'"), collapse = ",")
  glue("{var} IN ({y})")
}

#' @keywords internal
build_where_list <- function(var, vec) {
  unique(vec) |>
    batch_it(50L) |> # turns out this limit is rather crucial!
    purrr::map(\(x) build_flat_query(var, x))
}

#' @keywords internal
cd_colnames <- \(x) colnames(dplyr::select(x, ends_with("cd")))

#' @keywords internal
return_field_code <- function(x, names_vec, year = NULL, fn = NULL) {
  fn <- ifnull(fn, "return_lookup_table_info")
  if (is.null(x) || is.na(x)) return(NULL)
  if (is.null(year)) {
    y2 <- names_vec |>
      stringr::str_subset(glue("(?<=^{x})\\d+")) |>
      stringr::str_extract("\\d{2}(?=cd$)") |>
      as.numeric()
    year <- if_else(y2 > 30, y2 + 1900, y2 + 2000) # Will need updating in 2031!
    year <- max(year, na.rm = TRUE) # should return most recent year by default
  }
  y2 <- stringr::str_extract(year, "\\d{2}$") # or `as.numeric(year) %% 100`
  field_code <- first(stringr::str_subset(names_vec, glue("^{x}{y2}cd$")))

  assert_that(
    length(field_code) == 1 && !is.na(field_code),
    msg = glue(
      "{.fn {fn}}: That combination of levels and year has not returned a ",
      "result.\nPerhaps try a different year?"
    )
  )
  field_code
}

no_table_msg <- \(fn) {
  glue(
    "{.fn {fn}}: no relevant lookup tables for `lookup` found in schema.\n",
    "Try a different lookup year?"
  )
}
no_lu_field_msg <- \(fn) glue("{.fn {fn}}: No suitable lookup field found.")
