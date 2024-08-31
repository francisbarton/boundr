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
