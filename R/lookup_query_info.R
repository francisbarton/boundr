#' Get initial data about query for "narrow" tables (with no lookup table)
#'
#' @keywords internal
return_narrow_table_info <- function(lookup_level, lookup_year, rs = NULL) {
  fn <- "return_narrow_table_info"
  ul <- toupper(lookup_level)
  rs <- ifnull(rs, "NC")
  # g1 <- rlang::expr("^{ul}.*_{rs}")

  assert_that(
    lookup_level != "oa",
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
  lu_code_field <- lookup_level |>
    return_field_code(cd_colnames(s1), lookup_year, fn)
  assert_that(!is.null(lu_code_field), msg = no_lu_msg(fn))

  s2 <- dplyr::filter(s1, !if_any(.data[[lu_code_field]], is.na)) |>
    janitor::remove_empty("cols") |>
    rlang::with_options(lifecycle_verbosity = "quiet")

  if (is_interactive()) cli_alert_info("Using {.val {lu_code_field}}")
  list(
    schema = s2,
    lookup_code = lu_code_field,
    within_code = NULL
  )
}

#' Get initial data about query
#'
#' @keywords internal
return_lookup_table_info <- function(
    lookup_level,
    within_level,
    lookup_year,
    within_year,
    joinable) {
  fn <- "return_lookup_table_info"
  ul <- toupper(lookup_level)
  wl <- toupper(within_level)
  repl_empty <- \(x) if (rlang::is_empty(x)) "" else x
  uy <- repl_empty(as.numeric(lookup_year) %% 100)
  wy <- repl_empty(as.numeric(within_year) %% 100)

  s1 <- opengeo_schema |>
    # prioritise tables with "lookup_level" at the start, or nearer to it
    dplyr::filter(
      if_any("service_name", \(x) gregg(x, "{ul}{uy}.*{wl}{wy}.*_LU"))
    ) |>
    dplyr::arrange(nchar(sub(glue("{ul}.*$"), "", .data[["service_name"]]))) |>
    janitor::remove_empty("cols")
  s1_names <- cd_colnames(s1)

  if (joinable) {
    rx <- condense(res_codes())
    sp <- opengeo_schema |>
      dplyr::filter(if_any("service_name", \(x) gregg(x, "^{ul}.*_{rx}"))) |>
      janitor::remove_empty("cols")
    assert_that(nrow(sp) > 0, msg = no_table_msg(fn))
    s1_names <- intersect(s1_names, cd_colnames(sp))
  }

  lu_code_field <- return_field_code(lookup_level, s1_names, lookup_year)
  assert_that(!is.null(lu_code_field), msg = no_lu_msg(fn))
  s2 <- s1 |>
    dplyr::filter(!if_any(.data[[lu_code_field]], is.na)) |>
    janitor::remove_empty("cols") |>
    rlang::with_options(lifecycle_verbosity = "quiet")
  assert_that(nrow(s2) > 0, msg = no_table_msg(fn))

  wn_code_field <- return_field_code(within_level, cd_colnames(s2), within_year)
  s3 <- s2 |>
    dplyr::filter(!if_any(.data[[wn_code_field]], is.na)) |>
    janitor::remove_empty("cols") |>
    rlang::with_options(lifecycle_verbosity = "quiet")
  assert_that(nrow(s3) > 0, msg = no_table_msg(fn))

  if (is_interactive()) {
    cli_alert_info("Using {.val {lu_code_field}}, {.val {wn_code_field}}")
  }
  list(
    schema = s3,
    lookup_code = lu_code_field,
    within_code = wn_code_field
  )
}



#' Just another piece of the pipeline
#'
#' @inheritParams common_spatial
#' @keywords internal
process_query_info <- function(
    query_info,
    within_names,
    within_codes,
    return_width,
    query_opt) {
  schema <- query_info[["schema"]]
  lookup_code_field <- query_info[["lookup_code"]]
  within_code_field <- ifnull(query_info[["within_code"]], lookup_code_field)

  table_options <- schema[["service_name"]]
  if (is.null(query_opt) && length(table_options) > 1 && is_interactive()) {
    cli_alert_info("More than 1 result found:")
    cli::cli_ol(table_options)
    cli_alert_info(c(
      "Using option {.field 1} by default. ",
      "(Change the {.var query_option} parameter to try another data source.)"
    ))
  }
  query_opt <- ifnull(query_opt, 1)

  if (query_opt > length(table_options)) {
    lr <- length(table_options)
    cli_alert_info(c(
      "There is no option {.var {query_opt}}! There are only {.emph {lr}} ",
      "options available. Option {.strong {lr}} will be selected instead."
    ))
    query_opt <- lr
  }
  query_url <- schema[["service_url"]][[query_opt]]

  lookup_name_field <- sub("cd$", "nm", lookup_code_field)
  within_name_field <- sub("cd$", "nm", within_code_field)
  fields <- switch(return_width,
    "tidy" = unique(c(
      lookup_code_field, lookup_name_field, within_code_field, within_name_field
    )),
    "full" = "*",
    "minimal" = c(lookup_code_field, lookup_name_field)
  ) |>
    purrr::discard(\(x) grepl("^oa.+nm$", x)) # OAs don't have names

  if (is.null(within_names) && is.null(within_codes)) {
    where_list <- "1=1" # this means "return all rows"
  } else if (!is.null(within_names)) {
    where_list <- build_where_list(within_name_field, within_names)
  } else { # implied: !is.null(within_codes))
    where_list <- build_where_list(within_code_field, within_codes)
  }

  list(
    query_url = query_url,
    fields = fields,
    where_list = where_list
  )
}


#' This function is the one that handles all actual querying of the API for
#'  `lookup()`. Previous queries in the pipeline have been working "offline"
#'  with just the schema data provided within the package.
#' @keywords internal
process_lookup_query_data <- function(query_data) {
  query_url <- query_data[["query_url"]]
  fields <- query_data[["fields"]]
  where_list <- query_data[["where_list"]]
  ids <- where_list |>
    purrr::map(\(x) return_query_ids(query_url, where_string = x)) |>
    purrr::list_c()
  ids |>
    batch_it(100) |> # Could this go to more than 100? 500?
    purrr::map(
      \(x) return_table_data(x, query_url, fields),
      .progress = if (is_interactive()) "Retrieving table data" else FALSE
    )
}


#' This function is the one that handles all actual querying of the API for
#'  `bounds()`/`centroids()`. Previous queries in the pipeline have been working
#'  "offline" with just the schema data provided within the package.
#' @keywords internal
process_spatial_query_data <- function(query_data, crs) {
  query_url  <- query_data[["query_url"]]
  fields  <- query_data[["fields"]]
  where_list <- query_data[["where_list"]]
  ids <- where_list |>
    purrr::map(\(x) return_query_ids(query_url, where_string = x)) |>
    purrr::list_c()
  ids |>
    batch_it(100L) |>
    purrr::map(
      \(x) return_spatial_data(x, query_url, fields, crs),
      .progress = if (is_interactive()) "Retrieving spatial data" else FALSE
    )
}


#' This function also actually queries the API
#' @keywords internal
return_query_ids <- function(query_url, where_string) {
  fn <- "return_query_ids"
  ids <- unique(return_result_ids(query_url, where = where_string))
  assert_that(
    is.vector(ids),
    !is.list(ids),
    length(ids) > 0,
    is.numeric(ids),
    msg = cli::format_error(
      "{.fn {fn}}: The query has not returned any valid result IDs."
    )
  )
  ids
}


# Helper functions --------------------------------

#' This is such a crucial function to the whole package! But so simple.
#' @keywords internal
build_flat_query <- function(var, vec) {
  fvec <- stringr::str_flatten(glue("'{unique(vec)}'"), collapse = ",")
  glue("{var} IN ({fvec})")
}


#' @keywords internal
build_where_list <- function(var, vec) {
  unique(vec) |>
    batch_it(50L) |> # turns out this limit is rather crucial!
    purrr::map_chr(\(x) build_flat_query(var, x))
}


#' @keywords internal
return_field_code <- function(x, names_vec, year = NULL, fn = NULL) {
  fn <- ifnull(fn, "return_lookup_table_info")
  assert_that(
    length(names_vec) > 0,
    msg = cli::format_error("{.fn {fn}}: No names vector supplied.")
  )
  if (is.null(x) || is.na(x)) return(NULL)
  if (is.null(year)) {
    y2 <- names_vec |>
      stringr::str_subset(glue("(?<=^{x})\\d+")) |>
      stringr::str_extract("\\d{2}(?=cd$)") |>
      as.numeric()
    assert_that(
      length(y2) > 0,
      msg = cli::format_error("{.fn {fn}}: No suitable year could be found.")
    )
    year <- if_else(y2 > 30, y2 + 1900, y2 + 2000) # Will need updating in 2031!
    year <- max(year) # should return most recent year by default
  }
  y2 <- stringr::str_extract(year, "\\d{2}$") # or `as.numeric(year) %% 100`
  field_code <- first(stringr::str_subset(names_vec, glue("^{x}{y2}cd$")))

  assert_that(
    length(field_code) == 1 && !is.na(field_code),
    msg = cli::format_error(c(
      "{.fn {fn}}: That combination of levels and year has not returned a ",
      "result.\nPerhaps try a different year?"
    ))
  )
  field_code
}


no_table_msg <- \(fn) {
  cli::format_error(c(
    "{.fn {fn}}: No relevant lookup tables for `lookup` found in schema.\n",
    "Try a different lookup year?"
  ))
}


no_lu_msg <- \(fn) cli_alert_info("{.fn {fn}}: No suitable lookup field found.")
