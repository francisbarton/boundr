#' Return lookup query URL and lower and higher field codes
#'
#' @param lookup character. Lower level area code eg "lsoa", "wd", "lad".
#'  Equivalent to the `lookup` parameter in `bounds()`.
#' @param within character. Higher level area code eg "lad", "cty". Equivalent
#' to the `within` parameter in `bounds()`.
#' @param lookup_year numeric or character. A specific year for data relating
#'  to parameter `x`, if needed. Defaults to `NULL`, which will return the most
#'  recent data.
#' @param within_year numeric or character. A specific year for data relating
#'  to parameter `y`, if needed. Defaults to `NULL`, which will return the most
#'  recent data.
#' @param country_filter character. Open Geography datasets are sometimes
#'  available just within certain countries. Specify a country code if you want
#'  your results restricted to a certain country only - eg "WA" for Wales, "EW"
#'  for England and Wales. By default returns all options.
#' @param option numeric. Defaults to 1, which means that the URL will just be
#'  the first one from the list of possible services resulting from the level
#'  and year filters above. If this does not give you what you want, you can
#'  run the script again with a different option from the list.
#' @param standalone logical. Whether this query is standalone (`TRUE`) or
#'  forms part of a geospatial query (`FALSE`). In the latter case, this
#'  function will try to return a lookup table that contains the appropriate
#'  columns for spatial data (boundaries or centroids) to be joined onto.
#' @param chatty Boolean. Whether to print feedback on the 'decisions' the
#'  function has taken about which table to query. Default `TRUE` when the
#'  function is run in an interactive session, `FALSE` otherwise.
#' 
#' @keywords internal
#'
#' @returns A list of length 3: the query URL, the lower level (`lookup`) field
#'  code (eg `lsoa11cd`), and the higher level (`within`) field code.
return_lookup_query_info <- function(
    lookup,
    within,
    lookup_year,
    within_year,
    country_filter,
    option,
    standalone,
    chatty
  ) {

  # find spatial tables from the schema
  schema_spatial_names <- opengeo_schema |>
    dplyr::filter(
      if_any("has_geometry") &
      if_any("service_name", \(x) stringr::str_detect(x, country_filter))
    ) |>
    janitor::remove_empty("cols") |>
    dplyr::select(ends_with("cd")) |>
    names()

  # filter only lookup tables from the schema
  # and those with the right country filter
  schema_lookups <- opengeo_schema |>
    dplyr::filter(
      !if_any("has_geometry") &
      if_any("service_name", \(x) stringr::str_detect(x, country_filter))
    ) |>
    janitor::remove_empty("cols")

  assert_that(
    nrow(schema_lookups) > 0,
    msg = "return_lookup_query_info: no lookup tables found."
  )

  # make list of codes used in lookups
  schema_names <- schema_lookups |>
    dplyr::select(ends_with("cd")) |>
    names()

  if (!standalone) schema_names <- intersect(schema_names, schema_spatial_names)

  lookup_field <- return_field_code(lookup, lookup_year, schema_names)

  # reduce schema to only those matching lookup_field
  schema2 <- schema_lookups |>
    dplyr::filter(!if_any({{ lookup_field }}, is.na)) |>
    janitor::remove_empty("cols")

  schema2_names <- schema2 |>
    dplyr::select(ends_with("cd")) |>
    names()

  within_field <- return_field_code(within, within_year, schema2_names)

  if (chatty) {
    cli_alert_info("Using codes {lookup_field}, {within_field}")
  }


  results_0 <- schema2 |>
    dplyr::filter(!if_any({{ within_field }}, is.na)) |>
    janitor::remove_empty("cols") |>
    dplyr::arrange(desc(across("data_edit_date")))

  lookup_stub <- toupper(sub("cd$", "", lookup_field))

  # Prioritise results where lookup_field is at the left-hand end
  results <- results_0 |>
    dplyr::filter(
      if_any("service_name", \(x) stringr::str_starts(x, lookup_stub))
    ) |>
    dplyr::bind_rows(results_0) |>
    dplyr::distinct()

  assert_that(
    nrow(results) > 0,
    msg = paste0(
      "return_lookup_query_info: No result was found for the parameters ",
      "supplied. Try a different year or a different country filter?"
    )
  )

  if (nrow(results) > 1 && is.null(option) && chatty) {
    cli_alert_info(
      stringr::str_c(
        "More than 1 result found:",
        stringr::str_flatten(
          paste0(
            "\t(",
            seq_len(nrow(results)),
            ") ",
            results[["service_name"]]),
          collapse = "\n"),
      "Using option {tbl_option}. ",
      "(Change the `option` parameter to use a different one.)",
      sep = "\n")
    )
  }

  query_url <- results |>
    dplyr::slice(option) |>
    dplyr::pull("service_url")

  # Return query URL and lookup_field and within_field in a list,
  # to be passed on to `create_lookup_table()`.
  list(
    query_url = query_url,
    lookup_field = lookup_field,
    within_field = within_field
  )
}




#' @noRd
#' @keywords internal
return_field_code <- function(lookup, year, names_vec) {
  if (is.null(year)) {
    y2 <- names_vec |>
      stringr::str_subset(glue("(?<=^{lookup})\\d+")) |>
      stringr::str_extract("\\d{2}(?=cd$)") |>
      as.numeric() |>
      max() |> # should return most recent year by default
      stringr::str_pad(width = 2, side = "left", pad = "0")
  } else {
    y2 <- stringr::str_extract(year, "\\d{2}$")
  }

  field_code <- stringr::str_subset(names_vec, glue("^{lookup}{y2}cd$"))[[1]]

  assert_that(
    length(field_code) == 1,
    msg = paste0(
      "return_lookup_query_info: That combination of area levels and year ",
      "has not returned a result. Perhaps try a different year?"
    )
  )

  field_code
}
