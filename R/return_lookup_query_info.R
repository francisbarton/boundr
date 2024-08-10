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
#' @param chatty Boolean. Whether to print feedback on the 'decisions' the
#'  function has taken about which table to query. Default `TRUE` when the
#'  function is run in an interactive session, `FALSE` otherwise.
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
    chatty
  ) {
  # filter only lookup tables from the schema
  # and those with the right country filter
  schema_lookups <- opengeo_schema |>
    dplyr::filter(
      !if_any("has_geometry") &
      if_any("service_name", \(x) stringr::str_detect(x, country_filter))
    ) |>
    janitor::remove_empty("cols")

  assert_that(nrow(schema_lookups) > 0,
    msg = "return_lookup_query_info: no lookup tables found.")

  # make list of codes used in lookups
  schema_names <- schema_lookups |>
    dplyr::select(ends_with("cd")) |>
    names()

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
    ui_info("Using codes {lookup_field}, {within_field}")
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

  assert_that(nrow(results) > 0,
    msg = "return_lookup_query_info: No result was found for the parameters supplied. Try a different year or a different country filter?")

  if (nrow(results) > 1 & is.null(option) & chatty) {
    ui_info(
      stringr::str_c(
        "More than 1 result found:",
        stringr::str_flatten(
          paste0(
            "\t(",
            seq(nrow(results)),
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

  # return query URL and lookup_field and within_field in a list,
  # to be passed on to create_lookup_table()
  list(
    query_url = query_url,
    lookup_field = lookup_field,
    within_field = within_field)
}




#' @noRd
return_field_code <- function(prefix, year, names_vec) {
  if (is.null(year)) {
    years <- names_vec |>
      stringr::str_extract(glue("(?<=^{prefix})\\d+"))  |>
      as.numeric()

    # will need updating in 2030 ;-)
    year_out <- dplyr::if_else(years > 30, years + 1900, years + 2000) |>
      # choose most recent year available
      max(na.rm = TRUE) |>
      stringr::str_extract("\\d{2}$")
  } else {
    year_out <- year |>
      stringr::str_extract("\\d{1,2}$") |>
      stringr::str_pad(width = 2, side = "left", pad = "0")
  }

  field_code <- paste0(prefix, year_out, "cd")

  assert_that(
    field_code %in% names_vec,
    msg = paste0(
      "return_lookup_query_info: That combination of area levels ",
      "and years has not returned a result. Perhaps try a different year?"
    )
  )

  field_code
}



