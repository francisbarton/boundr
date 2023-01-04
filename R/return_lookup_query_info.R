#' Return lookup query URL and lower and higher field codes
#' @param lookup Lower level area code eg "lsoa", "wd", "lad". Equivalent to the `lookup` parameter in `bounds()`.
#' @param within Higher level area code eg "lad", "cty". Equivalent to the `within` parameter in `bounds()`.
#' @param lookup_year A specific year for data relating to parameter `x`, if needed. Defaults to `NULL`, which will return the most recent data.
#' @param within_year A specific year for data relating to parameter `y`, if needed. Defaults to `NULL`, which will return the most recent data.
#' @param country_filter Open Geography datasets are sometimes available just within certain countries. Specify a country code if you want your results restricted to a certain country only - eg "WA" for Wales, "EW" for England and Wales. By default returns all options.
#' @param option Defaults to 1, which means that the URL will just be the first one from the list of possible services resulting from the level and year filters above. If this does not give you what you want, you can run the script again with a different option from the list.
#' @returns A list of length 3: the query URL, the lower level (`lookup`) field code (eg `lsoa11cd`), and the higher level (`within`) field code.
return_lookup_query_info <- function(
    lookup,
    within,
    lookup_year = NULL,
    within_year = NULL,
    country_filter = c("UK|EW|EN|WA", "UK", "EW", "EN", "WA"),
    option = 1
  ) {

  country_filter <- match.arg(country_filter)
  assertthat::assert_that(is.numeric(option))

  # filter only lookup tables from the schema
  # and those with the right country filter
  schema_lookups <- opengeo_schema |>
    dplyr::filter(if_any("service_name", \(x) stringr::str_detect(x, "_LU$"))) |>
    dplyr::filter(if_any("service_name", \(x) stringr::str_detect(x, country_filter))) |>
    janitor::remove_empty("cols")

  # make list of codes used in lookups
  schema_names <- schema_lookups |>
    dplyr::select(ends_with("cd")) |>
    names()

  return_field_code <- function(prefix, year = NULL, names_vec) {
    if (is.null(year)) {
      years <- names_vec |>
        stringr::str_extract(stringr::str_glue("(?<=^{prefix})\\d+"))  |>
        as.numeric()
      year_out <- dplyr::case_when(
        years > 30 ~ years + 1900, # needs updating in 2030 ;-)
        TRUE ~ years + 2000
      ) |>
      sort() |>
      utils::tail(1) |>
      stringr::str_extract("\\d{2}$")
    } else {
      year_out <- year |>
      stringr::str_extract("\\d{1,2}$") |>
      stringr::str_pad(width = 2, "left", pad = "0")
    }

    field_code <- paste0(prefix, year_out, "cd")
    assertthat::assert_that(field_code %in% names_vec,
    msg = "return_lookup_query_info: That combination of area levels and years has not returned a result. Please try a different year.")

    # return
    field_code
  }

  lookup_field <- return_field_code(lookup, lookup_year, schema_names)




  # reduce schema to only those matching x_field
  schema2 <- schema_lookups |>
    dplyr::filter(!is.na(!!rlang::sym(lookup_field))) |>
    janitor::remove_empty("cols")

  schema2_names <- schema2 |>
    dplyr::select(ends_with("cd")) |>
    names()

  within_field <- return_field_code(within, within_year, schema2_names)




  usethis::ui_info(
    stringr::str_glue("Using codes {lookup_field}, {within_field}.")
  )


  results <- schema2 |>
    dplyr::filter(!is.na(!!rlang::sym(within_field))) |>
    janitor::remove_empty("cols")  |>
    dplyr::arrange(desc(across("edit_date")))

  assertthat::assert_that(
    nrow(results) > 0,
    msg = paste0(
      "return_lookup_query_info: ",
      "No result was found for the parameters supplied. ",
      "Try a different year or a different country filter?"))

  if (nrow(results) > 1) {
      stringr::str_c(
        "More than 1 result found:",
        stringr::str_flatten(
          paste0(
            "\t(",
            seq(nrow(results)),
            ") ",
            results[["service_name"]]),
          collapse = "\n"),
        "Using option {option}. ",
        "(Change the `option` parameter to use a different one.)",
        sep = "\n") |>
    usethis::ui_info()
  }

  query_url <- results |>
    dplyr::slice(option) |>
    dplyr::pull("service_url")

  # return query URL and lookup_field and within_field in a list,
  # to be passed on to create_lookup_table()
  list(query_url, lookup_field, within_field) |>
    purrr::set_names()
}
