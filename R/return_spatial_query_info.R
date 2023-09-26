#' Return lookup query URL and lower and higher field codes
#'
#' @param lookup character. Lower level area code eg "lsoa", "wd", "lad".
#'  Equivalent to the `lookup` parameter in `bounds()`.
#' @param lookup_year numeric or character. A specific year for data relating
#'  to parameter `x`, if needed. Defaults to `NULL`, which will return the most
#'  recent data.
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
return_spatial_query_info <- function(
    lookup,
    lookup_year,
    option,
    chatty = rlang::is_interactive()
) {
  if (is.null(option)) option <- 1

  # filter only spatial tables from the schema
  schema_spatial <- opengeo_schema |>
    dplyr::filter(
      if_any("has_geometry")
    ) |>
    janitor::remove_empty("cols")

  schema_spatial_names <- schema_spatial |>
    dplyr::select(ends_with("cd")) |>
    names()


  lookup_field <- return_field_code(lookup, lookup_year, schema_spatial_names)

  if (chatty) {
    ui_info("Using code {lookup_field}")
  }


  results <- schema_spatial |>
    dplyr::filter(if_any({{ lookup_field }}, \(x) !is.na(x))) |>
    janitor::remove_empty("cols") |>
    dplyr::arrange(desc(across("data_edit_date"))) |>
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
    dplyr::pull(all_of("service_url"))

  list(
    query_url = query_url,
    lookup_field = lookup_field)
}
