#' Create a lookup table by querying the ONS OpenGeography API
#' 
#' Calls `return_lookup_query_info()` and processes the output, returning a
#'  tibble.
#' 
#' @inheritParams return_lookup_query_info
#' @param within_names,within_codes character. In order to restrict data
#'  returned to a specific area, either `within_names` or `within_codes` must
#'  be provided. Otherwise all boundaries within the country at that level will
#'  be retrieved. Use place names eg "Essex" to restrict to a certain
#'  geographical area. Use ONS area codes eg "W02000103" likewise (this is
#'  useful with wards, where there are many that share identical names). A
#'  vector of multiple names or codes can be supplied.
#' @param return_width character. How many of the possible columns in the
#'  returned table to keep. Options are "tidy", "basic", "full" or "minimal".
#'
#' @returns A tibble.
#' @export
create_lookup_table <- function(
    lookup,
    within,
    within_names = NULL,
    within_codes = NULL,
    return_width = c("tidy", "basic", "full", "minimal"),
    lookup_year = NULL,
    within_year = NULL,
    country_filter = c("UK|EW|EN|WA", "UK", "EW", "EN", "WA"),
    option = 1
  ) {

  # https://developers.arcgis.com/rest/services-reference/
  # enterprise/query-feature-service-layer-.htm

  country_filter <- match.arg(country_filter)
  assertthat::assert_that(length(country_filter) == 1)
  return_width <- match.arg(return_width)
  assertthat::assert_that(length(return_width) == 1)

  # call subsidiary function
  lookup_query_info <- return_lookup_query_info(
    lookup,
    within,
    lookup_year,
    within_year,
    country_filter,
    option)

  query_base_url <- lookup_query_info[["query_url"]]

  lookup_code_field <- lookup_query_info[["lookup_field"]]
  lookup_name_field <- sub("cd$", "nm", lookup_code_field)
  within_code_field <- lookup_query_info[["within_field"]]
  within_name_field <- sub("cd$", "nm", within_code_field)

  if (is.null(within_names) & is.null(within_codes)) {
    within <- "1=1"
  } else {
    within <- c(
      within_name_field |>
        paste0(
          " = ",
          (paste0("'", toupper(within_names), "'"))
        ) |>
        utils::head(length(within_names)),
      within_code_field |>
        paste0(
          " = ",
          (paste0("'", toupper(within_codes), "'"))
        ) |>
        utils::head(length(within_codes))
    ) |>
      stringr::str_flatten(collapse = " OR ")
  }

  ids <- query_base_url |>
    return_result_ids(where = within) |>
    unique()



  fields <- switch(return_width,
                   "tidy" = "*",
                   "basic" = paste(lookup_code_field, lookup_name_field, within_code_field, within_name_field, collapse = ","),
                   "full" = "*",
                   "minimal" = paste(lookup_code_field, lookup_name_field, collapse = ","))



  # actually retrieve the data (in batches of 500 ids if necessary)
  out <- ids |>
    batch_it(500) |>
    purrr::map(
      \(ids) return_table_data(ids, query_base_url, fields),
      .progress = "Lookup table data") |>
    purrr::list_rbind()

  if (tolower(lookup) == "msoa" &
    grepl("^lsoa", lookup_code_field)) {
    lookup_code_field <- lookup_code_field |>
      stringr::str_replace("^lsoa", "msoa")
    lookup_name_field <- lookup_name_field |>
      stringr::str_replace("^lsoa", "msoa")
    msoa_names <- switch(key,
      msoa11nm = hocl_msoa11_names,
      msoa21nm = hocl_msoa21_names)
    out <- out |>
      dplyr::select(!lookup_code_field) |>
      dplyr::rename_with(\(x) sub("^lsoa", "msoa", x)) |>
      dplyr::mutate(across(lookup_name_field,
        \(x) sub("[:upper:]{1}$", "", x))) |>
      dplyr::distinct() |>
      dplyr::left_join(msoa_names, by = lookup_name_field) |>
      dplyr::relocate(starts_with("msoa")) |>
      dplyr::relocate(lookup_code_field) |>
      janitor::remove_empty("cols")
  }


  if (return_width == "tidy") {
    out <- out |>
      dplyr::select(lookup_code_field:within_name_field)
  }

  out |>
    dplyr::distinct() |>
    janitor::remove_empty("cols")
}
