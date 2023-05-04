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
#'  returned table to keep. Options are "tidy", "full" or "minimal".
#'
#' @examples
#' create_lookup_table("pcon", "utla", "South Gloucestershire")
#'
#' @returns A tibble.
#' @export
create_lookup_table <- function(
    lookup,
    within,
    within_names = NULL,
    within_codes = NULL,
    return_width = c("tidy", "full", "minimal"),
    lookup_year = NULL,
    within_year = NULL,
    country_filter = c("UK|EW|EN|WA", "UK", "EW", "EN", "WA"),
    option = NULL,
    chatty = rlang::is_interactive()
  ) {

  # https://developers.arcgis.com/rest/services-reference/
  # enterprise/query-feature-service-layer-.htm

  return_width <- match.arg(return_width)
  assert_that(length(return_width) == 1)
  country_filter <- match.arg(country_filter)
  assert_that(length(country_filter) == 1)

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
    # within_string <- "1=1"
    within_string <- ""
  } else {
    within_string <- c(
      within_name_field |>
        paste0(
          " IN (",
          stringr::str_flatten(
            paste0("'", within_names, "'"),
          collapse = ","),
          ")"
        ) |>
        utils::head(length(within_names)),
      within_code_field |>
        paste0(
          " IN (",
          stringr::str_flatten(
            paste0("'", within_codes, "'"),
            collapse = ","),
          ")"
        ) |>
        utils::head(length(within_codes))
    ) |>
      stringr::str_flatten(collapse = " OR ")
  }

  ids <- query_base_url |>
    return_result_ids(where = within_string) |>
    unique()

  assertthat::assert_that(is.vector(ids),
                          msg = "No IDs returned by `return_result_ids()`")


  fields <- switch(return_width,
                   "tidy" = c(
                     lookup_code_field,
                     lookup_name_field,
                     within_code_field,
                     within_name_field),
                   "full" = "*",
                   "minimal" = c(lookup_code_field, lookup_name_field))



  # Actually retrieve the data (in batches if necessary).
  # (100 seems to be safe? 500 is ok in theory but sometimes we get a 404 -
  # not to do with maxRecordCount but just query URL too long??)
  out <- ids |>
    batch_it(100) |>
    purrr::map(\(x) return_table_data(x, query_base_url, fields),
      .progress = "Lookup table data") |>
    purrr::list_rbind()


  # More hacks to handle the MSOAs issue
  if (lookup == "msoa") {
    msoa_name_field <- sub("^lsoa", "msoa", lookup_name_field)
    if (msoa_name_field == "msoa11nm") hocl_msoa_names <- hocl_msoa11_names
    else if (msoa_name_field == "msoa21nm") hocl_msoa_names <- hocl_msoa21_names
    else hocl_msoa_names <- NULL
    out <- out |>
      # dplyr::select(all_of(c(lookup_code_field, lookup_name_field))) |>
      dplyr::select(all_of(lookup_name_field)) |>
      dplyr::mutate(
        {{ msoa_name_field }} := unlist(across(all_of(lookup_name_field), \(x) sub("[A-Z]{1}$", "", x)))
      ) |>
      dplyr::left_join(hocl_msoa_names, msoa_name_field) |>
      # dplyr::left_join(out, c(lookup_code_field, lookup_name_field)) |>
      dplyr::left_join(out, lookup_name_field) |>
      dplyr::select(names(hocl_msoa_names), !starts_with("lsoa"))

  } else if (within == "msoa") {
    msoa_name_field <- sub("^lsoa", "msoa", within_name_field)
    if (msoa_name_field == "msoa11nm") hocl_msoa_names <- hocl_msoa11_names
    else if (msoa_name_field == "msoa21nm") hocl_msoa_names <- hocl_msoa21_names
    else hocl_msoa_names <- NULL
    out <- out |>
      # dplyr::select(all_of(c(within_code_field, within_name_field))) |>
      dplyr::select(all_of(within_name_field)) |>
      dplyr::mutate(
        {{ msoa_name_field }} := unlist(across(all_of(within_name_field), \(x) sub("[A-Z]{1}$", "", x)))
      ) |>
      dplyr::left_join(hocl_msoa_names, msoa_name_field) |>
      # dplyr::left_join(out, c(within_code_field, within_name_field)) |>
      dplyr::left_join(out, within_name_field) |>
      dplyr::select(all_of(c(names(out), names(hocl_msoa_names))))

    if (lookup != "lsoa" | return_width != "full") {
      out <- out |>
        dplyr::select(!starts_with("lsoa"))
    }
  }

  out |>
    dplyr::select(!any_of(c("object_id", "global_id", "chgind"))) |>
    dplyr::distinct() |>
    janitor::remove_empty("cols")
}
