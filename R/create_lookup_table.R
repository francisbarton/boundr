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
#' @param include_welsh logical. Whether to attempt to include Welsh language
#'  name fields in the lookup table (fields ending in 'nmw') where available.
#'  Default `FALSE`.
#'
#' @examples
#' create_lookup_table("msoa", "utla", "Swindon")
#'
#' @returns A tibble
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
    include_welsh = FALSE,
    chatty = rlang::is_interactive()
  ) {

  # https://developers.arcgis.com/rest/services-reference/
  # enterprise/query-feature-service-layer-.htm

  new_lookup <- process_aliases(lookup)
  new_within <- process_aliases(within)
  return_width <- match.arg(return_width)
  country_filter <- match.arg(country_filter)
  if (is.null(option)) tbl_option <- 1 else tbl_option <- option

  lookup_query_info <- return_lookup_query_info(
    new_lookup,
    new_within,
    lookup_year,
    within_year,
    country_filter,
    tbl_option,
    chatty)

  query_base_url <- lookup_query_info[["query_url"]]

  lookup_code_field <- lookup_query_info[["lookup_field"]]
  lookup_name_field <- sub("cd$", "nm", lookup_code_field)
  within_code_field <- lookup_query_info[["within_field"]]
  within_name_field <- sub("cd$", "nm", within_code_field)

  if (include_welsh) {
    lookup_name_field_cy <- sub("cd$", "nmw", lookup_code_field)
    within_name_field_cy <- sub("cd$", "nmw", within_code_field)
  } else {
    lookup_name_field_cy <- NULL
    within_name_field_cy <- NULL
  }


  fields <- switch(return_width,
                   "tidy" = c(
                     lookup_code_field,
                     lookup_name_field,
                     lookup_name_field_cy,
                     within_code_field,
                     within_name_field,
                     within_name_field_cy),
                   "full" = "*",
                   "minimal" = c(lookup_code_field, lookup_name_field, lookup_name_field_cy))



  if (is.null(within_names) & is.null(within_codes)) {
    # within_string <- "1=1"
    within_string <- ""
  } else if (!is.null(within_names)) {
    within_string <- within_name_field |>
        paste0(
          " IN (",
          stringr::str_flatten(
            paste0("'", within_names, "'"),
          collapse = ","),
          ")"
        ) |>
        head(length(within_names)) |>
      stringr::str_flatten(collapse = " OR ")
  } else if (!is.null(within_codes)) {
    within_string <- within_code_field |>
        paste0(
          " IN (",
          stringr::str_flatten(
            paste0("'", within_codes, "'"),
            collapse = ","),
          ")"
        ) |>
        head(length(within_codes)) |>
      stringr::str_flatten(collapse = " OR ")
  } else {
    within_string <- NULL
  }

  assert_that(!is.null(within_string),
              msg = "create_lookup_table: The within_names or within_codes arguments have not resulted in a valid query string.")

  ids <- query_base_url |>
    return_result_ids(where = within_string) |>
    unique()

  assert_that(length(ids) > 0, msg = "create_lookup_table: The initial query has not returned any results IDs.")



  # Actually retrieve the data (in batches if necessary).
  # (100 seems to be safe? 500 is ok in theory but sometimes we get a 404 -
  # not to do with maxRecordCount but just query URL too long??)
  out <- ids |>
    batch_it(100) |>
    purrr::map(\(x) return_table_data(x, query_base_url, fields),
      .progress = "Lookup table data") |>
    purrr::list_rbind() |>
    dplyr::select(!any_of(c("object_id", "global_id", "chgind")))


  # More hacks to handle the MSOAs issue
  hocl_msoa_names <- NULL

  if (lookup == "msoa") {
    if (grepl("^lsoa11", lookup_name_field)) hocl_tbl <- hocl_msoa11_names
    if (grepl("^lsoa21", lookup_name_field)) hocl_tbl <- hocl_msoa21_names
    out2 <- join_msoa_table(out, lookup_name_field, hocl_tbl, type = "lookup")
  }

  if (within == "msoa") {
    if (grepl("^lsoa11", within_name_field)) hocl_tbl <- hocl_msoa11_names
    if (grepl("^lsoa21", within_name_field)) hocl_tbl <- hocl_msoa21_names
    out2 <- join_msoa_table(out, within_name_field, hocl_tbl, type = "within")

    if (lookup != "lsoa" | return_width != "full") {
      out2 <- out2 |>
        dplyr::select(!starts_with("lsoa"))
    }
  }

  out2 |>
    dplyr::distinct() |>
    janitor::remove_empty("cols")
}


#' Process geo level aliases
#' @noRd
process_aliases <- function(x) {
  x |>
    tolower() |>
    stringr::str_replace_all(c(
      "parish" = "par",
      "ward" = "wd",
      "county" = "cty",
      "region" = "rgn",
      "country" = "ctry",

      # Some lookups contain LSOA but not MSOA. If the user has requested MSOA
      # we can search for LSOA instead, and later convert back to MSOA, because
      # LSOA and MSOA names play nicely together
      "msoa" = "lsoa"
    ))
}


join_msoa_table <- function(.data, lsoa_col, hocl_tbl, type = c("lookup", "within")) {
  type <- match.arg(type)
  msoa_col <- sub("^lsoa", "msoa", lsoa_col)

  out <- .data |>
    dplyr::mutate({{msoa_col}} := c_across(lsoa_col)) |>
    dplyr::mutate(across({{msoa_col}}, \(x) sub("[A-Z]{1}$", "", x))) |>
    dplyr::left_join(hocl_tbl, msoa_col)

  if (type == "lookup") {
    out |>
      dplyr::select(all_of(c(names(hocl_tbl))), !starts_with("lsoa"))
  } else if (type == "within") {
    out |>
      dplyr::select(all_of(c(names(.data), names(hocl_tbl)))) |>
      dplyr::relocate(matches("^.*oa"))
  } else invisible(TRUE)
}

