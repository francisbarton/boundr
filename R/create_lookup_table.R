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
#' @param within character. Higher level area code eg "lad", "cty". Equivalent
#' to the `within` parameter in `bounds()`.
#' @param within_year numeric or character. A specific year for data relating
#'  to parameter `y`, if needed. Defaults to `NULL`, which will return the most
#'  recent data.
#' @param country_filter character. Open Geography datasets are sometimes
#'  available just within certain countries. Specify a country code if you want
#'  your results restricted to a certain country only - eg "WA" for Wales, "EW"
#'  for England and Wales. By default returns all options.
#' @param return_width character. How many of the possible columns in the
#'  returned table to keep. Options are "tidy", "full" or "minimal".
#'
#' @examples
#' create_lookup_table("msoa", "lad", "Swindon")
#' create_lookup_table("wd", "sener", lookup_year = 2022, return_width = "full")
#'
#' @returns A tibble
#' @export
create_lookup_table <- function(
    lookup,
    within = NULL,
    within_names = NULL,
    within_codes = NULL,
    return_width = c("tidy", "full", "minimal"),
    lookup_year = NULL,
    within_year = NULL,
    country_filter = c("UK|GB|EW|EN|SC|WA", "UK", "GB", "EW", "EN", "SC", "WA"),
    option = NULL,
    chatty = rlang::is_interactive()
  ) {

  # https://developers.arcgis.com/rest/services-reference/
  # enterprise/query-feature-service-layer-.htm

  new_lookup <- process_aliases(lookup)
  if (lookup == "msoa") new_lookup <- "lsoa"
  if (is.null(within)) within <- lookup
  new_within <- process_aliases(within)
  # if (within == "msoa") new_within <- "lsoa"
  return_width <- match.arg(return_width)
  country_filter <- match.arg(country_filter)
  if (is.null(option)) option <- 1

  lookup_query_info <- return_lookup_query_info(
    new_lookup,
    new_within,
    lookup_year,
    within_year,
    country_filter,
    option,
    standalone,
    chatty
  )

  query_base_url <- lookup_query_info[["query_url"]]

  lookup_code_field <- lookup_query_info[["lookup_field"]]
  lookup_name_field <- sub("cd$", "nm", lookup_code_field)
  within_code_field <- lookup_query_info[["within_field"]]
  within_name_field <- sub("cd$", "nm", within_code_field)

  fields <- switch(
    return_width,
    "tidy" = c(
      lookup_code_field,
      lookup_name_field,
      within_code_field,
      within_name_field
    ),
    "full" = "*",
    "minimal" = c(
      lookup_code_field,
      lookup_name_field
    )
  )

  if (lookup == "oa") {
    fields <- stringr::str_subset(fields, "^oa.+nm$", negate = TRUE)
  }



  if (is.null(within_names) & is.null(within_codes)) {
    within_string <- "1=1"
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

  assert_that(
    !is.null(within_string),
    msg = paste0(
      "create_lookup_table: The within_names or within_codes arguments ",
      "have not resulted in a valid query string."
    )
  )

  ids <- unique(return_result_ids(query_base_url, where = within_string))

  assert_that(
    is.vector(ids) & !is.list(ids) & length(ids),
    msg = "create_lookup_table: The query has not returned any result IDs."
  )


  # Actually retrieve the data (in batches if necessary).
  # (100 seems to be safe? 500 is ok in theory but sometimes we get a 404 -
  # not to do with maxRecordCount but just query URL too long??)
  out <- ids |>
    batch_it(100) |>
    purrr::map(\(x) return_table_data(x, query_base_url, fields),
      .progress = {if (chatty) "Lookup table data"}) |>
    dplyr::bind_rows() |>
    dplyr::select(!any_of(c("object_id", "global_id", "chgind"))) |>
    dplyr::distinct()


  # A hack to handle the MSOAs issue (Lookups are not available as often for
  # MSOAs as they are for LSOAs). Aug 2024: This no longer seems to be the case.
  # if (lookup == "msoa") {
  #   msoanm_col <- sub("^lsoa", "msoa", lookup_name_field)
  #   msoacd_col <- sub("nm$", "cd", msoanm_col)

  #   out <- out |>
  #     dplyr::rename({{ msoanm_col }} := {{ lookup_name_field }}) |>
  #     dplyr::mutate(across({{ msoanm_col }}, \(x) sub("[A-Z]{1}$", "", x))) |>
  #     dplyr::select(!starts_with("lsoa")) |>
  #     dplyr::distinct()
  # }

  if (any(grepl("^msoa", names(out)))) {
    if (any(grepl("^msoa21", names(out)))) {
      hocl_tbl <- hocl_msoa21_names
    } else if (any(grepl("^msoa11", names(out)))) {
      hocl_tbl <- hocl_msoa11_names
    } else {
      hocl_tbl <- NULL
    }

    msoanm_col <- grep("^msoa.*nm$", names(out), value = TRUE)[[1]]
    join_vars <- intersect(names(out), names(hocl_tbl))

    out <- out |>
      dplyr::left_join(hocl_tbl, by = {{ join_vars }}) |>
      dplyr::relocate(matches("^msoa.*cd$"), .before = {{ msoanm_col }}) |>
      dplyr::relocate(matches("^msoa.*hclnm"), .after = {{ msoanm_col }}) |>
      dplyr::relocate(matches("^msoa\\d*nmw$"), .after = {{ msoanm_col }})
  }


  any_welsh <- out |>
    dplyr::select(ends_with("cd")) |>
    dplyr::pull(1) |>
    purrr::some(\(x) grepl("^W", x))

  if (!any_welsh) out <- out |>
    dplyr::select(!ends_with("nmw"))

  out |>
    dplyr::distinct() |>
    janitor::remove_empty("cols")
}


#' Process geo level aliases
#' @noRd
process_aliases <- function(x, y = NULL) {
  tolower(x %||% y) |>
    stringr::str_replace_all(
      c(
        "parish" = "par",
        "ward" = "wd",
        "county" = "cty",
        "region" = "rgn",
        "country" = "ctry"
      )
    )
}
