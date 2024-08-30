#' Common procedure for joinable and not-necessarily-joinable lookup()s
#'
#' @param joinable logical. Whether this query is joinable (`TRUE`) or
#'  forms part of a geospatial query (`FALSE`). In the latter case, this
#'  function will try to return a lookup table that contains the appropriate
#'  field for spatial data (that is, boundaries or centroids) to be joined onto.
#'  For `{boundr}`'s internal use only, users should ignore this.
#' @keywords internal
common_lookup <- function(
    lookup_level,
    within_level = NULL,
    within_names = NULL,
    within_codes = NULL,
    lookup_year = NULL,
    within_year = NULL,
    opts = boundr_options(),
    joinable = FALSE) {
  lookup_level <- tolower(lookup_level)
  return_width <- opts[["rw"]]
  query_option <- opts[["opt"]]

  if (is.null(within_level)) {
    query_info <- return_narrow_table_info(lookup_level, lookup_year)
  } else {
    query_info <- return_lookup_table_info(
      lookup_level,
      within_level,
      lookup_year,
      within_year,
      joinable
    )
  }

  query_data <- query_info |>
    process_query_info(within_names, within_codes, return_width, query_option)
  tbl <- process_lookup_query_data(query_data) |>
    dplyr::bind_rows() |>
    dplyr::select(!any_of(c("object_id", "global_id", "chgind"))) |>
    dplyr::distinct()

  if (lookup == "msoa") tbl <- add_msoa_names(tbl)
  if (return_width != "full") tbl <- remove_nmw(tbl)

  tbl |>
    dplyr::distinct() |>
    janitor::remove_empty("cols")
}


# Helper functions

#' If our `lookup` is MSOA, add the House of Commons Library MSOA Names -
#' @keywords internal
add_msoa_names <- function(x) {
  if (!any(grepl("^msoa[12]1cd$", names(x)))) {
    return(x)
  } else {
    mcol <- first(stringr::str_subset(names(x), "^msoa[12]1cd$"))
    hocl_tbl <- if (mcol == "msoa21cd") hocl_msoa21_names else hocl_msoa11_names
    join_vars <- intersect(names(x), names(hocl_tbl))
    dplyr::left_join(hocl_tbl, x, by = join_vars)
  }
}

#' If we don't have any areas in Wales, remove 'nmw' columns for simplicity
#' @keywords internal
remove_nmw <- function(tbl) {
  cy <- any(grepl("^W", dplyr::pull(dplyr::select(tbl, ends_with("cd")), 1)))
  if (cy) tbl else dplyr::select(tbl, !ends_with("nmw"))
}
