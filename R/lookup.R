#' Create a lookup table by querying the ONS OpenGeography API
#'
#' @param lookup_level character. Lower level area code eg "lsoa", "wd", "lad".
#' @param within_level character. Higher level area code eg "lad", "cty", "icb".
#'  (That is, higher (conceptually) than `lookup_level`; aka larger than
#'  `lookup`!). If not supplied, this uses the default `NULL`, and just data
#'  for `lookup_level` will be returned. In this case, `within_names` and
#'  `within_codes` (if supplied) will instead be taken to refer to, and used to
#'  filter at, the `lookup_level` level.
#' @param within_names,within_codes character. In order to restrict data
#'  returned to a specific area, either `within_names` or `within_codes` must
#'  be provided. Otherwise all available boundaries at that level will be
#'  retrieved. Use place names eg "Essex" to restrict to a certain
#'  geographical area. Or use ONS area codes eg "W02000103" likewise (this is
#'  useful with wards, where there are many that share identical names).
#'  To use this argument to filter `within_level`, the `within_level` must be
#'  specified! Otherwise `{boundr}` will apply them to `lookup_level` instead.
#'  See examples.
#'  Vectors of multiple names or multiple codes can be supplied.
#'  If you supply both `within_names` and `within_codes`, only `within_names`
#'  will be used; (`within_codes` will be ignored).
#' @param lookup_year numeric or character. A specific year for data relating
#'  to `lookup_level`, if you need it. Defaults to `NULL`, which then aims to
#'  return data for the most recent year available. Provide as YYYY.
#' @param within_year numeric or character. A specific year for data relating
#'  to `within_level`, if you need it. Defaults to `NULL`, which then aims to
#'  return data for the most recent year available. Provide as YYYY.
#' @inheritParams bounds
#'
#' @examples
#' lookup("msoa", "lad", "Swindon")
#' lookup("wd", "sener", opts = opts(return_width = "full"))
#'
#' @returns A tibble
#' @export
lookup <- function(
    lookup_level,
    within_level = NULL,
    within_names = NULL,
    within_codes = NULL,
    lookup_year = NULL,
    within_year = NULL,
    opts = boundr_options()) {
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
    x |>
      dplyr::left_join(hocl_tbl, by = join_vars) |>
      dplyr::relocate(contains("hclnm"), .after = all_of(join_vars)) |>
      dplyr::relocate(any_of("geometry"), .after = tidyselect::last_col())
  }
}


#' If we don't have any areas in Wales, remove 'nmw' columns for simplicity
#' @keywords internal
remove_nmw <- function(tbl) {
  cy <- any(grepl("^W", dplyr::pull(dplyr::select(tbl, ends_with("cd")), 1)))
  if (cy) tbl else dplyr::select(tbl, !ends_with("nmw"))
}
