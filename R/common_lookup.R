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
