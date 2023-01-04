#' Return boundary data at a specified level and area from the ONS OG API
#' @inheritParams create_lookup_table
#' @inheritParams bounds_data_req
#' @param resolution how generalised should the boundary be, and should coastal boundaries adhere to the coastline or to the full territorial extent
#' @export
bounds <- function(
    lookup,
    within,
    within_names = NULL,
    within_codes = NULL,
    return_width = c("tidy", "basic", "full", "minimal"),
    lookup_year = NULL,
    within_year = NULL,
    country_filter = c("UK|EW|EN|WA", "UK", "EW", "EN", "WA"),
    resolution = c("BGC", "BSC", "BUC", "BFC", "BFE"),
    option = 1,
    crs = 4326
  ) {

  resolution <- match.arg(resolution)
  country_filter <- match.arg(country_filter)
  return_width <- match.arg(return_width)

  # !!!!!!!!!!!!!!!!
  # add in subroutine to deal with LSOA -> MSOA conversion
  # in lookup table creation process

  lookup_table <- create_lookup_table(lookup, within, within_names, within_codes, return_width, lookup_year, within_year, country_filter, option)

  geo_code_field <- lookup_table |>
    dplyr::select(starts_with(lookup) & ends_with("cd")) |>
    names()

  query_base_url <- pull_geo_query_url(geo_code_field, resolution)

  area_codes <- lookup_table |>
    dplyr::pull(geo_code_field) |>
    batch_it(50) # turns out this limit is rather crucial!

  bounds_codes <- area_codes |>
    purrr::map(\(codes) paste_area_codes(vec = codes, var = geo_code_field))

  ids <- bounds_codes |>
    purrr::map(\(codes) return_result_ids(url = query_base_url, where = codes))

  bounds_data <- ids |>
    purrr::map(\(ids) return_bounds_data(ids, query_base_url, crs)) |>
    purrr::list_rbind() |>
    janitor::clean_names()

  join_vars <- intersect(names(lookup_table), names(bounds_data))

  bounds_data |>
    dplyr::left_join(lookup_table, by = join_vars) |>
    dplyr::relocate(names(lookup_table)) |>
    dplyr::select(!objectid) |>
    janitor::remove_empty("cols") |>
    dplyr::distinct()
}





# Helper functions --------------------------------

#' @noRd
pull_geo_query_url <- function(field, resolution, option) {
  opengeo_schema |>
    dplyr::arrange(desc(edit_date)) |>
    dplyr::filter(has_geometry) |>
    dplyr::filter(!is.na(field)) |>
    dplyr::filter(stringr::str_ends(service_name, resolution)) |>
    dplyr::slice(1) |>
    dplyr::pull(service_url)
}



#' @noRd
paste_area_codes <- function(vec, var, join_string = " OR ") {
  var |>
    paste0(
      " = ",
      (paste0("'", unique(vec), "'"))) |>
    toupper() |>
    stringr::str_c(collapse = join_string)
}


#' @noRd
batch_it <- function(x, batch_size) {
  f <- rep(1:ceiling(length(x) / batch_size), each = batch_size) |>
    utils::head(length(x))
  split(x, f)
}
