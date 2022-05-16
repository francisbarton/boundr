#' @export
bounds <- function(lookup, within, within_names = NULL, within_codes = NULL, return_width = c("tidy", "basic", "full", "minimal"), lookup_year = NULL, within_year = NULL, country_filter = c("UK|EW|EN|WA", "UK", "EW", "EN", "WA"), resolution = c("BGC", "BSC", "BUC", "BFC", "BFE"), option = 1, geo_option = 1, crs = 4326) {

  resolution <- match.arg(resolution)
  country_filter <- match.arg(country_filter)
  return_width <- match.arg(return_width)

  # !!!!!!!!!!!!!!!!
  # add in subroutine to deal with LSOA -> MSOA conversion
  # in lookup table creation process

  lookup_table <- create_lookup_table(lookup, within, within_names, within_codes, return_width, lookup_year, within_year, country_filter, option)

  geo_code_field <- lookup_table %>%
    dplyr::select(starts_with(lookup) & ends_with("cd")) %>%
    names()

  bounds_query_data <- pull_geo_query_url(geo_code_field, resolution, geo_option)

  query_base_url <- bounds_query_data[["service_url"]]
  geo_code_field <- bounds_query_data[["x_code"]] %>%
    tolower()

  if (!geo_code_field %in% names(lookup_table)) {
    # not sure what we should do if this happens - just error I guess
  }

  area_codes <- lookup_table %>%
    dplyr::pull(geo_code_field) %>%
    batch_it(50) # turns out this limit is rather crucial!

  bounds_codes <- area_codes %>%
    purrr::map(paste_area_codes, var = geo_code_field)

  ids <- bounds_codes %>%
    purrr::map(return_result_ids, url = query_base_url)

  bounds_data <- ids %>%
    purrr::map_df(return_bounds_data, url = query_base_url, crs = crs) %>%
    janitor::clean_names()

  join_vars <- intersect(names(lookup_table), names(bounds_data))

  bounds_data %>%
    dplyr::left_join(lookup_table, by = join_vars) %>%
    dplyr::relocate(names(lookup_table)) %>%
    dplyr::select(!objectid) %>%
    janitor::remove_empty("cols") %>%
    dplyr::distinct()
}

return_bounds_data <- function(ids, url, crs) {
  ids <- stringr::str_c(ids, collapse = ",")
  url %>%
    httr2::request() %>%
    httr2::req_url_path_append("/0/query") %>%
    httr2::req_url_query(objectIds = ids) %>%
    httr2::req_url_query(outFields = "*") %>%
    httr2::req_url_query(returnGeometry = "true") %>%
    httr2::req_url_query(returnIdsOnly = "false") %>%
    httr2::req_url_query(returnCountOnly = "false") %>%
    httr2::req_url_query(outSR = crs) %>%
    httr2::req_url_query(f = "geojson") %>%
    httr2::req_perform() %>%
    httr2::resp_body_string() %>%
    sf::st_read(quiet = TRUE)
}


paste_area_codes <- function(vec, var, join_string = " OR ") {
  var %>%
    paste0(
      " = ",
      (paste0("'", unique(vec), "'"))) %>%
    toupper() %>%
    stringr::str_c(collapse = join_string)
}
