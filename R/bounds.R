#' Return boundary data at a specified level and area from the ONS OG API
#'
#' @inheritParams create_lookup_table
#' @inheritParams bounds_data_req
#' @param resolution character. How generalised should the boundary be, and
#'  whether coastal boundaries should adhere to the coastline or to the full
#'  territorial extent. BGC by default (G = Generalised (20m), C = limited to
#'  the coastline.) F indicates Full resolution; S indicates Super-generalised
#'  (200m); U indicates Ultra-generalised (500m) boundary resolution.
#' @param centroids Boolean. If TRUE, return centroids rather than boundaries.
#'
#' @export
create_bounds_table <- function(
    lookup,
    within,
    within_names = NULL,
    within_codes = NULL,
    return_width = c("tidy", "full", "minimal"),
    lookup_year = NULL,
    within_year = NULL,
    country_filter = c("UK|EW|EN|WA", "UK", "EW", "EN", "WA"),
    resolution = c("BGC", "BSC", "BUC", "BFC", "BFE"),
    centroids = FALSE,
    option = NULL,
    crs = 4326
  ) {

  lookup <- tolower(lookup)
  within <- tolower(within)
  return_width <- match.arg(return_width)
  assert_that(length(return_width) == 1)
  country_filter <- match.arg(country_filter)
  assert_that(length(country_filter) == 1)
  resolution <- match.arg(resolution)
  assert_that(length(resolution) == 1)


  lookup_table <- create_lookup_table(
    lookup,
    within,
    within_names,
    within_codes,
    return_width,
    lookup_year,
    within_year,
    country_filter,
    option)

  # This isn't watertight: if you have a table with eg "lad16cd" and "lad21cd"
  # columns, it will pull the leftmost column to use for the geometry query,
  # which may not be what you want.
  # It is fairly rare that more than 1 column name will match, and it can be
  # avoided in most cases by choosing a more appropriate 'option' parameter.
  # A 'tidy' or 'minimal' return_width parameter will eliminate this risk.
  geo_code_field <- lookup_table |>
    dplyr::select(starts_with(lookup) & ends_with("cd")) |>
    dplyr::select(1) |> # select the leftmost matching column
    names()


  if (centroids) {
    query_base_url <- pull_centroid_query_url(geo_code_field)
  } else {
    query_base_url <- pull_bounds_query_url(geo_code_field, resolution)
  }

  area_codes <- lookup_table |>
    dplyr::pull({{ geo_code_field }}) |>
    batch_it(50) |> # turns out this limit is rather crucial!
    purrr::map(\(x) paste_area_codes(var = geo_code_field, vec = x))

  ids <- area_codes |>
    purrr::map(\(x) return_result_ids(url = query_base_url, where = x))

  bounds_data <- ids |>
    purrr::map(\(x) return_bounds_data(x, query_base_url, crs)) |>
    purrr::list_rbind() |>
    janitor::clean_names()

  join_vars <- intersect(names(lookup_table), names(bounds_data))

  if (return_width %in% c("tidy", "minimal")) {
    bounds_data <- bounds_data |>
      dplyr::select(all_of(c(join_vars, "geometry")))
  }

  bounds_data |>
    dplyr::left_join(lookup_table, join_vars) |>
    dplyr::relocate(names(lookup_table)) |>
    dplyr::select(!any_of(c("fid", "objectid", "global_id"))) |>
    janitor::remove_empty("cols") |>
    dplyr::distinct()
}




#' @rdname create_bounds_table
#' @export
bounds <- create_bounds_table





# Helper functions --------------------------------

#' @noRd
pull_bounds_query_url <- function(field, resolution) {
  results <- opengeo_schema |>
    dplyr::filter(if_any("has_geometry")) |>
    dplyr::filter(if_any({{ field }}, \(x) !is.na(x))) |>
    dplyr::filter(stringr::str_detect(service_name, toupper(resolution))) |>
    janitor::remove_empty("cols")

  assert_that(nrow(results) > 0,
              msg = paste0(
              "No boundary data was found for the parameters supplied. ",
              "Try a different year or a different resolution?"))

  results |>
    # We assume any row of results will give the desired geo data. So take #1.
    dplyr::slice(1) |>
    dplyr::pull(service_url)
}

#' @noRd
pull_centroid_query_url <- function(field) {
  results <- opengeo_schema |>
    dplyr::filter(if_any({{ field }}, \(x) !is.na(x))) |>
    dplyr::filter(stringr::str_detect(service_name, "PWC|Centroids")) |>
    janitor::remove_empty("cols")

  assert_that(nrow(results) > 0,
              msg = paste0(
              "No boundary data was found for the parameters supplied. ",
              "Try a different year or a different resolution?"))

  results |>
    # We assume any row of results will give the desired geo data. So take #1.
    dplyr::slice(1) |>
    dplyr::pull(service_url)
}

#' @noRd
paste_area_codes <- function(var, vec) {
  var |>
    paste0(
      " IN (",
      stringr::str_flatten(
        paste0("'", unique(vec), "'"),
        collapse = ","),
      ")"
      )
}


#' @noRd
batch_it <- function(x, batch_size) {
  f <- rep(1:ceiling(length(x) / batch_size), each = batch_size) |>
    utils::head(length(x))
  split(x, f)
}
