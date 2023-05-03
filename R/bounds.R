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
#' @examples
#' create_lookup_table("pcon", "utla", "South Gloucestershire") |>
#'  dplyr::select(starts_with("pcon") & ends_with("nm")) |>
#'  dplyr::pull(1) |>
#'  create_lookup_table(lookup = "wd", within = "pcon") |>
#'  dplyr::select(starts_with("wd") & ends_with("cd")) |>
#'  dplyr::pull(1) |>
#'  bounds(
#'   lookup = "lsoa",
#'   within = "wd",
#'   within_names = NULL,
#'   return_width = "full")
#' @returns an `sfc` tibble (data frame with geometry)
#' @export
bounds <- function(
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

  assert_that(nrow(lookup_table) > 0,
    msg = "bounds: create_lookup_table() has returned a table with 0 rows")

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

  assert_that(length(geo_code_field) == 1 & !is.na(geo_code_field),
    msg = "bounds: suitable geo_code_field not found from lookup table")


  if (centroids) {
    query_base_url <- pull_centroid_query_url(geo_code_field, lookup)
  } else {
    query_base_url <- pull_bounds_query_url(geo_code_field, lookup, resolution)
  }

  area_codes <- lookup_table |>
    dplyr::pull({{ geo_code_field }}) |>
    batch_it(50) |> # turns out this limit is rather crucial!
    purrr::map(\(x) paste_area_codes(var = geo_code_field, vec = x))

  ids <- area_codes |>
    purrr::map(\(x) return_result_ids(url = query_base_url, where = x)) |>
    purrr::list_c()

  assert_that(is.vector(ids) & !is.list(ids) & length(ids),
    msg = "bounds: return_result_ids() has not returned a vector of IDs.")

  bounds_data <- ids |>
    purrr::map(\(x) return_bounds_data(x, query_base_url, crs))

  assert_that(is.list(bounds_data) & length(bounds_data),
    msg = "bounds: return_bounds_data() has not returned a list of length > 0")
  
  bounds_data_df <- bounds_data |>
    dplyr::bind_rows() |>
    # purrr::list_rbind() |>
    dplyr::distinct() |>
    janitor::clean_names()

  assert_that(inherits(bounds_data_df, "data.frame"),
    msg = "bounds: bounds_data could not be row-bound into a data frame")

  assert_that(inherits(bounds_data_df, "sf"),
    msg = "bounds: the data frame bounds_data_df does not have 'sf' class")
    

  join_vars <- intersect(names(lookup_table), names(bounds_data_df))

  if (return_width %in% c("tidy", "minimal")) {
    bounds_data_df <- bounds_data_df |>
      dplyr::select(all_of(c(join_vars, "geometry")))
  }

  bounds_data_df |>
    dplyr::left_join(lookup_table, join_vars) |>
    dplyr::relocate(names(lookup_table)) |>
    dplyr::select(!any_of(c("fid", "objectid", "global_id"))) |>
    janitor::remove_empty("cols") |>
    dplyr::distinct()
}







# Helper functions --------------------------------

#' @noRd
pull_bounds_query_url <- function(field, lookup, resolution) {
  results <- opengeo_schema |>
    dplyr::filter(if_any("has_geometry")) |>
    dplyr::filter(if_any({{ field }}, \(x) !is.na(x))) |>
    dplyr::filter(if_any(
      "service_name", \(x) stringr::str_starts(x, toupper(lookup)))) |>
    dplyr::filter(if_any(
      "service_name", \(x) stringr::str_detect(x, toupper(resolution)))) |>
    janitor::remove_empty("cols")

  assert_that(nrow(results) > 0,
              msg = paste0(
              "No boundary data was found for the parameters supplied. ",
              "Try a different year or a different resolution?"))

  results |>
    # We assume any row of results will give the desired geo data. So take #1.
    dplyr::slice(1) |>
    dplyr::pull(all_of("service_url"))
}

#' @noRd
pull_centroid_query_url <- function(field, lookup) {
  results <- opengeo_schema |>
    dplyr::filter(if_any({{ field }}, \(x) !is.na(x))) |>
    dplyr::filter(
      if_any("service_name", \(x) stringr::str_starts(x, toupper(lookup)))) |>
    dplyr::filter(
      if_any("service_name", \(x) stringr::str_detect(x, "PWC|Centroids"))) |>
    janitor::remove_empty("cols")

  assert_that(nrow(results) > 0,
              msg = paste0(
              "No boundary data was found for the parameters supplied. ",
              "Try a different year or a different resolution?"))

  results |>
    # We assume any row of results will give the desired geo data. So take #1.
    dplyr::slice(1) |>
    dplyr::pull(all_of("service_url"))
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
