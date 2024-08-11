#' Return boundary data at a specified level and area from the ONS OG API
#'
#' @inheritParams create_lookup_table
#' @inheritParams spatial_data_req
#' @param resolution character. See argument options for available options. How
#'  generalised should the boundary be, and whether coastal boundaries should
#'  adhere to the coastline or to the full territorial extent. BGC by default
#'  (G = Generalised (20m), C = limited to the coastline.) F indicates Full
#'  resolution; S indicates Super-generalised (200m); U indicates Ultra-
#'  generalised (500m) boundary resolution. Use E instead of C for full
#'  extent boundaries (e.g. BFE). Not all combinations are available.
#'
#' @examples
#' bounds("msoa", "lad", "Swansea")
#' bounds("msoa", "lad", "Shepway", lookup_year = 2011, within_year = 2015)
#' bounds("rgn", country_filter = "EN", resolution = "BUC")
#' bounds("parish", "lad", "Isles of Scilly")
#' bounds("spr")
#' bounds("npark", within_names = "Bannau Brycheiniog")
#'
#' @returns an `sfc` tibble (data frame with geometry)
#' @export
bounds <- function(
  lookup,
  within = NULL,
  within_names = NULL,
  within_codes = NULL,
  return_width = c("tidy", "full", "minimal"),
  lookup_year = NULL,
  within_year = NULL,
  country_filter = c("UK|GB|EW|EN|SC|WA", "UK", "GB", "EW", "EN", "SC", "WA"),
  option = NULL,
  crs = 4326,
  resolution = c("BGC", "BSC", "BUC", "BFC", "BFE")
) {
  common_spatial(
    lookup,
    within,
    within_names,
    within_codes,
    return_width,
    lookup_year,
    within_year,
    country_filter,
    option,
    crs,
    resolution
  )
}


#' Return centroid points at a specified level and area from the ONS OG API
#'
#' @inheritParams bounds
#'
#' @returns an `sfc` tibble (data frame with geometry)
#' @export
points <- function(
    lookup,
    within = NULL,
    within_names = NULL,
    within_codes = NULL,
    return_width = c("tidy", "full", "minimal"),
    lookup_year = NULL,
    within_year = NULL,
    country_filter = c("UK|GB|EW|EN|SC|WA", "UK", "GB", "EW", "EN", "SC", "WA"),
    option = NULL,
    crs = 4326
) {
  common_spatial(
    lookup,
    within,
    within_names,
    within_codes,
    return_width,
    lookup_year,
    within_year,
    country_filter,
    option,
    crs,
    centroids = TRUE
  )
}


# Helper functions --------------------------------

#' @noRd
build_flat_query <- function(var, vec) {
  y <- stringr::str_flatten(glue::glue("'{unique(vec)}'"), collapse = ",")
  glue::glue("{var} IN ({y})")
}


#' @noRd
batch_it <- function(x, batch_size) {
  f <- rep(1:ceiling(length(x) / batch_size), each = batch_size) |>
    head(length(x))
  split(x, f)
}
