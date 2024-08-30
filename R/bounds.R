#' Return boundary data at a specified level and area from the ONS OG API
#'
#' @inheritParams lookup
#' @param opts Should be set with the `boundr_options()` function. See
#'  `?boundr_options` for detail on what can be set by the user, and on the
#'  default values.
#'
#' @examples
#' bounds("msoa", "lad", "Swansea")
#' bounds("wd", "lad", "Shepway", within_year = 2016) # Shepway no longer exists
#' bounds("rgn", opts = boundr_options(resolution = "BUC"))
#' bounds("par", "lad", "Isles of Scilly") # par = "parish"
#' bounds("spr")
#' bounds("npark", within_names = "Bannau Brycheiniog")
#'
#' @seealso boundr_options
#' @returns an `sfc` tibble (data frame with geometry)
#' @export
bounds <- function(
    lookup_level,
    within_level = NULL,
    within_names = NULL,
    within_codes = NULL,
    lookup_year = NULL,
    within_year = NULL,
    opts = boundr_options()) {
  curr_args <- rlang::call_args(rlang::current_call())
  common_spatial(!!!curr_args)
}

#' Return centroid points at a specified level and area from the ONS OG API
#'
#' @inheritParams bounds
#'
#' @returns an `sfc` tibble (data frame with geometry)
#' @examples
#' points("msoa", "utla", "Swindon")
#' @rdname bounds
#' @export
points <- function(
    lookup_level,
    within_level = NULL,
    within_names = NULL,
    within_codes = NULL,
    lookup_year = NULL,
    within_year = NULL,
    opts = boundr_options()) {
  curr_args <- rlang::call_args(rlang::current_call())
  common_spatial(!!!curr_args, geometry = "centroids")
}

#' Use this to set custom options for `bounds()` and `points()`
#'
#' `opts()` is an alias for this function.
#'
#' @param resolution character. See `res_codes()` for the potential options. How
#'  generalised should the boundary be, and whether coastal boundaries should
#'  adhere to the coastline or to the full territorial extent. BGC by default
#'  (G = Generalised (20m), C = limited to the coastline.) F indicates Full
#'  resolution; S indicates Super-generalised (200m); U indicates Ultra-
#'  generalised (500m) boundary resolution. Use E instead of C for full
#'  extent boundaries (e.g. BFE). Not all combinations are available.
#'  Ignored if geometry is set to "centroids".
#' @param return_width character. How many of the possible columns in the
#'  returned table to keep. Options are "tidy", "full" or "minimal". "Tidy" aims
#'  to return four data columns (usually) - two columns for the lookup level
#'  codes and names, and two for the within level codes and names. Plus a
#'  geometry column. "Full" aims to return all data columns from the lookup.
#'  "Minimal" aims to return just the two data columns relating to
#'  `lookup_level`.
#'  If `within` is not supplied then "tidy" will be equivalent to "minimal".
#' @param query_option numeric. Defaults to 1, which means that the URL will
#'  just be the first one from the list of possible services resulting from the
#'  level and year filters above. If this does not give you what you want, you
#'  can run the script again with a different option from the list.
#' @inheritParams api_data_req
#'
#' @examples
#' boundr_options(geometry = "centroids") # Return centroids! Instead of bounds
#' boundr_options(crs = 27700) # Set the CRS to British National Grid
#' boundr_options(return_width = "full") # Ask boundr to return all data columns
#'
#' @export
boundr_options <- opts <- function(
    resolution = res_codes(),
    return_width = c("tidy", "full", "minimal"),
    crs = 4326,
    query_option = NULL) {
# rs <- if (is_missing(resolution)) res_codes_regex() else arg_match(resolution)
  list(
    rs = arg_match(resolution),
    rw = arg_match(return_width),
    crs = crs,
    opt = query_option
  )
}
