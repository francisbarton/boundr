#' Get boundaries for small areas within a local authority area
#'
#' The ONS OpenGeography Portal (\url{https://geoportal.statistics.gov.uk/})
#' is a great resource for area boundary and data lookups within the UK.
#' This program focuses on areas within England and Wales only, initially.
#'
#' \emph{I want a better name for this function! Suggestions welcome...}
#'
#' @inheritParams create_custom_lookup
#' @param spatial_ref The (EPSG) spatial reference of any returned geometry.
#'   Default value: 4326 ("WGS 84"). This parameter is ignored peacefully if
#'   no geometry is returned/returnable, eg lookup queries
#' @param centroid_fields Boolean, default FALSE. Whether to include BNG
#'   eastings, northings, lat and long fields in the return when returning
#'   boundaries.
#' @param shape_fields Boolean, default FALSE. Whether to include
#'   Shape__Area and Shape__Length fields in the return when returning
#'   boundaries.
#' @param return_boundaries whether to retrieve object boundaries data from
#'   the API. Default \code{TRUE}. If \code{return_boundaries} and
#'   \code{return_centroids} are both \code{FALSE}, a plain summary df
#'   without geometry will be returned.
#' @param return_centroids whether to retrieve area centroids instead of
#'   boundaries. Default \code{FALSE}. If set to TRUE then it will override
#'   \code{return_boundaries} whether that was set TRUE or otherwise. If
#'   \code{return_boundaries} and \code{return_centroids} are both \code{FALSE},
#'   a plain summary data frame without geometry will be returned.
#' @param quiet_read Controls quiet parameter to sf::st_read
#'
#' @return a data frame or an sf (simple features) object (data frame
#'   with geometries)
#' @export
#'
#' @examples
#' geo_get("wd", "Swindon", "lad")
#' geo_get("msoa", "Swansea", "lad", return_boundaries = FALSE) %>%
#'   head(10)
#' geo_get("lsoa", "Zetland", "ward", shape_fields = TRUE) %>%
#'   dplyr::arrange(desc(shape_area))
#' geo_get(bounds_level = "lad",
#'   within = "Gloucestershire",
#'   within_level = "cty",
#'   return_style = "simple",
#'   centroid_fields = TRUE,
#'   return_boundaries = FALSE)
geo_get <- function(
                    bounds_level,
                    within,
                    within_level = NULL,
                    include_msoa = NULL,
                    return_style = "tidy",
                    within_cd = FALSE,
                    include_welsh_names = NULL,
                    spatial_ref = 4326,
                    centroid_fields = FALSE,
                    shape_fields = FALSE,
                    return_boundaries = TRUE,
                    return_centroids = FALSE,
                    quiet_read = TRUE) {


  if (within_cd) {

    geo_get_bounds(
      bounds_query_level = bounds_level,
      area_codes = within,
      spatial_ref = spatial_ref,
      centroid_fields = centroid_fields,
      shape_fields = shape_fields,
      return_centroids = return_centroids,
      quiet_read = quiet_read
    )

  } else {


  # get the basic lookup table
  # create_custom_lookup() just inherits its params here from geo_get()
  basic_df <- create_custom_lookup(
    bounds_level = bounds_level,
    within = within,
    within_level = within_level,
    within_cd = within_cd,
    include_msoa = include_msoa,
    return_style = return_style,
    include_welsh_names = include_welsh_names
  )



  # if the user sets 'return_boundaries' FALSE then just return a summary table
  if (!return_boundaries && !return_centroids) {
    return(basic_df)
  }


  area_codes <- basic_df %>%
    dplyr::select(dplyr::ends_with("cd")) %>%
    dplyr::pull(1) %>%
    # According to the API docs, 50 is the limit for geo queries.
    # Excessively long queries return 404.
    batch_it_simple(batch_size = 25) # borrowed from my myrmidon utils package


  bounds_query_level <- basic_df %>%
    dplyr::select(dplyr::ends_with("cd")) %>%
    dplyr::select(1) %>%
    colnames()


  metro_counties <- c(
    "Greater Manchester",
    "Merseyside",
    "South Yorkshire",
    "West Midlands",
    "West Yorkshire",
    "Tyne and Wear"
  )

  if (bounds_query_level %in% c("cty20cd", "utla20cd")) {
    if (within %in% metro_counties) {
      bounds_query_level <- "mcty18cd"
    } else if (within %in% c("Inner London", "Outer London")) {
      usethis::ui_stop("Sorry but boundaries are not available for Inner London and Outer London")
    } else {
      bounds_query_level <- "ctyua19cd"
    }
  }

  # Set it up so that we can stipulate the join fields as 'cd' only, rather than
  # letting dplyr::left_join try to match on multiple fields as it does by
  # default.
  # The reason this matters is that if the ONS makes a small change to the
  # spelling of an area name ('nm') this can mess up the join; but the cd
  # shouldn't change unless there's a fundamental change in the area.

  # create a length 1 named vector ( c(x = x) )
  join_by <- purrr::set_names(bounds_query_level, bounds_query_level)

  # and some exceptions:
  if (bounds_query_level == "ltla20cd") {
    bounds_query_level <- "lad20cd"
    join_by <- c("lad20cd" = "ltla20cd")
  }
  if (bounds_query_level == "rgn20cd") {
    bounds_query_level <- "rgn19cd"
    join_by <- c("rgn19cd" = "rgn20cd")
  }
  if (bounds_query_level == "ctry20cd") {
    bounds_query_level <- "ctry19cd"
    join_by <- c("ctry19cd" = "ctry20cd")
  }


  geo_get_bounds(
    bounds_query_level,
    area_codes,
    spatial_ref,
    centroid_fields,
    shape_fields,
    return_centroids
  ) %>%
  dplyr::left_join(basic_df, by = join_by)
  }
}
