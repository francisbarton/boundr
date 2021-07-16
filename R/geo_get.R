#' Get boundaries for small areas within a local authority area
#'
#' The ONS OpenGeography Portal (\url{https://geoportal.statistics.gov.uk/})
#' is a great resource for area boundary and data lookups within the UK.
#' This program focuses on areas within England and Wales only, initially.
#'
#' \emph{I want a better name for this function! Suggestions welcome...}
#'
#' @inheritParams create_custom_lookup
#' @inheritParams geo_get_bounds
#'
#' @param bounds_level The lowest level at which to return codes and names, eg
#'   "LSOA". Has to be one of "lsoa", "msoa", "wd/ward", "lad",
#'   "cty/county". Case-insensitive.
#' @param within The name of a geographic area to filter by eg "Swindon",
#'   "Gloucestershire", "Wales".
#' @param within_level Upper geographic level to filter at. eg if filtering to
#'   find all LSOAs in a local authority, \code{within_level} will be "lad". Has
#'   to be one of "wd/ward", "lad", "cty/county", "utla/upper", "rgn/region",
#'   "cauth" or "ctry/country". Case-insensitive. Not all combinations of
#'   \code{bounds_level} and \code{within_level} make sense or are possible! NB
#'   "county" includes metropolitan counties such as "Inner London", "Tyne and
#'   Wear" and "West Midlands".
#' @param within_cd Usually you'll build the query with a place name to search
#'   within. But sometimes you may wish to pass in a vector of area codes
#'   instead (if that's all you have, or more likely if you are querying within
#'   wards, which don't have unique names (there's a lot of Abbey wards in
#'   England!)). If you're passing in area codes not names, set this to TRUE.
#' @param include_msoa If \code{bounds_level} = LSOA and return_style is "tidy",
#'   whether to also include MSOA columns (in "tidy" return style). If
#'   \code{bounds_level} is MSOA, this will be forced to \code{TRUE}.
#' @param return_style "tidy" (the default) means all available columns between
#'   \code{bounds_level} and \code{within_level} will be returned, but with any
#'   empty columns removed. "simple" means that only the code and name (cd and
#'   nm) columns for \code{bounds_level} and \code{within_level} are returned -
#'   other columns are omitted. "minimal" means 'only return the columns for
#'   \code{bounds_level}'.
#' @param include_welsh_names Only makes a difference when \code{bounds_level} =
#'   msoa, or when \code{bounds_level} = lsoa and \code{return_style} = "tidy".
#'   \code{FALSE} returns no Welsh language columns. \code{TRUE} attempts to
#'   return Welsh language LAD and MSOA names where relevant. \code{NULL} (the
#'   default) means that an educated decision will be made by the program,
#'   based on whether any of the areas returned have "^W" codes.
#' @param return_boundaries whether to retrieve object boundaries data from
#'   the API. Default \code{TRUE}. If \code{return_boundaries} and
#'   \code{return_centroids} are both \code{FALSE}, a plain summary df
#'   without geometry will be returned.
#'
#' @return a data frame or an sf (simple features) object (data frame
#'   with geometries)
#' @export
#'
#' @examples
#' geo_get("wd", "Swindon", "lad")
#' geo_get("msoa", "Swansea", "lad", centroid_fields = TRUE) %>%
#'   head(10)
#' geo_get("lsoa", "Zetland", "ward", shape_fields = TRUE)
#' geo_get(bounds_level = "lad",
#'   within = "Gloucestershire",
#'   within_level = "cty",
#'   return_style = "simple",
#'   return_boundaries = FALSE)
geo_get <- function(bounds_level,
                    within,
                    within_level = NULL,
                    within_cd = FALSE,
                    include_msoa = NULL,
                    return_style = "tidy",
                    include_welsh_names = NULL,
                    return_boundaries = TRUE,
                    return_centroids = FALSE,
                    centroid_fields = FALSE,
                    shape_fields = FALSE,
                    spatial_ref = 4326,
                    quiet_read = TRUE) {


  # centroids query doesn't include any higher level fields
  if (return_centroids) {
    return_boundaries <- FALSE
    return_style <- "minimal"
  }



  if (within_cd) {

    basic_df <- dplyr::as_tibble(!!bounds_level := within)

  } else {

    # get the basic lookup table
    basic_df <- create_custom_lookup(
      bounds_level = bounds_level,
      within = within,
      within_level = within_level,
      within_cd = within_cd,
      include_msoa = include_msoa,
      return_style = return_style,
      include_welsh_names = include_welsh_names
    )
  }



  # if the user sets 'return_boundaries' FALSE then just return a summary table
  if (!return_boundaries & !return_centroids) {
    basic_df # return
  } else {

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

    if (bounds_query_level %in% c("cty20cd", "utla21cd")) {
      if (within %in% metro_counties) {
        bounds_query_level <- "mcty18"
      } else if (within %in% c("Inner London", "Outer London")) {
        usethis::ui_stop(
          "Sorry, but boundaries are not available for Inner and Outer London")
      } else {
        bounds_query_level <- "ctyua"
      }
    }

    if (!within_cd) {
      within <- basic_df %>%
        dplyr::select(dplyr::ends_with("cd")) %>%
        dplyr::pull(1)
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
    if (bounds_query_level == "ltla21cd") {
      bounds_query_level <- "lad20cd"
      join_by <- c("lad20cd" = "ltla21cd")
    }


    geo_get_bounds(
      bounds_query_level = bounds_level,
      area_codes = within,
      return_centroids = return_centroids,
      centroid_fields = centroid_fields,
      shape_fields = shape_fields,
      spatial_ref = spatial_ref,
      quiet_read = quiet_read
    ) %>%
      dplyr::left_join(basic_df, by = join_by) %>%
      dplyr::relocate(names(basic_df))
  }
}
