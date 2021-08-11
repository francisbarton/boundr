#' Get boundaries for smaller areas within a larger area
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
#' @param return_boundaries whether to retrieve object boundaries data from
#'   the API. Default \code{TRUE}. If \code{return_boundaries} and
#'   \code{return_centroids} and \code{centroid_fields} are all \code{FALSE}, a
#'   plain summary df without any geometry will be returned.
#'
#' @return a data frame or an sf (simple features) object (data frame
#'   with geometries)
#' @export
#'
#' @examples
#' geo_get("wd", "Swindon", "lad")
#' geo_get("msoa", "Swansea", "lad", return_centroids = TRUE) %>%
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
                    bounds_cd = FALSE,
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


  if (return_centroids) {
    return_boundaries <- FALSE
    centroid_fields <- FALSE
    shape_fields <- FALSE
  }


  # get the basic lookup table
  basic_df <- create_custom_lookup(
    bounds_level,
    within,
    within_level,
    bounds_cd,
    within_cd,
    include_msoa,
    return_style,
    include_welsh_names
  )



  # if the user sets 'return_boundaries' FALSE then just return a summary table
  if (!return_boundaries && !return_centroids && !centroid_fields) {
    basic_df # return
  } else {

    bounds_query_level_orig <- basic_df %>%
      dplyr::select(dplyr::ends_with("cd")) %>%
      dplyr::select(1) %>%
      colnames()

    bounds_query_level_new <- bounds_query_level_orig

    metro_counties <- c(
      "Greater Manchester",
      "Merseyside",
      "South Yorkshire",
      "West Midlands",
      "West Yorkshire",
      "Tyne and Wear"
    )

    if (bounds_query_level_orig %in% c("cty20cd", "utla21cd")) {
      if (within %in% metro_counties) {
        bounds_query_level_new <- "mcty18cd"
      } else if (within %in% c("Inner London", "Outer London")) {
        usethis::ui_stop(
          "Sorry, but boundaries are not available for Inner and Outer London")
      } else {
        bounds_query_level_new <- "ctyua20cd"
      }
    }

    if (bounds_query_level_orig == "ltla21cd") {
      bounds_query_level_new <- "lad21cd"
    }


    if (!bounds_cd) {
      within <- basic_df %>%
        dplyr::select(dplyr::ends_with("cd")) %>%
        dplyr::pull(1)
    }


    # Set it up so that we can stipulate the join fields as 'cd' only,
    # rather than letting dplyr::left_join try to match on multiple fields,
    # as it does by default.
    # The reason this matters is that if the ONS makes a small change to the
    # spelling of an area name ('nm') this can mess up the join; but the cd
    # shouldn't change unless there's a fundamental change in the area.

    geo_get_bounds(
      bounds_query_level = bounds_query_level_new,
      area_codes = within,
      return_centroids,
      centroid_fields,
      shape_fields,
      spatial_ref,
      quiet_read
    ) %>%
      dplyr::rename(!!bounds_query_level_orig := 1) %>%
      dplyr::left_join(basic_df, by = bounds_query_level_orig) %>%
      dplyr::relocate(names(basic_df))
  }
}
