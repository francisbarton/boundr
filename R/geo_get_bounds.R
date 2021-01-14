#' Return boundary geometries for supplied list of area codes
#'
#' @inheritParams create_custom_lookup
#' @inheritParams geo_get
#' @param bounds_query_level area level to query for, e.g. "lsoa11cd". Needs to
#'   be the full code not an abbreviation such as "lsoa"
#' @param area_codes a vector of codes that match the level of
#'   bounds_query_level. Probably to be supplied from a column pulled from a
#'   lookup df or similar.
#' @return an sf object
#' @export
geo_get_bounds <- function(bounds_query_level,
                           area_codes,
                           return_style = "tidy",
                           spatial_ref = 4326,
                           centroid_fields = FALSE,
                           shape_fields = FALSE,
                           return_centroids = FALSE) {

  centroid_fields_list <- NULL
  if (centroid_fields) {
    centroid_fields_list <- c(
      "BNG_E",
      "BNG_N",
      "LONG_",
      "LAT"
    )
  }


  shape_fields_list <- NULL
  if (shape_fields) {
    shape_fields_list <- c(
      "Shape__Area",
      "Shape__Length"
    )
  }


  return_fields <- c(
    bounds_query_level,
    centroid_fields_list,
    shape_fields_list
  )



  table_code_ref_lookup <- dplyr::tribble(
    ~bounds_level, ~table_code_ref, ~type, ~server, ~centroids,

    "lsoa11cd",   6,    "census",     "feature",   FALSE,
    "msoa11cd",   7,    "census",     "feature",   FALSE,
    "wd20cd",     8,    "census",     "feature",   FALSE,
    "lad20cd",    9,    "census",     "feature",   FALSE,
    "ctyua19cd", 10,    "admin",      "map",       FALSE,
    "mcty18cd",  11,    "other",      "map",       FALSE,
    "msoa11cd",  12,    "centroid",   "map",       TRUE
  )


  table_code_refs <- table_code_ref_lookup %>%
    dplyr::filter(bounds_level == bounds_query_level) %>%
    # centroids is used here to filter, this is why the setting of
    # return_centroids as TRUE will override the setting of boundaries to TRUE
    dplyr::filter(centroids == return_centroids)

  bounds_queries <- area_codes %>%
    batch_it_simple(batch_size = 25) %>%
    purrr::map(~ build_api_query(
      table_code_ref = table_code_refs[["table_code_ref"]],
      type = table_code_refs[["type"]],
      server = table_code_refs[["server"]],
      within_level = bounds_query_level,
      within = .,
      fields = return_fields,
      sr = spatial_ref
    ))

  bounds_queries %>%
    purrr::map_df(sf::st_read) %>%
    janitor::clean_names()
}
