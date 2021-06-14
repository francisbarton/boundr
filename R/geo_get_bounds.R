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
                           spatial_ref = 4326,
                           centroid_fields = FALSE,
                           shape_fields = FALSE,
                           return_centroids = FALSE,
                           quiet_read = TRUE) {


  area_code_lookup <- dplyr::tribble(
    ~friendly, ~serious,
    "oa",      "oa11",
    "coa",     "oa11",
    "lsoa",    "lsoa11",
    "msoa",    "msoa11",
    "wd",      "wd20",
    "ward",    "wd20",
    "lad",     "lad20",
    "ltla",    "ltla21",
    "utla",    "utla21",
    "upper",   "utla21",
    "cty",     "cty20",
    "county",  "cty20",
    "cauth",   "cauth20",
    "rgn",     "rgn20",
    "region",  "rgn20",
    "ctry",    "ctry20",
    "country", "ctry20"
  )


  get_serious <- function(x) {
    area_code_lookup %>%
      dplyr::filter(friendly %in% tolower(x)) %>%
      dplyr::pull(serious)
  }


  # create a vector of field codes from the query level supplied

  if (bounds_query_level %in% area_code_lookup$friendly) {

    cd_field <- bounds_query_level %>%
      get_serious() %>%
      paste0("cd")
  } else {
    cd_field <- bounds_query_level

  }


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
    cd_field,
    centroid_fields_list,
    shape_fields_list
  )



  table_code_ref_lookup <- dplyr::tribble(
    ~bounds_level, ~table_code_ref, ~type, ~server, ~centroids,

    "oa11cd",      8,    "census",     "feature",   FALSE,
    "lsoa11cd",    9,    "census",     "feature",   FALSE,
    "msoa11cd",   10,    "census",     "feature",   FALSE,
    "wd20cd",     11,    "census",     "feature",   FALSE,
    "lad20cd",    12,    "census",     "feature",   FALSE,
    "ctyua20cd",  13,    "census",     "feature",   FALSE,
    "rgn20cd",    14,    "census",     "feature",   FALSE,
    "ctry20cd",   15,    "census",     "feature",   FALSE,
    "msoa11cd",   16,    "centroid",   "feature",   TRUE
  )


  table_code_refs <- table_code_ref_lookup %>%
    dplyr::filter(bounds_level == cd_field) %>%
    # centroids is used here to filter, this is why the setting of
    # return_centroids as TRUE will override the setting of boundaries to TRUE
    dplyr::filter(centroids == return_centroids)

  bounds_queries <- area_codes %>%
    batch_it_simple(batch_size = 25) %>%
    purrr::map(~ build_api_query(
      table_code_ref = table_code_refs[["table_code_ref"]],
      type = table_code_refs[["type"]],
      server = table_code_refs[["server"]],
      within_level = cd_field,
      within = .,
      fields = return_fields,
      sr = spatial_ref
    ))

  bounds_queries %>%
    purrr::map_df(~ sf::st_read(., quiet = quiet_read)) %>%
    janitor::clean_names()
}
