#' Get boundaries for small areas within a local authority area
#'
#' The ONS OpenGeography Portal (https://geoportal.statistics.gov.uk/)
#' is a great resource for area boundary and data lookups within the UK.
#' This program focuses on areas within England and Wales only, initially.
#'
#' @param place Name of an area such as a local authority
#' @param search_within Area level of 'place', LAD by default (lad19nm)
#' @param return_areas Sub-areas to be retrieved
#' @param boundaries Boolean, default TRUE. Whether to return geometry as well as lookup df
#' @param minimal Boolean, default FALSE. Whether to return all columns of lookup df or just the columns for the specified area and sub-area levels
#' @importFrom dplyr pull
#' @importFrom jsonlite fromJSON
#' @importFrom purrr pluck
#' @importFrom sf st_read
#' @importFrom stringr str_c
#'
#' @return an sf (simple features) object
#' @export
#'
#' @examples
#'
#'




geo_get <- function(
  place = NA,
  search_within = "lad19",
  return_areas = "wd19",
  boundaries = TRUE,
  minimal = FALSE) {


  bounds_lookup_table <- tribble(
    ~ level, ~ title, ~ code, ~ reference_url,

    # Postcode Centroids
    # "postcode",
    # "ONSPD Centroids Lite",
    # "https://ons-inspire.esriuk.com/arcgis/rest/services/Postcodes/ONSPD_Centroids_Lite/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json",
    # "https://geoportal.statistics.gov.uk/datasets/onspd-centroids-lite",

    # OAs (Generalised)
    # "oa",
    # "Output Areas (December 2011) Boundaries EW BGC",
    # "Output_Areas_December_2011_Boundaries_EW_BGC",
    # "https://geoportal.statistics.gov.uk/datasets/output-areas-december-2011-boundaries-ew-bgc-1",

    # LSOAs (Full resolution)
    "lsoa",
    "Lower Layer Super Output Areas (December 2011) Boundaries EW BFC",
    "Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC_v3",
    "https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-boundaries-ew-bfc",

    # MSOAs (Full resolution)
    "msoa",
    "Middle Layer Super Output Areas (December 2011) Boundaries EW BFC",
    "Middle_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC",
    "https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-ew-bfc-1",

    # Wards (Full resolution)
    "ward",
    "Wards (December 2019) Boundaries UK BFC",
    "Wards_December_2019_Boundaries_UK_BFC_v2",
    "https://geoportal.statistics.gov.uk/datasets/wards-december-2019-boundaries-uk-bfc-1",

    # Local Authorities (Generalised)
    "lad",
    "Local Authority Districts (May 2020) Boundaries UK BGC",
    "Local_Authority_Districts_May_2020_UK_BGC_V3",
    "https://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2020-boundaries-uk-bgc-1",

    # CCGs (Full)
    "ccg",
    "Clinical Commissioning Groups (April 2020) Full Clipped Boundaries EN",
    "https://ons-inspire.esriuk.com/arcgis/rest/services/Health_Boundaries/Clinical_Commissioning_Groups_April_2020_EN_BFC_V2/MapServer/1/query?where=1%3D1&outFields=*&outSR=4326&f=json",
    "https://geoportal.statistics.gov.uk/datasets/clinical-commissioning-groups-april-2020-full-clipped-boundaries-en",

    # Counties and Unitaries (Generalised) !!! admin - different URL format
    "utla",
    "Counties and Unitary Authorities (December 2019) Boundaries UK BGC",
    "https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Counties_and_Unitary_Authorities_December_2019_Boundaries_UK_BGC2/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json",
    "https://geoportal.statistics.gov.uk/datasets/counties-and-unitary-authorities-december-2019-boundaries-uk-bgc",

    # Regions (Generalised) !!! admin - different URL format
    "rgn",
    "Regions (December 2019) Boundaries EN BGC",
    "https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Regions_December_2019_Boundaries_EN_BGC/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json",
    "https://geoportal.statistics.gov.uk/datasets/regions-december-2019-boundaries-en-bgc",

    # Countries (UK) (Generalised) !!! admin - different URL format
    "ctry",
    "Countries (December 2019) Boundaries UK BGC",
    "https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Countries_December_2019_Boundaries_UK_BGC/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json",
    "https://geoportal.statistics.gov.uk/datasets/countries-december-2019-boundaries-uk-bgc"


  )


  areas_query_string <- get_lookup_table() %>%
    dplyr::pull(area_return) %>%
    paste0("'", ., "'") %>%
    paste0(area_return, " = ", .) %>%
    stringr::str_c(collapse = " OR ") %>%
    paste0("(", ., ")")

  paste0(
    arcgis_base,
    "Wards_December_2019_Boundaries_UK_BFC_v2",
    query_line,
    areas_query_string,
    json_coda
  ) %>%
    URLencode() %>%
    sf::st_read()

}

# test_out <- get_ward_bounds("Ealing")
# tmap::tm_shape(test_out) + tm_borders()
