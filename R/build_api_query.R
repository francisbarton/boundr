#' Helper Function to Build Query for Open Geography API
#'
#' @param table_code_ref
#' @param type
#' @param server
#' @param search_within
#' @param locations
#' @param fields
#'
#' @return
#'
#' @examples
#' build_api_query(table_code = "Wards_December_2019_Boundaries_EW_BFC",
#' type = "admin",
#' server = "map"
#' )
#'
#' [1] "https://ons-inspire.esriuk.com/arcgis/rest/services/Administrative_Boundaries/Wards_December_2019_Boundaries_EW_BFC/MapServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"
build_api_query <- function(
  table_code_ref,
  type = "census",
  server = "feature",
  search_within,
  locations = NULL,
  fields = "*") {


  lookup_lookup <- c(

    # "https://geoportal.statistics.gov.uk/datasets/ward-to-local-authority-district-to-county-to-region-to-country-december-2019-lookup-in-united-kingdom"
    "WD19_LAD19_CTY19_OTH_UK_LU",

    # "https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-combined-authority-december-2019-lookup-in-england",
    "LAD19_CAUTH19_EN_LU",

    # A lookup file of the lower tier local authorities
    # (local authority districts, unitary authorities, metropolitan districts,
    # London boroughs) to the upper tier local authorities (counties,
    # metropolitan counties, inner and outer London, unitary authorities)
    # in England and Wales
    # "https://geoportal.statistics.gov.uk/datasets/lower-tier-local-authority-to-upper-tier-local-authority-april-2019-lookup-in-england-and-wales"
    "LTLA19_UTLA19_EW_LU",

    # "https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-area-2011-to-upper-tier-local-authorities-2019-lookup-in-england-and-wales-"
    "LSOA11_UTLA19_EW_LU",

    # "https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-area-2011-to-ward-2019-lookup-in-england-and-wales"
    "LSOA11_WD19_LAD19_EW_LU"

  )


  # set up commonly used string variables for query URL construction;
  # just for neatness & easier updating. These are just taken from the
  # "API Explorer" tab on each Open Geography Portal page.


  # pull table code from lookup_lookup

  # table_code <- lookup_lookup[[table_code_ref]]
  table_code <- lookup_lookup[[5]]


  # type = "census" or "admin"

  if (type == "census") {
    url_base <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/"
    admin <- ""
    map_server <- 1
  }

  if (type == "admin") {
    url_base <- "https://ons-inspire.esriuk.com/"
    admin <- "Administrative_Boundaries/"
    map_server <- 0
  }



  # server string in query URL

  if (server == "feature") {
    server_line <- "/FeatureServer/0/"
  }

  if (server == "map") {
    server_line <- "/MapServer/0/"
  }


  # format locations correctly

  if (is.null(locations)) locations <- "1%3D1"
  else {
    locations <- stringr::str_c(
      search_within,
      "%3D",
      URLencode(paste0("'", toupper(locations), "'")),
      sep = "%20",
      collapse = "%20OR%20"
    )
  }

  fields <- fields %>%
    stringr::str_c(collapse = ",")


  arcgis_base <- "arcgis/rest/services/"
  query_line <- "query?where="
  fields_line <- "&outFields="
  json_coda <- "&returnDistinctValues=true&outSR=4326&f=json"

  # create the query
  paste0(
    url_base,
    arcgis_base,
    admin,
    table_code,
    server_line,
    query_line,
    locations,
    fields_line,
    fields,
    json_coda
  )

}
