#' Helper Function to Build Queries to the Open Geography API
#'
#' @description A function called by create_custom_lookup.R and geo_get.R to build a valid query
#'
#' @param table_code_ref an integer, passed by the calling function, that indicates which table_code to use
#' @param type geographies can be listed as various types. Here we are just using Census geographies and Administrative geographies. This affects the beginning of the query URL. Census is the default, but Admin can be passed instead where necessary.
#' @param server Some API queries (lookup tables) require the Feature server, others (boundaries) require the Map server
#' @param search_within The area level variable name associated with the locations filter. e.g. \code{"cty19nm"} or \code{"rgn19nm"}
#' @param locations A place name, or list of place names, to filter the data by. If nothing is stipulated then the full unfiltered table will be returned
#' @param fields The fields of the data to be returned. Defaults to \code{"*"} (all); can instead be a set of column names/variables.
#'
#' @return a string that should function as a valid API query
#' @export
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


  # create a list of codes for the main function. Source URLs are included in comments.
  table_codes <- c(

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


  # pull table code from lookup_lookup above
  table_code <- table_codes[table_code_ref]


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
