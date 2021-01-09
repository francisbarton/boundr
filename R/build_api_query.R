#' Helper Function to Build Queries to the Open Geography API
#'
#' A function called by create_custom_lookup.R and geo_get.R to build a
#' valid query
#'
#' @param table_code_ref an integer, passed by the calling function, that
#'   indicates which table_code to use
#' @param type geographies can be listed as various types. Here we are just
#'   using Census geographies and Administrative geographies. This affects the
#'   beginning of the query URL. "census" is the default, but "admin" or "other"
#'   can be passed instead where necessary.
#' @param server Some API queries (lookup tables) require the Feature server,
#'   others (boundaries) require the Map server
#' @param within_level The area level variable name associated with the
#'   locations filter. e.g. \code{"cty19nm"} or \code{"rgn19nm"}
#' @param within A place name, or list of place names or codes, to filter the
#'   data by. If nothing is stipulated then the full unfiltered table will be
#'   returned
#' @param fields The fields of the data to be returned. Defaults to \code{"*"}
#'   (all); can instead be a set of column names/variables.
#' @param sr The (EPSG) spatial reference of any returned geometry.
#'   4326 ("WGS 84") by default. Can be specified as numeric or character.
#' @param distinct Boolean. Whether to enable "returnDistinctValues" as part of
#'   the query. Seems like a good idea for lookups but a problem for boundaries.
#'
#' @return a string that should function as a valid API query
#' @export
#' @examples
#' build_api_query(
#'   table_code_ref = 2,
#'   type = "census",
#'   server = "feature",
#'   within_level = "cauth19nm",
#'   within = "Greater Manchester",
#'   fields = c("lad19cd", "lad19nm", "cauth19cd", "cauth19nm")
#' )
#' build_api_query(
#'   table_code_ref = 9,
#'   type = "admin",
#'   server = "map",
#'   within_level = "lad19nm",
#'   within = c(
#'     "Cheltenham", "Gloucester",
#'     "Stroud", "Cotswold",
#'     "Tewkesbury", "Forest of Dean"
#'   ),
#'   fields = c("lad19cd", "lad19nm")
#' )
build_api_query <- function(
                            table_code_ref,
                            type = "census",
                            server = "feature",
                            within_level,
                            within = NULL,
                            fields = "*",
                            sr = 4326,
                            distinct = TRUE) {


  # create a list of codes for the main function.
  # Source URLs are included as comments.
  table_codes <- c(

    ### LOOKUPS
    #########################################################################

    # https://geoportal.statistics.gov.uk/datasets/ward-to-local-authority-district-to-county-to-region-to-country-december-2020-lookup-in-united-kingdom/geoservice
    "WD20_LAD20_CTY20_OTH_UK_LU",

    # https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-combined-authority-december-2020-lookup-in-england
    "LAD20_CAUTH20_EN_LU",

    # https://geoportal.statistics.gov.uk/datasets/fd6a688548744c0d9ec34b1273b80c20_0/
    "LSOA11_UTLA20_EW_LU",

    # https://geoportal.statistics.gov.uk/datasets/7a9e4c5e7e8847b8b6a1ac93acd66358_0
    "LSOA11_WD20_LAD20_EW_LU",


    ### BOUNDARIES
    ########################################################################

    # Lower Layer Super Output Areas (December 2011) Boundaries EW BFC
    # https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-boundaries-ew-bfc
    "Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC_V2",

    # Middle Layer Super Output Areas (December 2011) Boundaries EW BFC
    # https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-ew-bfc-1
    "Middle_Layer_Super_Output_Areas_December_2011_EW_BFC_V2",

    # Wards (December 2020) Boundaries UK BFC
    # https://geoportal.statistics.gov.uk/datasets/wards-december-2020-uk-bfc
    "Wards_December_2020_UK_BFC",

    # Local Authority Districts (May 2020) Boundaries UK BFC
    # https://geoportal.statistics.gov.uk/datasets/local-authority-districts-may-2020-boundaries-uk-bfc-1
    "Local_Authority_Districts_May_2020_Boundaries_UK_BFC_V3",

    # Clinical Commissioning Groups (April 2020) Full Clipped Boundaries EN
    # "https://geoportal.statistics.gov.uk/datasets/clinical-commissioning-groups-april-2020-full-clipped-boundaries-en",
    # "https://ons-inspire.esriuk.com/arcgis/rest/services/Health_Boundaries/Clinical_Commissioning_Groups_April_2020_EN_BFC_V2/MapServer/1/query?where=1%3D1&outFields=*&outSR=4326&f=json",

    # Counties and Unitaries
    # https://geoportal.statistics.gov.uk/datasets/counties-and-unitary-authorities-december-2019-boundaries-uk-bfc
    # Counties and Unitary Authorities (December 2019) Boundaries UK BFC
    "Counties_and_Unitary_Authorities_December_2019_Boundaries_UK_BFC2",

    # Metropolitan Counties (Full)
    # Metropolitan Counties (December 2018) EN BFC
    # https://geoportal.statistics.gov.uk/datasets/metropolitan-counties-december-2018-en-bfc
    "Metropolitan_Counties_December_2018_EN_BFC",

    # Regions (Generalised) !!! admin
    # Regions (December 2019) Boundaries EN BGC
    # https://geoportal.statistics.gov.uk/datasets/regions-december-2019-boundaries-en-bgc
    # "rgn19cd",
    # "Regions_December_2019_Boundaries_EN_BGC",
    # "admin",
    # "map"

    # Countries (UK) (Generalised) !!! admin
    # Countries (December 2019) Boundaries UK BGC
    # https://geoportal.statistics.gov.uk/datasets/countries-december-2019-boundaries-uk-bgc
    # "ctry",
    # "Countries_December_2019_Boundaries_UK_BGC",
    # "admin",
    # "map"


    ### CENTROIDS
    ########################################################################

    # Middle Layer Super Output Areas (December 2011) Population Weighted Centroids
    # https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-population-weighted-centroids
    "Middle_Super_Output_Areas_December_2011_Centroids"
    )


  ####################################################################
  # set up commonly used string variables for query URL construction;
  # just for neatness & easier updating. These are just taken from the
  # "API Explorer" tab on each Open Geography Portal page.
  ####################################################################


  # pull table code from list above
  table_code <- table_codes[[table_code_ref]]


  if (type == "census") {
    url_base <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/"
    admin <- ""
  }

  if (type == "admin") {
    url_base <- "https://ons-inspire.esriuk.com/"
    admin <- "Administrative_Boundaries/"
  }

  if (type == "other") {
    url_base <- "https://ons-inspire.esriuk.com/"
    admin <- "Other_Boundaries/"
  }

  if (type == "centroid") {
    url_base <- "https://ons-inspire.esriuk.com/"
    admin <- "Census_Boundaries/"
  }



  assertthat::assert_that(server %in% c("feature", "map"))

  # server string in query URL
  if (server == "feature") {
    server_line <- "/FeatureServer/0/"
  }

  if (server == "map") {
    server_line <- "/MapServer/0/"
  }


  # upper or lower case field names? it seems to depend on the server, or
  # maybe the type, haven't managed to check yet.
  # NB the queries seem to actually work fine either way, I am just going
  # for 100% fidelity for the sake of the function tests!

  if (server == "feature") {
    fields <- toupper(fields)
    within_level <- toupper(within_level)
  }

  # format 'locations' correctly
  # I'm manually putting in percent-encoded strings instead of calling
  # utils::URLencode because I found that it wasn't encoding
  # all the things as I needed it to for the query to be valid

  if (is.null(within)) {
    within <- "1%3D1"
  } else {
    within <- within %>%
      stringr::str_replace_all(" ", "%20") %>%

      # don't think this is needed but it's what the site itself does
      toupper() %>%

      # surround each location in ''
      paste0("%27", ., "%27") %>%
      stringr::str_c(
        within_level, # area level code eg WD19CD, CAUTH19NM
        "%3D", # "="
        .,     # vector of 'within'
        sep = "%20", # Open Geog website puts spaces in, so so will I
        collapse = "%20OR%20" # collapse multiple locations with an " OR "
      )
  }


  # collapse a vector of fields to a single string
  # (it should usually be more than one)
  # fields is the columns to retrieve, if only some are wanted
  if (length(fields) > 1) {
    fields <- fields %>%
      stringr::str_c(collapse = ",")
  }


  arcgis_base <- "arcgis/rest/services/"
  query_line <- "query?where="
  within_open <- "%20("
  within_close <- ")%20"
  fields_line <- "&outFields="

  # in theory there are several other options that could be customised here
  # if it were worth the candle.
  # maybe I should bother to allow that, in order better to replicate the API

  if (distinct) distinct_val <- "&returnDistinctValues=true"
  if (!distinct) distinct_val <- ""

  coda <- paste0(distinct_val,
                 "&outSR=",
                 sr,
                 "&f=json")

  # create the query
  paste0(
    url_base,
    arcgis_base,
    admin,
    table_code,
    server_line,
    query_line,
    within_open,
    within,
    within_close,
    fields_line,
    fields,
    coda
  )
}
