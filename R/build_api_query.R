#' Helper Function to Build Queries to the Open Geography API
#'
#' A function called by create_custom_lookup.R and geo_get.R to build a
#' valid query
#'
#' @param ref an integer, passed by the calling function, that
#'   indicates which url_string to use
#' @param type geographies can be listed as various types. Here we are just
#'   using Census geographies and Administrative geographies. This affects the
#'   beginning of the query URL. "census" is the default.
#' @param within_level The area level variable name associated with the
#'   locations filter. e.g. \code{"cty19nm"} or \code{"rgn19nm"}
#' @param within A place name, or list of place names or codes, to filter the
#'   data by. If nothing is stipulated then the full unfiltered table will be
#'   returned
#' @param fields The fields of the data to be returned. Defaults to \code{"*"}
#'   (all); can instead be a set of column names/variables.
#' @param return_geometry Whether to return a geometry
#' @param sr The (EPSG) spatial reference of any returned geometry.
#'   4326 ("WGS 84") by default. Can be specified as numeric or character.
#'
#' @return a string that should function as a valid API query
#' @export
#' @examples
#' build_api_query(ref = 4,
#'   within_level = "cauth20nm",
#'   within = "Greater Manchester",
#'   fields = c("lad20cd", "lad20nm", "cauth20cd", "cauth20nm")
#' )
#' build_api_query(ref = 12,
#'   within_level = "lad20nm",
#'   within = c(
#'     "Cheltenham", "Gloucester",
#'     "Stroud", "Cotswold",
#'     "Tewkesbury", "Forest of Dean"
#'   ),
#'   fields = c("lad20cd", "lad20nm")
#' )
build_api_query <- function(ref,
                            type = "census",
                            within_level,
                            within = NULL,
                            fields = "*",
                            return_geometry = TRUE,
                            sr = 4326) {


  # TODO: set up a test/check for all URLs here, to auto-flag if the specific
  # code has changed (if ONS have updated and versioned their data).

  # create a list of codes for the main function.
  # Source URLs are included as comments.
  url_strings <- c(

    ### LOOKUPS (1 - 7)
    #########################################################################

    # https://geoportal.statistics.gov.uk/datasets/output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2020-lookup-in-england-and-wales
    "OA11_LSOA11_MSOA11_LAD20_RGN20_EW_LU",

    # https://geoportal.statistics.gov.uk/datasets/output-area-to-ward-to-local-authority-district-december-2020-lookup-in-england-and-wales-v2
    "OA11_WD20_LAD20_EW_LU_v2",

    # https://geoportal.statistics.gov.uk/datasets/ons::ward-to-local-authority-district-to-county-to-region-to-country-december-2020-lookup-in-united-kingdom-v2
    "WD20_LAD20_CTY20_OTH_UK_LU_v2",

    # https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-combined-authority-december-2020-lookup-in-england
    "LAD20_CAUTH20_EN_LU",

    # https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-area-2011-to-upper-tier-local-authorities-2021-lookup-in-england-and-wales-
    "LSOA11_UTLA21_EW_LU",

    # https://geoportal.statistics.gov.uk/datasets/ons::lower-tier-local-authority-to-upper-tier-local-authority-april-2021-lookup-in-england-and-wales
    "LTLA21_UTLA21_EW_LU",

    # https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-area-2011-to-ward-2020-to-lad-2020-lookup-in-england-and-wales-v2/
    "LSOA11_WD20_LAD20_EW_LU_v2",


    ### BOUNDARIES (8 - 15)
    ########################################################################

    # Output Areas (December 2011) Boundaries EW BFC
    "Output_Areas_December_2011_Boundaries_EW_BFC",

    # Lower Layer Super Output Areas (December 2011) Boundaries Full Clipped (BFC) EW V3
    "Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC_V2",

    # Middle Layer Super Output Areas (December 2011) Boundaries Full Clipped (BFC) EW V3
    "Middle_Layer_Super_Output_Areas_December_2011_EW_BFC_V2",

    # Wards (December 2020) Boundaries UK BFC
    "Wards_December_2020_UK_BFC_V3",

    # Local Authority Districts (December 2020) Boundaries UK BFC
    "Local_Authority_Districts_December_2020_UK_BFC",

    # Counties and Unitaries
    "Counties_and_Unitary_Authorities_December_2020_UK_BGC_V2",

    # Regions (December 2020) UK BUC
    "Regions_December_2020_EN_BUC_V2",

    # Countries (December 2020) UK BUC
    "Countries_December_2020_UK_BUC_V3",



    ### CENTROIDS (16)
    ##################################################################

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
  url_string <- url_strings[[ref]]

  url_base <- "https://ons-inspire.esriuk.com/"

  distinct <- "&returnDistinctValues=true"

  if (type == "census") {
    url_base <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/"
    admin <- ""
  }

  if (type == "admin") {
    admin <- "Administrative_Boundaries/"
  }

  if (type == "other") {
    admin <- "Other_Boundaries/"
  }

  if (type == "centroid") {
    admin <- "Census_Boundaries/"
    distinct <- ""
  }


  if (return_geometry) {
    geom_line <- ""
  } else {
    geom_line <- "&returnGeometry=false"
  }


  # server string in query URL
  # if (server == "feature") {
  #   server_line <- "/FeatureServer/0/"
  # }
  #
  # if (server == "map") {
  #   server_line <- "/MapServer/0/"
  # }

  # upper or lower case field names? it seems to depend on the server, or
  # maybe the type, haven't managed to check yet.
  # NB the queries seem to actually work fine either way, I am just going
  # for 100% fidelity for the sake of the function tests!

  # if (server == "feature") {
  fields <- toupper(fields)
  within_level <- toupper(within_level)
  # }

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
      # ' seems to be OK without being escaped as %27 in queries
      paste0("'", ., "'") %>%
      stringr::str_c(
        within_level, # area level code eg WD19CD, CAUTH19NM
        # "%3D", # "="
        "=", # trying this instead of %3D doesn't seem to matter
        .,     # vector of 'within'
        # using "+" instead of a space also seems to be good for the API
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


  # for simple lookup queries  we can use "standard" result type;
  # CRS is irrelevant
  # "7" will need to be changed if list above incorporates more lookup options
  if (ref %in% 1:7) {
    result_type <- "standard"
    sr_line <- ""
  } else {

    # this result_type is needed for spatial queries, "standard" doesn't agree
    # see examples six and seven here:
    # https://developers.arcgis.com/rest/services-reference/enterprise/query-feature-service-layer-.htm
    result_type <- "none"
    sr_line <- paste0("&outSR=", sr)
  }


  arcgis_base <- "arcgis/rest/services/"

  # they all seem to be this since recent changes
  server_line <- "/FeatureServer/0/"

  query_line <- "query?where="
  within_open <- "%20("
  within_close <- ")%20"
  fields_line <- "&outFields="
  result_type_line <- "&resultType="
  return_format <- "&f=json"

  # in theory there are several other options that could be customised here
  # if it were worth the candle.
  # maybe I should bother to allow that, in order better to replicate the API

  # create the query
  paste0(
    url_base,
    arcgis_base,
    admin,
    url_string,
    server_line,
    query_line,
    within_open,
    within,
    within_close,
    fields_line,
    fields,
    sr_line,
    result_type_line,
    result_type,
    geom_line,
    distinct,
    return_format
  )
}
