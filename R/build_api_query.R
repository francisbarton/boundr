#' Helper Function to Build Queries to the Open Geography API
#'
#' @description A function called by create_custom_lookup.R and geo_get.R to build a valid query
#'
#' @param table_code_ref an integer, passed by the calling function, that indicates which table_code to use
#' @param type geographies can be listed as various types. Here we are just using Census geographies and Administrative geographies. This affects the beginning of the query URL. Census is the default, but Admin can be passed instead where necessary.
#' @param server Some API queries (lookup tables) require the Feature server, others (boundaries) require the Map server
#' @param search_within The area level variable name associated with the locations filter. e.g. \code{"cty19nm"} or \code{"rgn19nm"}
#' @param locations A place name, or list of place names or codes, to filter the data by. If nothing is stipulated then the full unfiltered table will be returned
#' @param fields The fields of the data to be returned. Defaults to \code{"*"} (all); can instead be a set of column names/variables.
#'
#' @return a string that should function as a valid API query
build_api_query <- function(
                            table_code_ref,
                            type = "census",
                            server = "feature",
                            search_within,
                            locations = NULL,
                            fields = "*") {


  # create a list of codes for the main function.
  # Source URLs are included as comments.
  table_codes <- c(

    ### LOOKUPS
    #########################################################################

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
    "LSOA11_WD19_LAD19_EW_LU",


    ### BOUNDARIES
    ########################################################################

    # Lower Layer Super Output Areas (December 2011) Boundaries EW BFC
    # https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-boundaries-ew-bfc
    "Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC_v3",

    # Middle Layer Super Output Areas (December 2011) Boundaries EW BFC
    # https://geoportal.statistics.gov.uk/datasets/middle-layer-super-output-areas-december-2011-boundaries-ew-bfc-1
    "Middle_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BFC",

    # Wards (December 2019) Boundaries UK BFC
    # https://geoportal.statistics.gov.uk/datasets/wards-december-2019-boundaries-uk-bfc-1
    "Wards_December_2019_Boundaries_UK_BFC_v2",

    # Local Authority Districts (December 2019) Boundaries UK BFC
    # https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-boundaries-uk-bfc
    "Local_Authority_Districts_December_2019_Boundaries_UK_BFC",

    # "Clinical Commissioning Groups (April 2020) Full Clipped Boundaries EN",
    # "https://geoportal.statistics.gov.uk/datasets/clinical-commissioning-groups-april-2020-full-clipped-boundaries-en",
    # "https://ons-inspire.esriuk.com/arcgis/rest/services/Health_Boundaries/Clinical_Commissioning_Groups_April_2020_EN_BFC_V2/MapServer/1/query?where=1%3D1&outFields=*&outSR=4326&f=json",

    # Counties and Unitaries (Generalised) !!! admin
    # Counties and Unitary Authorities (December 2019) Boundaries UK BGC
    # https://geoportal.statistics.gov.uk/datasets/counties-and-unitary-authorities-december-2019-boundaries-uk-bgc
    "Counties_and_Unitary_Authorities_December_2019_Boundaries_UK_BGC2"

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

  )


  ####################################################################
  # set up commonly used string variables for query URL construction;
  # just for neatness & easier updating. These are just taken from the
  # "API Explorer" tab on each Open Geography Portal page.
  ####################################################################


  # pull table code from list above
  table_code <- table_codes[[table_code_ref]]


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


  # format 'locations' correctly
  # weird that I'm manually putting in percent-encoded strings _before_
  # doing the call to URLencode, but I found that it wasn't encoding
  # all the things as I needed it to for the query to be valid???

  if (is.null(locations)) {
    locations <- "1%3D1"
  } else {
    locations <- locations %>%
      stringr::str_replace_all(" ", "%20") %>%

      # don't think this is needed but it's what the site itself does
      toupper() %>%

      # surround each location in ''
      paste0("'", ., "'") %>%

      stringr::str_c(
      search_within, # area level code eg wd19cd, lad19nm
      "%3D", # "="
      .,     # vector of 'locations'
      sep = "%20",          # Open Geog website puts spaces in so so will I
      collapse = "%20OR%20" # collapse multiple locations with an " OR "
    ) %>%
      utils::URLencode() # not sure this is still needed!
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
  fields_line <- "&outFields="

  # in theory there are several other options that could be customised here
  # if it were worth the candle.
  # maybe I should bother to allow that, in order better to replicate the API
  coda <- "&returnDistinctValues=true&outSR=4326&f=json"

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
    coda
  )
}
