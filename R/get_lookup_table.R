lookup_lookup_table <- tribble(
  ~ title, ~code, ~reference_url,

  # provides LSOA11 -> LAD19 lookup
  # "Index of Multiple Deprivation (December 2019) Lookup in England",
  # "Index_of_Multiple_Deprivation_December_2019_Lookup_in_England",
  # "https://geoportal.statistics.gov.uk/datasets/index-of-multiple-deprivation-december-2019-lookup-in-england",
  #
  # "Index of Multiple Deprivation (December 2019) Lookup in Wales",
  # "Index_of_Multiple_Deprivation_December_2019_Lookup_in_Wales",
  # "https://geoportal.statistics.gov.uk/datasets/index-of-multiple-deprivation-december-2019-lookup-in-wales",
  #
  # "Index of Multiple Deprivation (December 2016) Lookup in Scotland",
  # "Index_of_Multiple_Deprivation_December_2016_Lookup_in_Scotland",
  # "https://geoportal.statistics.gov.uk/datasets/index-of-multiple-deprivation-december-2016-lookup-in-scotland",

  # provides LSOA11 -> WD19 and LAD19 lookup
  "Lower Layer Super Output Area (2011) to Ward (2019) Lookup in England and Wales",
  "LSOA11_WD19_LAD19_EW_LU",
  "https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-area-2011-to-ward-2019-lookup-in-england-and-wales",

  # provides LSOA11 -> UTLA19 lookup
  # "Lower Layer Super Output Area (2011) to Upper Tier Local Authorities (2019) Lookup in England and Wales",
  # "LSOA11_UTLA19_EW_LU",
  # # rogue hyphen at end of URL :-/
  # "https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-area-2011-to-upper-tier-local-authorities-2019-lookup-in-england-and-wales-",
  #
  # "Ward to Local Authority District (December 2019) Lookup in the United Kingdom",
  # "WD19_LAD19_UK_LU",
  # "https://geoportal.statistics.gov.uk/datasets/ward-to-local-authority-district-december-2019-lookup-in-the-united-kingdom",

  # A lookup file of the lower tier local authorities
  # (local authority districts, unitary authorities, metropolitan districts,
  # London boroughs) to the upper tier local authorities (counties,
  # metropolitan counties, inner and outer London, unitary authorities)
  # in England and Wales
  "Lower Tier Local Authority to Upper Tier Local Authority (April 2019) Lookup in England and Wales",
  "LTLA19_UTLA19_EW_LU",
  "https://geoportal.statistics.gov.uk/datasets/lower-tier-local-authority-to-upper-tier-local-authority-april-2019-lookup-in-england-and-wales",

  # includes April 2020 changes: Buckinghamshire two-tier to unitary
  # "Local Authority District to County (April 2020) Lookup in England",
  # "LAD20_CTY20_EN_LU",
  # "https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-county-april-2020-lookup-in-england",
  #
  "Local Authority District to Combined Authority (December 2019) Lookup in England",
  "LAD19_CAUTH19_EN_LU",
  "https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-combined-authority-december-2019-lookup-in-england",

  "Local Authority District to Region (April 2019) Lookup in England",
  "LAD19_RGN19_EN_LU",
  "https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-region-april-2019-lookup-in-england"


)

# "Output Area to Lower Layer Super Output Area to Middle Layer Super Output Area to Local Authority District (December 2011) Lookup in England and Wales",
# "OA11_LSOA11_MSOA11_LAD11_EW_LUv2",
# "https://geoportal.statistics.gov.uk/datasets/output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2011-lookup-in-england-and-wales/geoservice"

# https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/
# OA11_LSOA11_MSOA11_LAD11_EW_LUv2
# /FeatureServer/0/query?where=
# 1%3D1&outFields=
# LSOA11CD,LSOA11NM,MSOA11CD,MSOA11NM,LAD11CD,LAD11NM,LAD11NMW,ObjectId
# &outSR=4326&f=json


# David Kane's lookup
# https://github.com/drkane/geo-lookups/blob/master/lsoa_la.csv

# Scottish Data Zones lookup CSV
# https://statistics.gov.scot/data/data-zone-lookup
# http://statistics.gov.scot/downloads/file?id=2a2be2f0-bf5f-4e53-9726-7ef16fa893b7%2FDatazone2011lookup.csv


# scot_json_return <- httr::GET(url = "https://statistics.gov.scot/resource.json?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdata-zone-lookup") %>%
#   httr::content() %>%
#   pluck(1, "http://publishmydata.com/def/dataset#downloadURL") %>%
#   unlist(use.names = FALSE)
#
# readr::read_csv(scot_json_return)


# HoC Library friendly MSOA names ----------------------------------------
# https://visual.parliament.uk/msoanames
# hocl_msoanames_url = "https://visual.parliament.uk/msoanames/static/MSOA-Names-1.7.csv"

get_lookup_table <- function(
  place = NA,
  search_within = "lad19", return_areas = "wd19") {


  search_within <- search_within %>%
    paste0(., "nm") %>%
    toupper()

  return_areas <- return_areas %>%
    paste0(., "cd") %>%
    toupper()


  while (is.na(place)) {

    place <- as.character(
      readline("Name of Local Authority: ")
    )

  }

  place <- paste0("'", toupper(place), "'")

  # set up commonly used string variables for query URL construction;
  # just for neatness & easier updating. These are just taken from the
  # "API Explorer" tab on each Open Geography Portal page.
  arcgis_base <- paste0(
    "https://services1.arcgis.com/",
    "ESMARspQHYMw9BZ9/",
    "arcgis/rest/services/")

  query_line <- "/FeatureServer/0/query?where="

  json_coda <- "&outFields=*&outSR=4326&f=json"

  # create the query
  paste0(
    arcgis_base,
    "WD19_LAD19_UK_LU",
    query_line,
    search_within,
    " = ",
    place,
    json_coda
  ) %>%
    URLencode() %>%     # query must be URL-encoded
    jsonlite::fromJSON() %>% # read in the query response
    purrr::pluck("features", "attributes") # pull out the lookup table


}
