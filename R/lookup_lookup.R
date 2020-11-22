# This is just a worse way of making a lookup table than the way I used
# for heatmap ... probably aboandon this.

make_lookup_table <- function() {

  lookup_lookup_table <- dplyr::tribble(
    ~ title, ~ table_code, ~ reference_url, ~ lower_keys, ~ upper_keys, ~ ref,

    "Output Area to Lower Layer Super Output Area to Middle Layer Super Output Area to Local Authority District (December 2011) Lookup in England and Wales",
    "OA11_LSOA11_MSOA11_LAD11_EW_LUv2",
    "https://geoportal.statistics.gov.uk/datasets/output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2011-lookup-in-england-and-wales/geoservice",
    1,
    2,
    1,

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
    c(1,3),
    3:4,
    2,



    "Ward to Local Authority District to County to Region to Country (December 2019) Lookup in United Kingdom",
    "WD19_LAD19_CTY19_OTH_UK_LU",
    "https://geoportal.statistics.gov.uk/datasets/ward-to-local-authority-district-to-county-to-region-to-country-december-2019-lookup-in-united-kingdom",
    c(4,8),
    8:9,
    3,



    # provides LSOA11 -> UTLA19 lookup
    # "Lower Layer Super Output Area (2011) to Upper Tier Local Authorities (2019) Lookup in England and Wales",
    # "LSOA11_UTLA19_EW_LU",
    # # rogue hyphen at end of URL :-/
    # "https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-area-2011-to-upper-tier-local-authorities-2019-lookup-in-england-and-wales-",


    # "Ward to Local Authority District (December 2019) Lookup in the United Kingdom",
    # "WD19_LAD19_UK_LU",
    # "https://geoportal.statistics.gov.uk/datasets/ward-to-local-authority-district-december-2019-lookup-in-the-united-kingdom",
    # 3,
    # 4,

    # A lookup file of the lower tier local authorities
    # (local authority districts, unitary authorities, metropolitan districts,
    # London boroughs) to the upper tier local authorities (counties,
    # metropolitan counties, inner and outer London, unitary authorities)
    # in England and Wales
    "Lower Tier Local Authority to Upper Tier Local Authority (April 2019) Lookup in England and Wales",
    "LTLA19_UTLA19_EW_LU",
    "https://geoportal.statistics.gov.uk/datasets/lower-tier-local-authority-to-upper-tier-local-authority-april-2019-lookup-in-england-and-wales",
    5,
    6,
    4,

    # includes April 2020 changes: Buckinghamshire two-tier to unitary
    # "Local Authority District to County (April 2020) Lookup in England",
    # "LAD20_CTY20_EN_LU",
    # "https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-county-april-2020-lookup-in-england",



    "Local Authority District to Combined Authority (December 2019) Lookup in England",
    "LAD19_CAUTH19_EN_LU",
    "https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-combined-authority-december-2019-lookup-in-england",
    4,
    7,
    5


    # "Local Authority District to Region (April 2019) Lookup in England",
    # "LAD19_RGN19_EN_LU",
    # "https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-region-april-2019-lookup-in-england",
    # 4,
    # 7

  )


  levels_ref <- dplyr::tribble(
    ~ level,   ~ code,      ~ key,

    "lsoa",    "lsoa11",    1,
    "msoa",    "msoa11",    2,
    "ward",    "wd19",      3,
    "lad",     "lad19",     4,
    "ltla",    "ltla19",    5,
    "utla",    "utla19",    6,
    "cauth",   "cauth19",   7,
    "region",  "rgn19",     8,
    "country", "ctry19",    9
  )


  construction_order <- dplyr::tribble(
    ~ lower, ~ upper, ~ ref,
    1, 2, 1,
    1, 3, 2,
    3, 4, 2,
    5, 6, 4,
    4, 7, 5,
    4, 8, 3,
    8, 9, 3
  )

  rename_ltla <- function(df) {
    if ("LTLA19CD" %in% colnames(df)) {
      df %>%
        dplyr::rename(
          LAD19CD = LTLA19CD,
          LAD19NM = LTLA19NM
        )
    }
  }


  pull_data <- function(x, y, z) {

    table_code <- lookup_lookup_table %>%
      dplyr::filter(ref == z) %>%
      dplyr::pull(table_code)

    cols_to_pull <- levels_ref %>%
      dplyr::slice(x, y) %>%
      dplyr::pull(code) %>%
      rep(each = 2) %>%
      paste0(., c("cd", "nm")) %>%
      toupper() %>%
      stringr::str_c(collapse = ",")




  }

  purrr::pmap(construction_order, ~ pull_data(..1, ..2, ..3)) %>%
    purrr::map(rename_ltla) %>%
    purrr::reduce(dplyr::left_join)





  # search_within <- search_within %>%
  #   paste0(., "nm") %>%
  #   toupper()
  #
  # return_areas <- return_areas %>%
  #   paste0(., "cd") %>%
  #   toupper()


  # while (is.na(place)) {
  #
  #   place <- as.character(
  #     readline("Name of Local Authority: ")
  #   )
  #
  # }
  #
  # place <- paste0("'", toupper(place), "'")




}


# Scottish Data Zones lookup CSV
# https://statistics.gov.scot/data/data-zone-lookup
# http://statistics.gov.scot/downloads/file?id=2a2be2f0-bf5f-4e53-9726-7ef16fa893b7%2FDatazone2011lookup.csv


# scot_json_return <- httr::GET(url = "https://statistics.gov.scot/resource.json?uri=http%3A%2F%2Fstatistics.gov.scot%2Fdata%2Fdata-zone-lookup") %>%
#   httr::content() %>%
#   pluck(1, "http://publishmydata.com/def/dataset#downloadURL") %>%
#   unlist(use.names = FALSE)
#
# readr::read_csv(scot_json_return)



