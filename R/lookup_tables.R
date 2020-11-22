lookup_lookup_table <- dplyr::tribble(
  ~ table_code, ~ lower_level, ~ upper_level, ~ ref,

  # "https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-area-2011-to-ward-2019-lookup-in-england-and-wales"
  "LSOA11_WD19_LAD19_EW_LU",
  "lsoa",
  "ward",
  1,


  "LSOA11_WD19_LAD19_EW_LU",
  "lsoa",
  "lad",
  2,

  # "https://geoportal.statistics.gov.uk/datasets/ward-to-local-authority-district-december-2019-lookup-in-the-united-kingdom"
  "WD19_LAD19_UK_LU",
  "ward",
  "lad",
  3,

  # "https://geoportal.statistics.gov.uk/datasets/ward-to-local-authority-district-to-county-to-region-to-country-december-2019-lookup-in-united-kingdom"
  # "WD19_LAD19_CTY19_OTH_UK_LU",
  # ward,
  # lad,
  # 3,


  # "WD19_LAD19_CTY19_OTH_UK_LU",
  # ward,
  # region,
  # 4,


  "WD19_LAD19_CTY19_OTH_UK_LU",
  "lad",
  "region",
  4,

  "WD19_LAD19_CTY19_OTH_UK_LU",
  "lad",
  "country",
  5,


  # "https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-area-2011-to-upper-tier-local-authorities-2019-lookup-in-england-and-wales-"
  "LSOA11_UTLA19_EW_LU",
  "lsoa",
  "utla",
  6,


  # A lookup file of the lower tier local authorities
  # (local authority districts, unitary authorities, metropolitan districts,
  # London boroughs) to the upper tier local authorities (counties,
  # metropolitan counties, inner and outer London, unitary authorities)
  # in England and Wales
  # "https://geoportal.statistics.gov.uk/datasets/lower-tier-local-authority-to-upper-tier-local-authority-april-2019-lookup-in-england-and-wales"
  "LTLA19_UTLA19_EW_LU",
  "ltla",
  "utla",
  7,


  # "https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-combined-authority-december-2019-lookup-in-england",
  "LAD19_CAUTH19_EN_LU",
  "lad",
  "cauth",
  8


)



area_code_lookup <- dplyr::tribble(
  ~ friendly, ~ serious,
  "lsoa",   "lsoa11",
  "msoa",   "msoa11",
  "wd",     "wd19",
  "ward",   "wd19",
  "lad",    "lad19",
  "ltla",   "ltla19",
  "lower",  "ltla19",
  "utla",   "utla19",
  "upper",  "utla19",
  "unitary","utla19",
  "county", "utla19",
  "cauth",  "cauth19",
  "region", "rgn19",
  "ctry",   "ctry19"
)
