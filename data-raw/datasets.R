
# List of all LSOAs in England and Wales: codes and names -----------------

library(tidyverse)

lsoa11cdnm <- paste0(
  "https://opendata.arcgis.com/",
  "datasets/",
  "3ce71e53d9254a73b3e887a506b82f63_0.csv"
) %>%
  readr::read_csv() %>%
  janitor::clean_names() %>%
  dplyr::select(1:2)




extract_properties <- function(x) {
  x %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("features", "properties") %>%
    janitor::clean_names() %>%
    dplyr::select(-fid)
}





upper_tier_region_ctry_lookup <- extract_properties(
  # LTLA:UTLA lookup 2021
  # https://geoportal.statistics.gov.uk/datasets/lower-tier-local-authority-to-upper-tier-local-authority-april-2021-lookup-in-england-and-wales/
  "https://opendata.arcgis.com/datasets/967a3660c4aa49819731ceefe4008d76_0.geojson") %>%
  dplyr::full_join(
    extract_properties(
      # LAD:RGN lookup 2021
      # https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-region-april-2021-lookup-in-england/
      "https://opendata.arcgis.com/datasets/6a41affae7e345a7b2b86602408ea8a2_0.geojson"),
    by = c("ltla21cd" = "lad21cd", "ltla21nm" = "lad21nm")
  ) %>%
  dplyr::left_join(
    extract_properties(
      # LAD:CTRY lookup 2021
      # https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-country-april-2021-lookup-in-the-united-kingdom/
      "https://opendata.arcgis.com/datasets/7df3fe50816e4cb6b3147a9d91572106_0.geojson"
    ),
    by = c("ltla21cd" = "lad21cd", "ltla21nm" = "lad21nm")
  ) %>%
  # dplyr::select(!c(ltla21cd, ltla21nm)) %>%
  dplyr::distinct()






lad21nmw_lookup <- jsonlite::fromJSON(
  # https://geoportal.statistics.gov.uk/datasets/local-authority-districts-april-2021-names-and-codes-in-the-united-kingdom/
  "https://opendata.arcgis.com/datasets/c02975a3618b46db958369ff7204d1bf_0.geojson") %>%
  purrr::pluck("features", "properties") %>%
  janitor::clean_names() %>%
  dplyr::select(-fid)




nnhts <- c("Corby", "East Northamptonshire", "Kettering", "Wellingborough")
wnhts <- c("Daventry", "Northampton", "South Northamptonshire")


oa_lad21_lookup <- jsonlite::fromJSON(
  # https://geoportal.statistics.gov.uk/datasets/ons::output-area-to-lower-layer-super-output-area-to-middle-layer-super-output-area-to-local-authority-district-december-2020-lookup-in-england-and-wales/
  "https://opendata.arcgis.com/datasets/65664b00231444edb3f6f83c9d40591f_0.geojson"
) %>%
  purrr::pluck("features", "properties") %>%
  janitor::clean_names() %>%
  dplyr::select(!c(fid, rgn20cd, rgn20nm)) %>%
  dplyr::mutate(ltla21nm = dplyr::case_when(
    lad20nm %in% nnhts ~ "North Northamptonshire",
    lad20nm %in% wnhts ~ "West Northamptonshire",
    TRUE ~ lad20nm
  )) %>%
  dplyr::left_join(upper_tier_region_ctry_lookup) %>%
  dplyr::relocate(ltla21cd, .before = ltla21nm)





hocl_msoa_names <- paste0(
  "https://houseofcommonslibrary.github.io/",
  "msoanames/MSOA-Names-Latest.csv") %>%
  readr::read_csv() %>%
  dplyr::select(-Laname)


usethis::use_data(lsoa11cdnm, upper_tier_region_ctry_lookup, lad21nmw_lookup, hocl_msoa_names, oa_lad21_lookup, overwrite = TRUE, internal = TRUE, compress = "bzip2")
