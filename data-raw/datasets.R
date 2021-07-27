
# List of all LSOAs in England and Wales: codes and names -----------------



lsoa11cdnm <- paste0(
  "https://opendata.arcgis.com/",
  "datasets/",
  "3ce71e53d9254a73b3e887a506b82f63_0.csv"
) %>%
  readr::read_csv() %>%
  janitor::clean_names() %>%
  dplyr::select(1:2)


usethis::use_data(lsoa11cdnm, overwrite = TRUE, internal = FALSE)



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
  dplyr::select(!c(ltla21cd, ltla21nm)) %>%
  dplyr::distinct() %>%
  dplyr::arrange(utla21cd)



usethis::use_data(upper_tier_region_ctry_lookup, overwrite = TRUE, internal = FALSE)
