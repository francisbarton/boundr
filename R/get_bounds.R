library(dplyr)
library(jsonlite)
library(purrr)
library(sf)
library(stringr)
library(tmap)


get_ward_bounds <- function(x = NA) {

  search_within <- "lad19nm" %>%
    toupper()

  area_return <- "wd19" %>%
    paste0(., "cd") %>%
    toupper()


  if (is.na(x)) {

  x <- as.character(
      readline("Name of Local Authority: ")
    )

  }

  x <- paste0("'", toupper(x), "'")


  arcgis_base <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/"

  query_line <- "/FeatureServer/0/query?where="

  json_coda <- "&outFields=*&outSR=4326&f=json"

  ward_codes <- paste0(
    arcgis_base,
    "WD19_LAD19_UK_LU",
    query_line,
    search_within,
    " = ",
    x,
    json_coda
  ) %>%
    URLencode() %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("features", "attributes") %>%
    dplyr::pull(area_return)

  ward_string <- ward_codes %>%
    paste0("'", ., "'") %>%
    paste0(area_return, " = ", .) %>%
    stringr::str_c(collapse = " OR ") %>%
    paste0("(", ., ")")

  paste0(
    arcgis_base,
    "Wards_December_2019_Boundaries_UK_BFC_v2",
    query_line,
    ward_string,
    json_coda
  ) %>%
    URLencode() %>%
    sf::st_read()

}

# test_out <- get_ward_bounds("Ealing")
# tmap::tm_shape(test_out) + tm_borders()
