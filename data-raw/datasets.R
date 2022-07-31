# House of Commons Library MSOA Names data -------------


hocl_msoa_names <- paste0(
  "https://houseofcommonslibrary.github.io/",
  "msoanames/MSOA-Names-Latest.csv") |>
  readr::read_csv(col_types = "cccccc") |>
  dplyr::select(!Laname)


# Open Geography schema build ------------- 

pull_fields <- function(json, url) {
  json %>% {
    dplyr::tibble(
      service_name = purrr::pluck(., "name"),
      service_type = purrr::pluck(., "type"),
      version = purrr::pluck(., "currentVersion"),
      edit_date = purrr::pluck(., "editingInfo", "lastEditDate"),
      has_geometry = purrr::pluck(., "hasGeometryProperties", .default = FALSE),
      fields = purrr::pluck(., "fields", .default = list(name = NULL))
    )
  } %>%
    tidyr::hoist(fields, "name") %>%
    dplyr::select(1:name) %>%
    tidyr::pivot_wider(names_from = name, values_from = name) %>%
    janitor::clean_names() %>%
    dplyr::mutate(service_url = url, .before = 1) %>%
    dplyr::select(service_url:has_geometry, ends_with("cd")) %>%
    dplyr::filter(!if_all(ends_with("cd"), is.na))
}

# build_schema() ------------- 

build_schema <- function() {
  server_url <- paste0(
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/",
    "ArcGIS/rest/services")

  urls <- server_url %>%
    safely_query_opengeo_api(append = "") %>%
    purrr::pluck("result") %>% 
    httr2::resp_body_json() %>%
    purrr::pluck("services") %>%
    purrr::map_chr("url")

  responses <- urls %>%
    purrr::map(safely_query_opengeo_api)

  responses %>%
    purrr::map("error") %>%
    purrr::compact() %>%
    purrr::map("resp") %>%
    purrr::walk(httr2::resp_check_status)

  success_numbers <- responses %>%
    purrr::map("result") %>%
    purrr::map_lgl(~ !is.null(.)) %>%
    which()

  success_urls <- urls[success_numbers]

  responses %>%
    purrr::map("result") %>%
    purrr::compact() %>%
    purrr::map(httr2::resp_body_json) %>%
    purrr::map2_df(success_urls, pull_fields)
}



opengeo_schema <- build_schema()

usethis::use_data(hocl_msoa_names, opengeo_schema, overwrite = TRUE, internal = TRUE, compress = "bzip2")
