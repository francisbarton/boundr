schema_init <- function() {
  server_url <- paste0(
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/",
    "ArcGIS/rest/services")

  server_url %>%
    httr2::request() %>%
    httr2::req_url_query(f = "pjson") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    purrr::pluck("services") %>% {
      dplyr::tibble(
        name = purrr::map_chr(., "name"),
        type = purrr::map_chr(., "type"),
        url = purrr::map_chr(., "url")
      )
    }
}

pull_fields <- function(url) {
  url %>%
    httr2::request() %>%
    httr2::req_url_path_append("/0") %>%
    httr2::req_url_query(f = "pjson") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    purrr::pluck("fields") %>%
    purrr::map_chr("name") %>%
    purrr::set_names()
}

build_schema <- function() {
  schema_init <- schema_init()

  schema_init %>%
    dplyr::pull(url) %>%
    purrr::map_df(pull_fields) %>%
    dplyr::select(sort(names(.))) %>%
    dplyr::bind_cols(schema_init, .)
}
