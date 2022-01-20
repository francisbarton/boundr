og_api_query <- function(url, append = "/0") {
  url %>%
    httr2::request() %>%
    httr2::req_url_path_append(append) %>%
    httr2::req_url_query(f = "pjson") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()
}


pull_fields <- function(url) {
  url %>%
    og_api_query() %>% {
      dplyr::tibble(
        service_name = purrr::pluck(., "name"),
        service_type = purrr::pluck(., "type"),
        version = purrr::pluck(., "currentVersion"),
        edit_date = purrr::pluck(., "editingInfo", "lastEditDate"),
        has_geometry = ifelse(!is.null(purrr::pluck(., "hasGeometryProperties")), purrr::pluck(., "hasGeometryProperties"), FALSE),
        fields = purrr::pluck(., "fields")
      )
    } %>%
    tidyr::unnest_wider(fields) %>%
    dplyr::select(service_name:has_geometry, name) %>%
    tidyr::pivot_wider(names_from = name, values_from = name) %>%
    janitor::clean_names() %>%
    dplyr::select(service_name:has_geometry, ends_with("cd"))
}


build_schema <- function() {
  server_url <- paste0(
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/",
    "ArcGIS/rest/services")

  urls <- server_url %>%
    og_api_query(append = "") %>%
    purrr::pluck("services") %>%
    purrr::map_chr("url")

  urls %>%
    purrr::map_df(pull_fields) %>%
    dplyr::bind_cols(service_url = urls, .) %>%
    dplyr::filter(!if_all(ends_with("cd"), is.na))
}
