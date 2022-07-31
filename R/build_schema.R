query_opengeo_api <- function(url, append = "0") {
  url %>%
    httr2::request() %>%
    httr2::req_headers(UserAgent = "boundr R package") %>%
    httr2::req_url_path_append(append) %>%
    httr2::req_url_query(f = "pjson") %>%
    httr2::req_retry(max_tries = 3)
}

# safely_query_opengeo_api <- purrr::safely(query_opengeo_api)
safely_query_opengeo_api <- function(...) "dummy"



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


build_schema <- function() {
  server_url <- paste0(
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/",
    "ArcGIS/rest/services")

  urls <- server_url %>%
    query_opengeo_api(append = "") %>%
    httr2::resp_body_json() %>%
    purrr::pluck("services") %>%
    purrr::map_chr("url") # length(urls): 4839

  responses <- urls %>%
    purrr::map(safely_query_opengeo_api)

  # error_numbers <- responses %>%
  #   purrr::map("error") %>%
  #   purrr::map_lgl(~ !is.null(.)) %>%
  #   which()

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
