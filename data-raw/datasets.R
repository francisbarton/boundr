# House of Commons Library MSOA Names data -------------


hocl_msoa11_names <- paste0(
  "https://houseofcommonslibrary.github.io",
  "/msoanames/MSOA-Names-Latest.csv"
) |>
  readr::read_csv(col_types = "cccccc") |>
  dplyr::select(!Laname)

hocl_msoa21_names <- paste0(
  "https://houseofcommonslibrary.github.io",
  "/msoanames/MSOA-Names-Latest2.csv"
) |>
  readr::read_csv(col_types = "cccccc") |>
  dplyr::select(!localauthorityname)


# Open Geography schema build -------------


pull_fields <- function(dtf, url) {
  tibble::tibble(
    service_name = purrr::pluck(dtf, "name", .default = ""),
    service_type = purrr::pluck(dtf, "type"),
    version = purrr::pluck(dtf, "currentVersion"),
    edit_date = purrr::pluck(dtf, "editingInfo", "lastEditDate"),
    has_geometry = purrr::pluck(dtf, "hasGeometryProperties", .default = FALSE),
    fields = purrr::pluck(dtf, "fields", .default = list(name = NULL))
  ) %>%
    tidyr::hoist(fields, "name") %>%
    tidyr::pivot_wider(names_from = name, values_from = name) %>%
    janitor::clean_names() %>%
    dplyr::mutate(service_url = url, .before = 1) %>%
    dplyr::select(service_url:has_geometry, ends_with("cd"))
}

# build_schema() -----------------------


build_schema <- function() {
  api_base_url <- paste0(
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/",
    "ArcGIS/rest/services"
  )
  
  urls <- api_base_url %>% 
    safely_query_opengeo_api(append = "") %>%
    purrr::pluck("result") %>%
    httr2::resp_body_json() %>%
    purrr::pluck("services") %>%
    purrr::map_chr("url")

  responses <- urls %>%
    purrr::map(safely_query_opengeo_api)

  fail_numbers <- responses %>%
    purrr::map("result") %>%
    purrr::map_lgl(is.null) %>%
    which()

  success_urls <- urls[-fail_numbers]

  responses %>%
    purrr::map("result") %>%
    purrr::compact() %>%
    purrr::map(httr2::resp_body_json) %>%
    purrr::map2_df(success_urls, pull_fields) %>%
    dplyr::filter(!if_all(ends_with("cd"), is.na))
}



opengeo_schema <- build_schema()

usethis::use_data(hocl_msoa11_names, hocl_msoa21_names, opengeo_schema,
  overwrite = TRUE,
  internal = TRUE,
  compress = "bzip2"
)
