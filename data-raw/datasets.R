# House of Commons Library MSOA Names data -------------


hocl_msoa11_names <- paste0(
  "https://houseofcommonslibrary.github.io",
  "/msoanames/MSOA-Names-Latest.csv"
) |>
  readr::read_csv(col_types = "cccccc") |>
  dplyr::select(!"Laname")

hocl_msoa21_names <- paste0(
  "https://houseofcommonslibrary.github.io",
  "/msoanames/MSOA-Names-Latest2.csv"
) |>
  readr::read_csv(col_types = "cccccc") |>
  dplyr::select(!c("localauthorityname", "type"))


# Open Geography schema build -------------

build_schema <- function() {
  api_base_url <- paste0(
    "https://services1.arcgis.com/ESMARspQHYMw9BZ9/",
    "ArcGIS/rest/services"
  )

  api_services_data <- api_base_url |> 
    opengeo_api_req(append = "") |>
    query_opengeo_api() |>
    httr2::resp_body_json() |>
    purrr::pluck("services") |>
    purrr::map_df(unlist) |>
    dplyr::rename(service_url = url)

  api_data_return <- function(url) {
    url |>
      opengeo_api_req(append = "0") |>
      possibly_query_opengeo_api() |>
      httr2::resp_body_json()
  }

  pluck_api_data <- function(dat, url) {
    tibble::tibble(
      service_name = purrr::pluck(dat, "name", .default = ""),
      service_url = url,
      version = purrr::pluck(dat, "currentVersion"),
      edit_date = purrr::pluck(dat, "editingInfo", "lastEditDate"),
      data_edit_date = purrr::pluck(dat, "editingInfo", "dataLastEditDate"),
      max_record_count = purrr::pluck(dat, "maxRecordCount", .default = 500),
      has_geometry = purrr::pluck(dat, "hasGeometryProperties", .default = FALSE),
      field_names = purrr::pluck(dat, "fields", .default = NULL) |> purrr::map_chr("name")
    ) |>
      tidyr::pivot_wider(names_from = field_names, values_from = field_names) |>
      janitor::clean_names() |>
      dplyr::select(service_name:has_geometry, ends_with("cd"))
  }

  data_from_api <- api_services_data |>
    dplyr::pull(service_url) |>
    purrr::map(api_data_return, .progress = rlang::is_interactive())

  success_index <- data_from_api |>
    purrr::map_lgl(rlang::is_list)

  if (sum(success_index) < nrow(api_services_data)) {
    fails <- api_services_data |>
      dplyr::filter(!success_index)
    ui_info(str_glue("{nrow(fails)} service URLs did not successfully return data this time. Examples:")
    cat(head(fails)))
  }

  service_urls <- api_services_data$url[which(success_index)]

  api_data_df <- data_from_api |>
    purrr::compact() |>
    purrr::map2(service_urls, pluck_api_data,
      .progress = rlang::is_interactive()) |>
    purrr::list_rbind()

  opengeo_schema <- api_services_data |>
    dplyr::left_join(api_data_df) |>
    dplyr::filter(!if_all(ends_with("cd"), is.na))
}

opengeo_schema <- build_schema()

usethis::use_data(hocl_msoa11_names, hocl_msoa21_names, opengeo_schema,
  overwrite = TRUE,
  internal = TRUE,
  compress = "bzip2"
)
