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

  # Helper functions
  api_data_return <- function(url) {
    url |>
      opengeo_api_req(append = "0") |>
      possibly_query_opengeo_api()
  }

  pluck_api_data <- function(dat, url) {
    tibble::tibble(
      service_name = purrr::pluck(dat, "name", .default = ""),
      service_url = url,
      # version = purrr::pluck(dat, "currentVersion"),
      edit_date = purrr::pluck(dat, "editingInfo", "lastEditDate"),
      data_edit_date = purrr::pluck(dat, "editingInfo", "dataLastEditDate"),
      max_record_count = purrr::pluck(dat, "maxRecordCount", .default = 500),
      has_geometry = purrr::pluck(dat, "hasGeometryProperties",
                                  .default = FALSE),
      field_names = purrr::pluck(dat, "fields", .default = NULL) |>
        purrr::map_chr("name")
    ) |>
      tidyr::pivot_wider(
        names_from = field_names,
        values_from = field_names) |>
      janitor::clean_names() |>
      dplyr::select(service_name:has_geometry, ends_with("cd"))
  }

  # Data pipeline
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
    dplyr::rename(service_url = url) |>
    dplyr::filter(type == "FeatureServer") |>
    dplyr::select(!type)

  data_from_api <- api_services_data |>
    dplyr::pull(service_url) |>
    purrr::map(api_data_return, .progress = rlang::is_interactive())

  # Deal with any URLs that don't return data
  fails <- which(purrr::map_lgl(data_from_api, is.null))

  if (length(fails)) {
    info <- stringr::str_c("* ", head(api_services_data$name[fails]),
      collapse = ",\n")
    ui_info(
      stringr::str_glue(
        "{length(fails)} services ",
        "did not successfully return data this time. Examples:\n",
        "{info}"))
  }

  service_urls <- api_services_data$service_url[-fails]

  # Final data format process
  data_from_api |>
    purrr::compact() |>
    purrr::map(httr2::resp_body_json) |>
    purrr::map2(service_urls, pluck_api_data,
      .progress = rlang::is_interactive()) |>
    purrr::list_rbind() |>
    dplyr::filter(!if_all(ends_with("cd"), is.na)) |>
    janitor::remove_empty("cols") |>
    dplyr::select(
      "service_name":"has_geometry", ends_with("cd")
    ) |>
    dplyr::arrange(desc(data_edit_date))
}

opengeo_schema <- build_schema()

usethis::use_data(hocl_msoa11_names, hocl_msoa21_names, opengeo_schema,
  overwrite = TRUE,
  internal = TRUE,
  compress = "bzip2"
)
