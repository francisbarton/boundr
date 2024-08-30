# House of Commons Library MSOA Names data -------------

hocl_msoa11_names <- paste0(
  "https://houseofcommonslibrary.github.io/msoanames/",
  "MSOA-Names-Latest.csv"
) |>
  readr::read_csv(col_types = "cccccc") |>
  dplyr::select(!"Laname")

hocl_msoa21_names <- paste0(
  "https://houseofcommonslibrary.github.io/msoanames/",
  "MSOA-Names-Latest2.csv"
) |>
  readr::read_csv(col_types = "cccccc") |>
  dplyr::select(!c("localauthorityname", "type"))


# Open Geography schema build -------------

devtools::load_all()

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
        names_from = "field_names",
        values_from = "field_names"
      ) |>
      janitor::clean_names() |>
      dplyr::select("service_name":"has_geometry", ends_with("cd"))
  }

  # Data pipeline
  api_services_data <- og_() |>
    opengeo_api_req(append = "") |>
    query_opengeo_api() |>
    httr2::resp_body_json() |>
    purrr::pluck("services") |>
    purrr::map_df(unlist) |>
    dplyr::rename(service_url = url) |>
    dplyr::filter(if_any("type", \(x) x == "FeatureServer")) |>
    dplyr::select(!"type")

  data_from_api <- api_services_data |>
    dplyr::pull("service_url") |>
    purrr::map(api_data_return, .progress = "Retrieving services data")

  # Deal with any URLs that don't return data
  fails <- which(purrr::map_lgl(data_from_api, is.null))

  if (length(fails)) {
    info <- api_services_data[["name"]][fails] |> # nolint
      cli::cli_vec(c(`vec-trunc` = 3))
    cli_alert_info(c(
      "{length(fails)} services did not successfully return data this time. ",
      "Examples: {info}"
    ))
  }

  service_urls <- api_services_data[["service_url"]][-fails]

  # Final data format process
  data_from_api |>
    purrr::compact() |>
    purrr::map(httr2::resp_body_json) |>
    purrr::map2(
      service_urls, pluck_api_data,
      .progress = "Retrieving schema data"
    ) |>
    purrr::list_rbind() |>
    dplyr::filter(!if_all(ends_with("cd"), is.na)) |>
    janitor::remove_empty("cols") |>
    dplyr::select("service_name":"has_geometry", ends_with("cd")) |>
    dplyr::mutate(
      across("service_name", \(x) gsub("\\sv", "_v", x)), # do this first
      across("service_name", \(x) gsub("__", "_", x)),
      across("service_name", \(x) {
        stringr::str_replace_all(x, c(
          "^Built_Up_Area" = "BUA",
          "^Cancer_Alliances" = "CAL",
          "^Clinical_Commissioning_Groups" = "CCG",
          "^Combined_Authorities" = "CAUTH",
          "^Community_Safety_Partnerships" = "CSP",
          "^Counties" = "CTY",
          "^Counties_and_Unitary_Authorities" = "CTYUA",
          "^Countries" = "CTRY",
          "^Country" = "CTRY",
          "^CUA" = "CTYUA",
          "^European_Electoral_Regions" = "EER",
          "^Fire_and_Rescue_Authorities" = "FRA",
          "^Index_of_Multiple_Deprivation" = "IMD",
          "^London_Assembly_Constituencies" = "LAC",
          "^Local_Authority_Districts" = "LAD",
          "^Local_Enterprise_Partnerships" = "LEP",
          "^Local_Health_Boards" = "LHB",
          "^Local_Planning_Authorities" = "LPA",
          "^Local_Resilience_Forums" = "LRF",
          "^Metropolitan_Counties" = "MCTY",
          "^National_Assembly_Economic_Regions" = "NAER",
          "^NHS_Commissioning_RGNs" = "NHSCR",
          "^NHS_England_Regions" = "NHSER",
          "^National_Parks" = "NPARK",
          "^Output_Area" = "OA",
          "^Parishes" = "PAR",
          "^Parishes_and_Non_Civil_Parished_Areas" = "PARNCP",
          "^Police_Force_Areas" = "PFA",
          "^Public_Health_England_Centres" = "PHEC",
          "^Public_Health_England_RGNs" = "PHEREG",
          "^Regions" = "RGN",
          "^Registration_Districts" = "REGD",
          "^Strategic_Clinical_Networks" = "SCN",
          "^Sustainability_and_Transformation_Partnerships" = "STP",
          "^Travel_to_Work_Areas" = "TTWA",
          "^Wards" = "WD",
          "_Ward_" = "_WD_",
          "^Westminster_Parliamentary_Constituencies" = "PCON",
          "^Workplace_Zones" = "WZ",
          "Local_Authority_District" = "LAD",
          "Lower_Tier_Local_Authority" = "LTLA",
          "Upper_Tier_Local_Authority" = "UTLA",
          "Region" = "RGN",
          "Lookup" = "LU",
          "_Wales$" = "_WA",
          "England" = "EN",
          "Great_Britain" = "GB",
          "Scotland" = "SC",
          "Sub_ICB_Locations" = "SICBL",
          "Ultra_Generalised_Clipped_Boundaries" = "UGCB",
          "Ultra_Generalised_Boundaries" = "UGCB",
          "Generalised_Clipped_Boundaries" = "GCB",
          "Generalised_Clipped" = "GCB",
          "Generalised_Boundaries" = "GCB",
          "Full_Clipped_Boundaries" = "FCB",
          "Full_Extent_Boundaries" = "FEB",
          "Full_Clipped" = "FCB",
          "Full_Extent" = "FEB",
          "Full_extent" = "FEB",
          "_UGB" = "UGCB",
          "_FECB" = "FEB",
          "_FRCB" = "FCB"
        ))
      })
    ) |>
    dplyr::arrange(desc(pick("data_edit_date")))
}

opengeo_schema <- build_schema()

usethis::use_data(
  hocl_msoa11_names,
  hocl_msoa21_names,
  opengeo_schema,
  internal = TRUE,
  overwrite = TRUE
)
