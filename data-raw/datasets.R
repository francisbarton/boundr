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
      has_geometry = purrr::pluck(
        dat,
        "hasGeometryProperties",
        .default = FALSE
      ),
      field_names = purrr::pluck(dat, "fields", .default = NULL) |>
        purrr::map_chr("name")
    ) |>
      tidyr::pivot_wider(
        names_from = "field_names",
        values_from = "field_names"
      ) |>
      janitor::clean_names() |>
      dplyr::select("service_name":"has_geometry", tidyselect::ends_with("cd"))
  }

  # Data pipeline
  api_services_data <- ogu() |>
    opengeo_api_req(append = "") |>
    query_opengeo_api() |>
    httr2::resp_body_json() |>
    purrr::pluck("services") |>
    purrr::map_df(unlist) |>
    dplyr::rename(service_url = url) |>
    dplyr::filter(dplyr::if_any("type", \(x) x == "FeatureServer")) |>
    dplyr::select(!"type")

  data_from_api <- api_services_data |>
    dplyr::pull("service_url") |>
    purrr::map(api_data_return, .progress = "Retrieving services data")

  # Deal with any URLs that don't return data
  fails <- which(purrr::map_lgl(data_from_api, is.null))

  if (length(fails) > 0) {
    info <- api_services_data[["name"]][fails] |> # nolint
      cli::cli_vec(c(`vec-trunc` = 3))
    cli_alert_info(c(
      "{length(fails)} services did not successfully return data this time. ",
      "Examples: {info}"
    ))
  }

  service_urls <- api_services_data[["service_url"]][-fails]

  # Final data format process
  schema <- data_from_api |>
    purrr::compact() |>
    purrr::map(httr2::resp_body_json) |>
    purrr::map2(
      service_urls,
      pluck_api_data,
      .progress = "Retrieving schema data"
    ) |>
    purrr::list_rbind() |>
    dplyr::filter(!dplyr::if_all(tidyselect::ends_with("cd"), is.na)) |>
    janitor::remove_empty("cols") |>
    dplyr::select("service_name":"has_geometry", tidyselect::ends_with("cd")) |>
    dplyr::mutate(
      dplyr::across("service_name", \(x) gsub("\\sv", "_v", x)), # do this first
      dplyr::across("service_name", \(x) gsub("__", "_", x)),
      dplyr::across("service_name", \(x) {
        stringr::str_replace_all(
          x,
          c(
            "^Administrative_Counties" = "CTY",
            "^Administrative_Districts" = "DIST",
            "^Built_Up_Area" = "BUA",
            "^Cancer_Alliances" = "CAL",
            "^Census_Districts" = "CD",
            "^Census_Merged_Wards" = "CMW",
            "^Clinical_Commissioning_Groups" = "CCG",
            "^Combined_Authorities" = "CAUTH",
            "^Community_Safety_Partnerships" = "CSP",
            "^Counties_and_Unitary_Authorities" = "CTYUA",
            "^Counties" = "CTY",
            "^Countries" = "CTRY",
            "^Country" = "CTRY",
            "Covid_Infection_Survey" = "CIS",
            "^CUA" = "CTYUA",
            "^Enumeration_Districts" = "ED",
            "^European_Electoral_Regions" = "EER",
            "^Fire_and_Rescue_Authorities" = "FRA",
            "^Health_and_Social_Care_Board" = "HSCB",
            "^Index_of_Multiple_Deprivation" = "IMD",
            "^London_Assembly_Constituencies" = "LAC",
            "^Local_Authority_Districts" = "LAD",
            "^Local_Enterprise_Partnerships" = "LEP",
            "^Local_Health_Boards" = "LHB",
            "^Local_Planning_Authorities" = "LPA",
            "^Local_Resilience_Forums" = "LRF",
            "^Major_Towns_and_Cities" = "TCITY",
            "^Metropolitan_Counties" = "MCTY",
            "^National_Assembly_Economic_Regions" = "NAER",
            "^National_Parks" = "NPARK",
            "^NHS_Commissioning_Regions" = "NHSCR",
            "^NHS_Region_Local_Offices" = "NHSRLO",
            "^NHS_England_Region_Local_Office" = "NHSRLO",
            "^NHS_Regions_Geography" = "NHSRG",
            "^NHS_England_Regions" = "NHSER",
            "^NUTS_Level_" = "NUTS",
            "^Output_Area" = "OA",
            "^OA_(2011)" = "OA1",
            "^OAs_(2021)" = "OA21",
            "^Parishes_and_Non_Civil_Parished_Areas" = "PARNCP",
            "^Parishes" = "PAR",
            "^Postcode" = "PCD",
            "^Police_Force_Areas" = "PFA",
            "^Public_Health_England_Centres" = "PHEC",
            "^Public_Health_England_Regions" = "PHEREG",
            "^Regions" = "RGN",
            "^Registration_Districts" = "REGD",
            "^Strategic_Clinical_Networks" = "SCN",
            "^Sustainability_and_Transformation_Partnerships" = "STP",
            "^Travel_to_Work_Areas" = "TTWA",
            "^Urban_Audit" = "UA",
            "^Wards" = "WD",
            "^Ward_names_and_codes" = "WD",
            "_Ward_" = "_WD_",
            "^Westminster_Parliamentary_Constituencies" = "PCON",
            "^.*Westminster_Parlimentary_constituencies" = "PCON",
            "^Workplace_Zones" = "WZ",
            "Local_Authority_District" = "LAD",
            "Lower_Tier_Local_Authority" = "LTLA",
            "Upper_Tier_Local_Authority" = "UTLA",
            "Upper_tier_LAs" = "UTLA",
            "Senedd_constituencies" = "SENC",
            "Region" = "RGN",
            "Lookup" = "LU",
            "_Wales$" = "_WA",
            "England" = "EN",
            "Great_Britain" = "GB",
            "Scotland" = "SC",
            "Northern Ireland" = "NI",
            "Sub_ICB_Locations" = "SICBL",
            "^PopCentroids_EW_2021_V3$" = "OA21_PWC",
            "^index_of_multiple_deprivation_d$" = "IMD_December_2016_LU_in_SC",
            "^IMD_DEC_17$" = "IMD_December_2017_LU_in_NI",
            "^IMD_December_2019_LU_in_EN$" = "IMD_December_2019_LU_in_EN",
            "^IMD_D$" = "IMD_December_2019_LU_in_WA",
            "Ultra_Generalised_Clipped_Boundaries" = "UGCB",
            "Ultra_Generalised_Boundaries" = "UGCB",
            "Ultra_Generalised" = "UGCB",
            "Generalised_Clipped_Boundaries" = "GCB",
            "General_Clipped_Boundaries" = "GCB",
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
          )
        )
      })
    ) |>
    # Weird thing where a dataset has no presence in the schema - fix here:
    dplyr::mutate(
      pcds21cd = dplyr::if_else(
        .data[["service_name"]] == "OA21_PCDS21_EW_LU",
        "PCDS21CD",
        NA
      )
    ) |>
    dplyr::arrange(dplyr::desc(dplyr::pick("data_edit_date")))
}

opengeo_schema <- build_schema()

usethis::use_data(
  hocl_msoa11_names,
  hocl_msoa21_names,
  opengeo_schema,
  internal = TRUE,
  overwrite = TRUE
)
