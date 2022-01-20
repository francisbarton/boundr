lookup_table <- create_lookup_table("lsoa", "ccg", "NHS Hull CCG", option = 2)

"bounds 1" %>%
  testthat::test_that({
    geo_code_field <- lookup_table %>%
      dplyr::select(starts_with("lsoa") & ends_with("cd")) %>%
      names()
    testthat::expect_identical(
      geo_code_field,
      "lsoa11cd"
    )

    bounds_query_data <- pull_geo_query_url(geo_code_field, resolution = "BSC", geo_option = 1)
    query_base_url <- bounds_query_data[["service_url"]]
    testthat::expect_identical(
      query_base_url,
      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/Lower_Layer_Super_Output_Areas_DEC_2011_EW_BSC_V3/FeatureServer"
    )

    geo_code_field <- bounds_query_data[["x_code"]] %>%
      tolower()
    testthat::expect_identical(
      geo_code_field,
      "lsoa11cd"
    )

    testthat::expect_true(
      geo_code_field %in% names(lookup_table)
    )

    area_codes <- lookup_table %>%
      dplyr::pull(geo_code_field) %>% # 166 codes
      batch_it(50)
    # ceiling(nrow(lookup_table)/50)
    testthat::expect_length(area_codes, 4)
    testthat::expect_true(stringr::str_starts(area_codes[[1]][1], "E01012"))
  })
