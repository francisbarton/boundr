"bounds 1" |>
  test_that({
    lookup_table <- create_lookup_table("lsoa", "ccg", lookup_year = "2011", "NHS Hull CCG", option = 1)

    geo_code_field <- lookup_table |>
      dplyr::select(starts_with("lsoa") & ends_with("cd")) |>
      names()

    expect_identical(
      geo_code_field,
      "lsoa11cd"
    )

    query_base_url <- pull_geo_query_url(geo_code_field, resolution = "BSC")
    expect_identical(
      query_base_url,
      paste0("https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/",
             "services/LSOA_Dec_2011_Boundaries_",
             "Super_Generalised_Clipped_BSC_EW_V3_2022/FeatureServer"
    ))

    expect_true(
      geo_code_field %in% names(lookup_table)
    )

    area_codes <- lookup_table |>
      dplyr::pull(geo_code_field) |> # 166 codes
      batch_it(50)

    expect_length(area_codes, 4)
    expect_true(stringr::str_starts(area_codes[[1]][1], "E01012"))
  })
