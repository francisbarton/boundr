"simple Stroud test 1" |>
  test_that({
    resolution <- "bgc"
    geo_code_field <- "wd21cd"
    results <- opengeo_schema |>
      dplyr::arrange(desc(edit_date)) |>
      dplyr::filter(!stringr::str_detect(service_name, "_LU$")) |>
      dplyr::filter(stringr::str_detect(service_name, toupper(resolution))) |>
      dplyr::filter(!is.na(.data[[geo_code_field]])) |>
      janitor::remove_empty("cols")

      expect_length(results, 7)
      expect_true(all(results$has_geometry))
  })
