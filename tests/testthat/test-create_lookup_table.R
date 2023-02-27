"create lookup 1" |>
  test_that({
    lookup_query_data <- return_lookup_query_info(lookup = "wd", within = "lad", country_filter = "EN", option = 1)
    within_names <- "Stroud"
    within_codes <- NULL
    within_code_field <- lookup_query_data[["within_field"]]
    within_name_field <- sub("cd$", "nm", within_code_field)

    within <- c(
      within_name_field |>
        paste0(
          " = ",
          (paste0("'", toupper(within_names), "'"))
        ) |>
        utils::head(length(within_names)),
      within_code_field |>
        paste0(
          " = ",
          (paste0("'", toupper(within_codes), "'"))
        ) |>
        utils::head(length(within_codes))
    ) |>
      stringr::str_c(collapse = " OR ")

    expect_identical(
      within_name_field,
      "lad22nm"
    )
    expect_identical(
      within,
      "lad22nm = 'STROUD'"
    )

    ids <- lookup_query_data[["query_url"]] |>
      return_result_ids(within) |>
      unique()

    expect_equal(length(ids), 37L)
  })


"create lookup 2" |>
  test_that({
    lookup_query_data <- return_lookup_query_info(
      lookup = "lsoa",
      within = "lad",
      option = 1)
    within_names <- "Swindon"
    within_codes <- NULL
    within_code_field <- lookup_query_data[["within_field"]]
    within_name_field <- sub("cd$", "nm", within_code_field)

    within <- c(
      within_name_field |>
        paste0(
          " = ",
          (paste0("'", toupper(within_names), "'"))
        ) |>
        utils::head(length(within_names)),
      within_code_field |>
        paste0(
          " = ",
          (paste0("'", toupper(within_codes), "'"))
        ) |>
        utils::head(length(within_codes))
    ) |>
      stringr::str_c(collapse = " OR ")

    expect_identical(
      within_name_field,
      "lad22nm"
    )
    expect_identical(
      within,
      "lad22nm = 'SWINDON'"
    )

    ids <- lookup_query_data[["query_url"]] |>
      return_result_ids(within) |>
      unique()

    expect_equal(length(ids), 140L)
  })


"create lookup 4" |>
  test_that({

    lookup_query_data <- return_lookup_query_info(lookup = "wd", within = "lad", country_filter = "EN", option = 1)
    within_names <- "Stroud"
    within_codes <- NULL

    lookup_code_field <- lookup_query_data[["lookup_field"]]
    lookup_name_field <- sub("cd$", "nm", lookup_code_field)
    within_code_field <- lookup_query_data[["within_field"]]
    within_name_field <- sub("cd$", "nm", within_code_field)

    within <- c(
      within_name_field |>
        paste0(
          " = ",
          (paste0("'", toupper(within_names), "'"))
        ) |>
        utils::head(length(within_names)),
      within_code_field |>
        paste0(
          " = ",
          (paste0("'", toupper(within_codes), "'"))
        ) |>
        utils::head(length(within_codes))
    ) |>
      stringr::str_flatten(collapse = " OR ")


    return_width <- "minimal"

    fields <- switch(return_width,
                     "tidy" = "*",
                     "basic" = c(lookup_code_field, lookup_name_field, within_code_field, within_name_field),
                     "full" = "*",
                     "minimal" = c(lookup_code_field, lookup_name_field))

    expect_identical(
      fields,
      c("wd22cd", "wd22nm")
    )

    ids <- lookup_query_data[["query_url"]] |>
      return_result_ids(within) |>
      unique()

    expect_equal(length(ids), 37L)

    out <- return_table_data(ids, lookup_query_data[["query_url"]], fields)

    expect_equal(nrow(out), 37L)
    expect_equal(ncol(out), 2L)
  })
