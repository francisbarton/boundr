"look up lsoa to msoa" |>
  test_that({
    lookup <- "lsoa"
    within <- "msoa"
    within_names <- "Stroud 001"
    within_codes <- NULL
    return_width <- "tidy"
    lookup_year <- NULL
    within_year <- NULL
    country_filter <- "EW"
    option <- 1
    chatty <- FALSE

    new_lookup <- process_aliases(lookup)
    expect_equal(new_lookup, "lsoa") # unchanged
    new_within <- process_aliases(within)
    expect_equal(new_within, "msoa") # unchanged

    lookup_query_info <- return_lookup_query_info(
      new_lookup,
      new_within,
      lookup_year,
      within_year,
      country_filter,
      option,
      chatty)

    expect_equal(
      lookup_query_info[["query_url"]],
      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/OA_LSOA_MSOA_EW_DEC_2021_LU_v3/FeatureServer"
    )

    expect_equal(
      lookup_query_info[["lookup_field"]],
      "lsoa21cd"
    )

    expect_equal(
      lookup_query_info[["within_field"]],
      "msoa21cd"
    )

    query_base_url <- lookup_query_info[["query_url"]]

    lookup_code_field <- lookup_query_info[["lookup_field"]]
    lookup_name_field <- sub("cd$", "nm", lookup_code_field)
    within_code_field <- lookup_query_info[["within_field"]]
    within_name_field <- sub("cd$", "nm", within_code_field)

    fields <- switch(
      return_width,
      "tidy" = c(
        lookup_code_field,
        lookup_name_field,
        within_code_field,
        within_name_field),
      "full" = "*",
      "minimal" = c(
        lookup_code_field,
        lookup_name_field)
    )

    expect_equal(fields, c("lsoa21cd", "lsoa21nm", "msoa21cd", "msoa21nm"))


    within_string <- within_name_field |>
      paste0(
        " IN (",
        stringr::str_flatten(
          paste0("'", within_names, "'"),
          collapse = ","),
        ")"
      ) |>
      head(length(within_names)) |>
      stringr::str_flatten(collapse = " OR ")

    expect_equal(within_string, "msoa21nm IN ('Stroud 001')")


    ids <- query_base_url |>
      return_result_ids(where = within_string) |>
      unique()

    expect_length(ids, 36)
    expect_true(all(is.numeric(ids)))

    batched_ids <- ids |>
      batch_it(100)

    expect_type(batched_ids, "list")
    expect_length(batched_ids, 1)
    expect_length(batched_ids[[1]], 36)

    table_data <- batched_ids |>
      purrr::map(\(x) return_table_data(x, query_base_url, fields)) |>
      dplyr::bind_rows() |>
      dplyr::distinct()

    expect_s3_class(table_data, "tbl_df")
    expect_equal(ncol(table_data), length(fields))
    expect_equal(nrow(table_data), 6)

    if (grepl("^msoa11", within_name_field)) hocl_tbl <- hocl_msoa11_names
    if (grepl("^msoa21", within_name_field)) hocl_tbl <- hocl_msoa21_names

    expect_identical(hocl_tbl, hocl_msoa21_names)

    out <- table_data |>
      dplyr::left_join(hocl_tbl, c(within_code_field, within_name_field))

    expect_named(out, c("lsoa21cd", "lsoa21nm", "msoa21cd", "msoa21nm", "msoa21nmw", "msoa21hclnm", "msoa21hclnmw"))

    any_welsh <- out |>
      dplyr::select(ends_with("cd")) |>
      dplyr::pull(1) |>
      purrr::some(\(x) grepl("^W", x))

    expect_false(any_welsh)

    if (!any_welsh) out <- out |>
      dplyr::select(!ends_with("nmw"))

    expect_length(out, 5)
    expect_equal(nrow(out), 6)
  })



"look up oa to msoa" |>
  test_that({
    lookup <- "oa"
    within <- "msoa"
    within_names <- "Stroud 001"
    within_codes <- NULL
    return_width <- "tidy"
    lookup_year <- NULL
    within_year <- NULL
    country_filter <- "EW"
    option <- 1
    chatty <- FALSE

    new_lookup <- process_aliases(lookup)
    expect_equal(new_lookup, "oa") # unchanged
    new_within <- process_aliases(within)
    expect_equal(new_within, "msoa") # unchanged

    lookup_query_info <- return_lookup_query_info(
      new_lookup,
      new_within,
      lookup_year,
      within_year,
      country_filter,
      option,
      chatty)

    expect_equal(
      lookup_query_info[["query_url"]],
      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/OA_LSOA_MSOA_EW_DEC_2021_LU_v3/FeatureServer"
    )

    expect_equal(
      lookup_query_info[["lookup_field"]],
      "oa21cd"
    )

    expect_equal(
      lookup_query_info[["within_field"]],
      "msoa21cd"
    )

    query_base_url <- lookup_query_info[["query_url"]]

    lookup_code_field <- lookup_query_info[["lookup_field"]]
    lookup_name_field <- sub("cd$", "nm", lookup_code_field)
    within_code_field <- lookup_query_info[["within_field"]]
    within_name_field <- sub("cd$", "nm", within_code_field)

    fields <- switch(
      return_width,
      "tidy" = c(
        lookup_code_field,
        lookup_name_field,
        within_code_field,
        within_name_field),
      "full" = "*",
      "minimal" = c(
        lookup_code_field,
        lookup_name_field)
    )

    fields <- stringr::str_subset(fields, "^oa.+nm$", negate = TRUE)

    expect_equal(fields, c("oa21cd", "msoa21cd", "msoa21nm"))


    within_string <- within_name_field |>
      paste0(
        " IN (",
        stringr::str_flatten(
          paste0("'", within_names, "'"),
          collapse = ","),
        ")"
      ) |>
      head(length(within_names)) |>
      stringr::str_flatten(collapse = " OR ")

    expect_equal(within_string, "msoa21nm IN ('Stroud 001')")


    ids <- query_base_url |>
      return_result_ids(where = within_string) |>
      unique()

    expect_length(ids, 36)
    expect_true(all(is.numeric(ids)))

    batched_ids <- ids |>
      batch_it(100)

    expect_type(batched_ids, "list")
    expect_length(batched_ids, 1)
    expect_length(batched_ids[[1]], 36)

    table_data <- batched_ids |>
      purrr::map(\(x) return_table_data(x, query_base_url, fields)) |>
      dplyr::bind_rows() |>
      dplyr::distinct()

    expect_s3_class(table_data, "tbl_df")
    expect_equal(ncol(table_data), length(fields))
    expect_equal(nrow(table_data), length(ids))

    if (grepl("^msoa11", within_name_field)) hocl_tbl <- hocl_msoa11_names
    if (grepl("^msoa21", within_name_field)) hocl_tbl <- hocl_msoa21_names

    expect_identical(hocl_tbl, hocl_msoa21_names)

    out <- table_data |>
      dplyr::left_join(hocl_tbl, c(within_code_field, within_name_field))

    expect_named(out, c("oa21cd", "msoa21cd", "msoa21nm", "msoa21nmw", "msoa21hclnm", "msoa21hclnmw"))

    any_welsh <- out |>
      dplyr::select(ends_with("cd")) |>
      dplyr::pull(1) |>
      purrr::some(\(x) grepl("^W", x))

    expect_false(any_welsh)

    if (!any_welsh) out <- out |>
      dplyr::select(!ends_with("nmw"))

    expect_length(out, 4)
    expect_equal(nrow(out), 36)
  })
