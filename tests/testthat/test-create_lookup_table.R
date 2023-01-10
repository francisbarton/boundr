"create lookup 1" |>
  test_that({
    lookup_query_data <- return_lookup_query_info(lookup = "wd", within = "lad", country_filter = "EN")
    within_names <- "Stroud"
    within_codes <- NULL
    within_code_field <- lookup_query_data[["y_code"]]
    within_name_field <- gsub("CD$", "NM", within_code_field)

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
      "LAD20NM"
    )
    expect_identical(
      within,
      "LAD20NM = 'STROUD'"
    )

    ids <- lookup_query_data[["service_url"]] |>
      return_result_ids(within) |>
      unique()

    expect_identical(
      ids,
      c(1202L, 1207L, 1217L, 1221L, 1228L, 1273L, 1278L, 1283L, 1289L, 1295L, 1301L, 1311L, 1315L, 1321L, 1327L, 1333L, 1339L, 1340L, 1341L, 1342L, 1492L, 1493L, 1494L, 1495L, 1496L, 1497L, 1498L, 1499L, 1500L, 1501L, 1502L, 1503L, 1504L, 1505L, 1506L))

  })


"create lookup 4" |>
  test_that({

    lookup_query_data <- return_lookup_query_info(lookup = "wd", within = "lad", country_filter = "EN")
    within_names <- "Stroud"
    within_codes <- NULL

    lookup_code_field <- lookup_query_data[["lookup_field"]]
    lookup_name_field <- gsub("CD$", "NM", lookup_code_field)
    within_code_field <- lookup_query_data[["within_field"]]
    within_name_field <- gsub("CD$", "NM", within_code_field)

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
                     "basic" = paste(lookup_code_field, lookup_name_field, within_code_field, within_name_field, collapse = ","),
                     "full" = "*",
                     "minimal" = paste(lookup_code_field, lookup_name_field, collapse = ","))

    expect_identical(
      fields,
      "WD20CD,WD20NM"
    )

    out <- ids |>
      purrr::map(\(ids) return_table_data(ids, lookup_query_data[["service_url"]], fields)) |>
      purrr::list_rbind()

    expect_equal(nrow(out), 35)
    expect_equal(length(out), 2)
  })
