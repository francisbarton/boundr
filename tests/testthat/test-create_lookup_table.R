lookup_query_data <- return_lookup_query_url(x = "wd", y = "lad", country_filter = "EN")
within_names <- "Stroud"
within_codes <- NULL
within_code_field <- lookup_query_data[["y_code"]]
within_name_field <- within_code_field %>%
  gsub("CD$", "NM", .)

within <- c(
  within_name_field %>%
    paste0(
      " = ",
      (within_names %>%
         toupper %>%
         # stringr::str_replace_all(" ", "%20") %>%
         paste0("'", ., "'"))) %>%
    utils::head(length(within_names)),
  within_code_field %>%
    paste0(
      " = ",
      (within_codes %>%
         toupper %>%
         paste0("'", ., "'"))) %>%
    utils::head(length(within_codes))
) %>%
  stringr::str_c(collapse = " OR ")

"create lookup 1" %>%
  testthat::test_that({
    testthat::expect_identical(
      within_name_field,
      "LAD20NM"
    )
  })

"create lookup 2" %>%
  testthat::test_that({
    testthat::expect_identical(
      within,
      "LAD20NM = 'STROUD'"
    )
  })

"create lookup 3" %>%
  testthat::test_that({
    ids <- lookup_query_data[["service_url"]] %>%
      return_result_ids(within) %>%
      unique()
    testthat::expect_identical(
      ids,
      c(1202L, 1207L, 1217L, 1221L, 1228L, 1273L, 1278L, 1283L, 1289L, 1295L, 1301L, 1311L, 1315L, 1321L, 1327L, 1333L, 1339L, 1340L, 1341L, 1342L, 1492L, 1493L, 1494L, 1495L, 1496L, 1497L, 1498L, 1499L, 1500L, 1501L, 1502L, 1503L, 1504L, 1505L, 1506L))
  })

"create lookup 4" %>%
  testthat::test_that({
    lookup_code_field <- lookup_query_data[["x_code"]]
    lookup_name_field <- lookup_code_field %>%
      gsub("CD$", "NM", .)
    within_code_field <- lookup_query_data[["y_code"]]
    within_name_field <- within_code_field %>%
      gsub("CD$", "NM", .)

    return_width <- "minimal"

    fields <- switch(return_width,
                     "tidy" = "*",
                     "basic" = c(lookup_code_field, lookup_name_field, within_code_field, within_name_field),
                     "full" = "*",
                     "minimal" = c(lookup_code_field, lookup_name_field)
    )
    testthat::expect_identical(
      fields,
      c("WD20CD", "WD20NM")
    )
  })

"create lookup 5" %>%
  testthat::test_that({
    ids <- lookup_query_data[["service_url"]] %>%
      return_result_ids(within) %>%
      unique()
    lookup_code_field <- lookup_query_data[["x_code"]]
    lookup_name_field <- lookup_code_field %>%
      gsub("CD$", "NM", .)
    within_code_field <- lookup_query_data[["y_code"]]
    within_name_field <- within_code_field %>%
      gsub("CD$", "NM", .)
    return_width <- "minimal"
    fields <- switch(return_width,
                     "tidy" = "*",
                     "basic" = c(lookup_code_field, lookup_name_field, within_code_field, within_name_field),
                     "full" = "*",
                     "minimal" = c(lookup_code_field, lookup_name_field)) %>%
      paste(., collapse = ",")
    out <- ids %>%
      purrr::map_df(return_query_data, lookup_query_data[["service_url"]], fields)
    testthat::expect_type(out, "list")
    testthat::expect_equal(nrow(out), 35)
    testthat::expect_equal(length(out), 2)
  })
