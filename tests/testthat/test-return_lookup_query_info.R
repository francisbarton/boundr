"test 1" |>
  test_that({


  # filter only lookup tables from the schema
  schema_lookups <- opengeo_schema |>
    dplyr::filter(stringr::str_detect(service_name, "_LU$")) |>
    janitor::remove_empty("cols")

  # make list of codes used in lookups
  schema_names <- schema_lookups |>
    dplyr::select(ends_with("cd")) |>
    names()

  expect_vector(schema_names)

  x <- "wd"

  return_field_code <- function(prefix, year = NULL, names_vec) {
    if (is.null(year)) {
      years <- names_vec |>
        stringr::str_extract(str_glue("(?<=^{prefix})\\d+"))  |>
        as.numeric()
      year_out <- dplyr::case_when(
        years > 30 ~ years + 1900, # needs updating in 2030 ;-)
        TRUE ~ years + 2000
      ) |>
      sort() |>
      utils::tail(1) |>
      stringr::str_extract("\\d{2}$")
    } else {
      year_out <- year |>
      stringr::str_extract("\\d{1,2}$") |>
      stringr::str_pad(width = 2, "left", pad = "0")
    }

    field_code <- paste0(prefix, year_out, "cd")

    # some lookups contain LSOA but not MSOA. If the user has requested MSOA
    # and it's not available, we can search for LSOA instead, and later convert
    # back to MSOA, because LSOAs and MSOAs play nicely together of course
    if (prefix == "msoa" & !field_code %in% names_vec) {
      field_code <- sub("^msoa", "lsoa", field_code)
    }

    assertthat::assert_that(
      field_code %in% names_vec,
      msg = paste0(
        "return_lookup_query_info: ",
        "That combination of area levels and years has not returned a result. ",
        "Please try a different year."))

    # return
    field_code
  }

  # what to do if year_x is NULL
  years <- schema_names |>
    stringr::str_extract(str_glue("(?<=^{x})\\d+")) |>
    purrr::discard(is.na) |>
    as.numeric()
  x_year <- dplyr::case_when(
    years > 30 ~ years + 1900,
    TRUE ~ years + 2000
  ) |>
    sort() |>
    utils::tail(1) |>
    stringr::str_extract("\\d{2}$")

  # what to do if year_x is provided
  year_x <- 2011
  x_year <- stringr::str_extract(year_x, "\\d{1,2}$") |>
      stringr::str_pad(width = 2, side = "left", pad = "0")
  expect_identical(x_year, "11")
  year_x <- 9
  x_year <- stringr::str_extract(year_x, "\\d{1,2}$") |>
      stringr::str_pad(width = 2, side = "left", pad = "0")
  expect_identical(x_year, "09")

  x_name <- schema_names |>
      stringr::str_subset(str_glue("^{x}{x_year}")) |>
      head(1)
  x_name2 <- str_glue("{x}{x_year}cd")
  expect_identical(x_name, x_name2)

  x_name3 <- return_field_code(x, names_vec = schema_names)
  expect_identical(x_name3, "wd22cd")

  expect_error(return_field_code("msoa", 2022, schema_names))
  expect_identical(return_field_code("msoa", 2021, schema_names), "msoa21cd")
  expect_identical(return_field_code("wd", 20, schema_names), "wd20cd")
  expect_error(return_field_code("wd", 21, schema_names))
  expect_identical(return_field_code("ltla", 22, schema_names), "ltla22cd")
  expect_identical(return_field_code("lad", 2015, schema_names), "lad15cd")
})
