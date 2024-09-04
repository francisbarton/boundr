test_that("return_narrow_table_info works overall", {
  ti1 <- return_narrow_table_info("msoa", 2021) |>
    expect_no_error()
  ti2 <- return_narrow_table_info("msoa", NULL) |>
    expect_no_error()
  expect_identical(ti1, ti2)
  return_narrow_table_info("msoa", 2023) |>
    expect_error("combination of levels and year has not returned a")
  ti3 <- return_narrow_table_info("msoa", NULL, rs = "BGC") |>
    expect_no_error()
  return_narrow_table_info("msoa", NULL, rs = "FEB") |> # FEB is a rarer resn.
    expect_error("No relevant lookup tables")

  expect_equal(nrow(ti1[["schema"]]), 1)
  expect_equal(nrow(ti3[["schema"]]), 2)
  expect_equal(ti1[["lookup_code"]], "msoa21cd")
}) |>
  rlang::with_interactive(FALSE)


"why doesn't this one work - let's see" |>
  test_that({
    # FIXED NOW!
    # lookup("lad", "ctry", "Wales") currently not working
    # but it should find a lookup...:
    # https://geoportal.statistics.gov.uk/datasets/
    # edf4bab38dc747fa9400fb52ae85a8ae_0/about
    return_lookup_table_info("lad", "ctry", NULL, NULL, FALSE) |>
      # expect_error("^That combination of levels and year")
      expect_no_error()
    lookup <- "lad"
    within_level <- "ctry"
    lookup_year <- NULL
    within_year <- NULL
    ul <- toupper(lookup)
    wl <- toupper(within_level)
    s1 <- opengeo_schema |>
      dplyr::filter(if_any("service_name", \(x) gregg(x, "{ul}.*{wl}.*_LU"))) |>
      janitor::remove_empty("cols")
    expect_gt(nrow(s1), 0)
    s1_names <- cd_colnames(s1)
    lu_code_field <- return_field_code(lookup, s1_names, lookup_year) |>
      expect_no_error()
    expect_equal(lu_code_field, "lad23cd")
    s2 <- dplyr::filter(s1, !if_any(any_of(lu_code_field), is.na)) |>
      janitor::remove_empty("cols")
    expect_equal(nrow(s2), 1)
    wn_code_field <- return_field_code(within_level, cd_colnames(s2), NULL)
    expect_equal(wn_code_field, "ctry23cd")
  }) |>
  rlang::with_interactive(FALSE)

"hmmm this one is returning all MSOAs in EW not just the Stroud ones!" |>
  test_that({
    # FIXED NOW
    dat <- lookup("msoa", "lad", "Stroud")
    expect_equal(nrow(dat), 14) # YAY
    query_info <- return_lookup_table_info(
      lookup = "msoa",
      within_level = "lad",
      lookup_year = NULL,
      within_year = NULL,
      joinable = FALSE
    )
    expect_equal(nrow(query_info[[1]]), 1)
    expect_equal(query_info[[2]], "msoa21cd")
    expect_equal(query_info[[3]], "lad24cd")

    query_data <- query_info |>
    process_query_info(
      within_names = "Stroud",
      within_codes = NULL,
      return_width = "full",
      query_opt = NULL
    )

    expect_true(grepl("MSOA21.*LAD24.*_LU", query_data[[1]]))
    expect_equal(query_data[[2]], "*")
    expect_equal(query_data[[3]], "lad24nm IN ('Stroud')")

    # tbl <- process_lookup_query_data(query_data) |>
    #   dplyr::bind_rows() |>
    #   janitor::clean_names()

  }) |>
  rlang::with_interactive(FALSE)

"this is working but for some reason taking AGES" |>
  test_that({
    # FIXED!
    dat <- lookup("icb", "nhser", "South East") |>
      expect_no_error()
    expect_equal(nrow(dat), 6)
    query_info <- return_lookup_table_info(
      lookup = "icb",
      within_level = "nhser",
      lookup_year = NULL,
      within_year = NULL,
      joinable = FALSE
    ) # OMG it's returning all the OAs
  }) |>
  rlang::with_interactive(FALSE)

"It can find a suitable lookup year based on within_year" |>
  test_that({
    lookup(
      "wd", "lad", "Shepway", within_year = 2016,
      opts = boundr_options(resolution = "SGCB")
    ) |>
      expect_no_error()

    query_info <- return_lookup_table_info(
      lookup = "wd",
      within_level = "lad",
      lookup_year = NULL,
      within_year = 2016,
      joinable = TRUE
    )
    expect_equal(nrow(query_info[[1]]), 8)
    expect_equal(query_info[[2]], "wd16cd")
    expect_equal(query_info[[3]], "lad16cd")

    query_data <- query_info |>
      process_query_info("Shepway", NULL, "tidy", NULL) |>
      expect_no_error()

    expect_equal(query_data[[1]], paste0(
      ogu(),
      "/WD16_REGD16_LAD16_EW_LU_e40c67c9fe5245f7871f8857c8cd022b",
      "/FeatureServer"
    ))
    expect_equal(query_data[[2]], c("wd16cd", "wd16nm", "lad16cd", "lad16nm"))
    expect_equal(query_data[[3]], "lad16nm IN ('Shepway')")

  })
