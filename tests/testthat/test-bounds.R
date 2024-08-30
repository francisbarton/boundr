test_that("overall - run examples", {
  expect_no_error(bounds("msoa", "lad", "Swansea"))
  # previous example (MSOA to LAD lookup from 2015 no longer available from API)
  expect_no_error(bounds("wd", "lad", "Shepway", within_year = 2016, opts = boundr_options(resolution = "SGCB")))
  expect_no_error(bounds("rgn", opts = opts(resolution = "BUC")))
  # par = "parish"
  bounds("par", "lad", "Isles of Scilly", opts = boundr_options(crs = 27700)) |>
    expect_no_error()
  expect_no_error(bounds("npark", within_names = "Bannau Brycheiniog"))
  expect_no_error(points("msoa", "utla", "Swindon"))
})


"gregg!" |>
  test_that({
    expect_no_error(bounds("msoa", "lad", "Swansea"))
    expect_no_error(common_spatial("msoa", "lad", "Swansea"))
    tbl <- expect_no_error(
      common_lookup(
        lookup_level = "msoa",
        within_level = "lad",
        within_names = "Swansea",
        within_codes = NULL,
        lookup_year = NULL,
        within_year = NULL,
        opts = boundr_options(),
        joinable = TRUE
    ))
    expect_no_error(add_geometry(tbl))
    opts <- boundr_options()
    gm_type <- "boundaries"
    rs <- if (gm_type == "centroids") "(PopCentroids|PWC|AWC)" else opts[["rs"]]
    return_width <- opts[["rw"]]
    expect_equal(return_width, "tidy")
    crs <- opts[["crs"]]
    expect_equal(crs, 4326)
    query_option <- opts[["opt"]]
    lookup_level <- NULL
    geo_code_field <- NULL
    l <- ifnull(lookup_level, "[a-z]")
    fallback_field <- first(grep(glue("^{l}.*cd$"), names(tbl), value = TRUE))
    gcf <- ifnull(geo_code_field, fallback_field)
    expect_equal(gcf, "msoa21cd")
    fallback_lookup <- stringr::str_extract(gcf, ".*(?=\\d{2}cd$)")
    expect_equal(fallback_lookup, "msoa")
    lookup_level <- ifnull(lookup_level, fallback_lookup)
    query_url <- pull_query_url(gcf, lookup_level, rs)
    exp_url <- paste0(
      og_(),
      "/Middle_layer_Super_Output_Areas_December_2021_Boundaries_EW_BGC_V3",
      "/FeatureServer"
    )
    expect_equal(query_url, exp_url)
  })



"test pull_query_url()" |>
  test_that({
    geo_code_field <- "msoa21cd"
    ul <- "MSOA"
    rs <- "BGC"
    s1 <- opengeo_schema |>
      dplyr::filter(
        if_any(.data[[geo_code_field]], \(x) !is.na(x)) &
        if_any("service_name", \(x) gregg(x, "^{ul}.*{rs}"))
      ) |>
      janitor::remove_empty("cols") |>
      rlang::with_options(lifecycle_verbosity = "quiet") |>
      expect_no_error()
    expect_gt(nrow(s1), 0)
  })

"something not working - 'match needs vector' error" |>
  test_that({
    expect_no_error(bounds("rgn", opts = opts(resolution = "BUC")))
    expect_no_error(common_spatial("rgn", opts = opts(resolution = "BUC")))
    gm_type <- "boundaries"
    rs <- "(BUC)"
    return_width <- "tidy"
    crs <- 4326
    query_option <- NULL
    lookup_level <- "rgn"
    query_info <- return_narrow_bounds_info(lookup_level, NULL, rs) |>
      expect_no_error()
    query_data <- query_info |>
      process_query_info(NULL, NULL, return_width, query_option) |>
      expect_no_error()
    fields <- query_data[[2]]
    expect_equal(fields, c("rgn23cd", "rgn23nm"))
    expect_equal(
      query_data[[1]],
      paste0(og_(), "/Regions_December_2023_Boundaries_EN_BUC/FeatureServer")
    )
    tbl <- process_spatial_query_data(query_data, crs) |>
      dplyr::bind_rows() |>
      janitor::clean_names() |>
      dplyr::select(!any_of(drop_cols(crs))) |>
      dplyr::distinct() |>
      expect_no_error()
    tbl <- remove_nmw(tbl)
    tbl |>
      dplyr::distinct() |>
      janitor::remove_empty("cols") |>
      expect_no_error()
  })
