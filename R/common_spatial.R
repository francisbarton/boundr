#' @noRd
common_spatial <- function(
    lookup,
    within = NULL,
    within_names = NULL,
    within_codes = NULL,
    return_width = c("tidy", "full", "minimal"),
    lookup_year = NULL,
    within_year = NULL,
    country_filter = c("UK|GB|EW|EN|SC|WA", "UK", "GB", "EW", "EN", "SC", "WA"),
    option = NULL,
    crs = 4326,
    resolution = c("BGC", "BSC", "BUC", "BFC", "BFE"),
    centroids = FALSE
) {
  new_lookup <- process_aliases(lookup)
  country_filter <- match.arg(country_filter)
  resolution <- match.arg(resolution)

  lookup_table <- create_lookup_table(
    new_lookup,
    within,
    within_names,
    within_codes,
    return_width,
    lookup_year,
    within_year,
    country_filter,
    option,
    standalone = FALSE
  )

  assert_that(
    nrow(lookup_table) > 0,
    msg = glue("{fun}: create_lookup_table() has returned 0 rows")
  )

  geo_code_field <- names(lookup_table) |>
    stringr::str_subset(glue("^{new_lookup}.*cd$")) |>
    dplyr::first()

  add_geometry_to_table(
    lookup_table,
    centroids,
    new_lookup,
    geo_code_field,
    return_width,
    crs,
    resolution
  )
}
