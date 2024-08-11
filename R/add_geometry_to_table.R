#' Use an existing tibble as the basis for a spatial query
#'
#' If you have a tibble such as those produced by `create_lookup_table()` -
#' that is, there is a column of geographical ONS codes ending in 'cd' -
#' simply use this table as the basis for retrieving the relevant boundaries.
#'
#' @param tbl A tibble with a column for geographical codes. This function will
#'  use the lefthand-most column ending in 'cd' by default as the basis for
#'  retrieving boundary or point data
#' @param points Whether to retrieve area centroids (TRUE) or area boundaries
#'  (FALSE, the default)
#' @param geo_code_field character The column name from `tbl` for which you
#'  wish to retrieve spatial data. By default, `add_bounds_to_table()` will use
#'  the lefthand-most column that ends in "cd"
#' @inheritParams bounds
#'
#' @returns If successful, will return the initial table with an additional
#'  geometry column added. Duplicate rows will be removed.
#'
#' @export
add_geometry_to_table <- function(
    tbl,
    points = FALSE,
    lookup = NULL,
    geo_code_field = NULL,
    crs = 4326,
    resolution = c("BGC", "BSC", "BUC", "BFC", "BFE")
) {

  fun <- "add_geometry_to_table"
  l <- lookup %||% ".*"
  # select the leftmost matching column name
  fallback_field <- grep(glue("^{l}.*cd$"), names(tbl), value = TRUE)[[1]]
  geo_code_field <- geo_code_field %||% fallback_field
  lookup <- lookup %||% stringr::str_extract(geo_code_field, ".*(?=\\d{2}cd$)")

  assert_that(
    length(geo_code_field) == 1 & !is.na(geo_code_field),
    msg = glue("{fun}: suitable geo_code_field not found from lookup table")
  )

  final_filter <- ifelse(points, "PWC|Centroids", toupper(resolution))
  query_base_url <- pull_query_url(geo_code_field, lookup, final_filter)

  if (is.null(query_base_url)) {
    gtype <- ifelse(points, "centroid", "boundary")
    gcf <- geo_code_field
    cli::cli_abort(c(
      "Unfortunately no {gtype} data was found to match the code {.var {gcf}} ",
      "in the supplied table."
    ))
  }

  area_codes <- tbl |>
    dplyr::pull({{ geo_code_field }}) |>
    batch_it(50) |> # turns out this limit is rather crucial!
    purrr::map(\(x) build_flat_query(geo_code_field, x))

  ids <- area_codes |>
    purrr::map(\(x) return_result_ids(url = query_base_url, where = x)) |>
    purrr::list_c()

  assert_that(
    is.vector(ids) & !is.list(ids) & length(ids),
    msg = glue("{fun}: return_result_ids() has not returned a vector of IDs.")
  )

  spatial_data <- ids |>
    batch_it(500) |> # experimenting with 500 instead of 100
    purrr::map(
      \(x) return_spatial_data(x, query_base_url, crs),
      .progress = "Looking up spatial data"
    )

  assert_that(
    is.list(spatial_data) & length(spatial_data) > 0,
    msg = glue("{fun}: return_spatial_data() hasn't returned a list of data")
  )

  spatial_data_df <- spatial_data |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    janitor::clean_names() |>
    dplyr::select(c({{ geo_code_field }}, "geometry"))

  assert_that(
    inherits(spatial_data_df, "data.frame"),
    msg = glue("{fun}: spatial_data could not be row-bound into a data frame")
  )

  assert_that(
    inherits(spatial_data_df, "sf"),
    msg = glue("{fun}: the data frame spatial_data_df does not have 'sf' class")
  )

  join_vars <- intersect(names(tbl), names(spatial_data_df))

  spatial_data_df <- spatial_data_df |>
    dplyr::left_join(tbl, by = {{ join_vars }}) |>
    dplyr::relocate(names(tbl))

  spatial_data_df |>
    dplyr::select(!any_of(c("fid", "objectid", "global_id"))) |>
    janitor::remove_empty("cols") |>
    dplyr::distinct()
}


# Helper functions --------------------------------

#' @noRd
#' @keywords internal
pull_query_url <- function(field, lookup, final_filter) {
  results <- opengeo_schema |>
    dplyr::filter(
      if_any("has_geometry") &
      if_any(all_of({{ field }}), \(x) !is.na(x)) &
      if_any("service_name", \(x) stringr::str_starts(x, toupper(lookup))) &
      if_any("service_name", \(x) stringr::str_detect(x, {{ final_filter }}))
    ) |>
    janitor::remove_empty("cols")

  if (nrow(results) == 0) {
    NULL
  } else {
    results |>
      # We assume any row of results will give the desired geo data. So take #1.
      dplyr::slice(1) |>
      dplyr::pull("service_url")
  }
}
