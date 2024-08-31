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
    opts = boundr_options()) {
  fn <- "add_geometry_to_table"
  rs <- if (gm_type == "centroids") "(PopCentroids|PWC|AWC)" else opts[["rs"]]
  return_width <- opts[["rw"]]
  crs <- opts[["crs"]]
  query_option <- opts[["opt"]]
  
  # select the leftmost matching column name
  fallback_field <- grep(glue("^{l}.*cd$"), names(tbl), value = TRUE)[[1]]
  geo_code_field <- geo_code_field %||% fallback_field
  lookup <- lookup %||% stringr::str_extract(geo_code_field, ".*(?=\\d{2}cd$)")

  assert_that(
    length(geo_code_field) == 1 && !is.na(geo_code_field),
    msg = glue("{fun}: suitable geo_code_field not found from lookup table")
  )

  final_filter <- ifelse(points, "Centroids", toupper(resolution))
  query_base_url <- pull_query_url(geo_code_field, lookup, final_filter)

  if (is.null(query_base_url)) {
    gtype <- ifelse(points, "centroid", "boundary") # nolint
    gcf <- geo_code_field # nolint
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
    is.vector(ids) && !is.list(ids) && length(ids),
    msg = glue("{fun}: return_result_ids() has not returned a vector of IDs.")
  )

  spatial_data <- ids |>
    batch_it(500) |> # experimenting with 500 instead of 100
    purrr::map(
      \(x) return_spatial_data(x, query_base_url, crs),
      .progress = "Looking up spatial data"
    )

  assert_that(
    is.list(spatial_data) && length(spatial_data) > 0,
    msg = glue("{fun}: return_spatial_data() hasn't returned a list of data")
  )

  spatial_data_df <- spatial_data |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    janitor::clean_names()

  if (return_width != "full") {
    spatial_data_df <- spatial_data_df |>
      dplyr::select(c({{ geo_code_field }}, "geometry"))
  }

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

pull_query_url <- function(field, lookup, final_filter) {
  geo_regex <- glue("^{toupper(lookup)}.*{final_filter}")
  results <- opengeo_schema |>
    dplyr::filter(
      if_any(any_of(field), \(x) !is.na(x)) &
      if_any("service_name", \(x) grepl(geo_regex, x))
    ) |>
    janitor::remove_empty("cols")

  if (nrow(results) == 0) {
    NULL
  } else {
    results |>
      # any row of results should give the desired geo data. So take #1.
      dplyr::slice(1) |>
      dplyr::pull("service_url")
  }
}

batch_it <- function(x, batch_size) {
  f <- rep(1:ceiling(length(x) / batch_size), each = batch_size)[seq(length(x))]
  split(x, f)
}


ifnull <- \(x, y) if (is.null(x)) y else x

cd_colnames <- \(x) colnames(dplyr::select(x, ends_with("cd")))
