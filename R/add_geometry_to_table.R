#' Use an existing tibble as the basis for a spatial query
#'
#' If you have a tibble such as those produced by `lookup()` - that is, there is
#'  a column of geographical ONS codes ending in 'cd' - simply use this table as
#'  the basis for retrieving the relevant boundaries or centroids.
#'
#' @param tbl A tibble with a column for geographical codes. Table names ought
#'  to be in lower case. This function will use the lefthand-most column ending
#'  in 'cd' by default as the basis for retrieving boundary or point data.
#' @param geo_code_field character The column name from `tbl` for which you
#'  wish to retrieve spatial data. By default, `add_geometry_to_table()` will
#'  use the lefthand-most column that ends in "cd".
#' @inheritParams bounds
#' @inheritParams common_spatial
#'
#' @returns If successful, will return the initial table with an additional
#'  geometry column added. Duplicate rows will be removed.
#' @aliases add_geometry
#' @export
add_geometry_to_table <- add_geometry <- function(
    tbl,
    geometry = c("boundaries", "centroids"),
    lookup = NULL,
    geo_code_field = NULL,
    opts = boundr_options()) {
  fn <- "add_geometry_to_table"
  rs <- if (gm_type == "centroids") "(PopCentroids|PWC|AWC)" else opts[["rs"]]
  return_width <- opts[["rw"]]
  crs <- opts[["crs"]]
  query_option <- opts[["opt"]]
  
  # select the leftmost matching column name
  l <- ifnull(lookup, "[a-z]")
  fallback_field <- first(grep(glue("^{l}.*cd$"), names(tbl), value = TRUE))
  gcf <- ifnull(geo_code_field, fallback_field)

  assert_that(
    length(gcf) == 1 && !is.na(gcf),
    msg = glue("{.fn {fn}}: no valid geo_code_field found from {.var {tbl}}")
  )

  # if lookup is NULL, use the first bit of the geo field (preceding eg '23cd')
  fallback_lookup <- stringr::str_extract(gcf, ".*(?=\\d{2}cd$)")
  lookup <- ifnull(lookup, fallback_lookup)

  query_url <- pull_query_url(gcf, lookup, rs)

  where_list <- unique(tbl[[gcf]]) |>
    batch_it(50L) |> # turns out this limit is rather crucial!
    purrr::map(\(x) build_flat_query(gcf, x))

  query_data <- list(
    query_url = query_url,
    fields = if (return_width == "full") "*" else gcf,
    where_list = where_list
  )

  spatial_data_df <- process_spatial_query_data(query_data, crs) |>
    dplyr::bind_rows() |>
    janitor::clean_names() |>
    dplyr::select(!any_of(c("object_id", "global_id", "chgind"))) |>
    dplyr::distinct()

  assert_that(
    inherits(spatial_data_df, "tbl_df"),
    msg = glue("{fn}: spatial_data could not be row-bound into a tibble")
  )

  assert_that(
    inherits(spatial_data_df, "sf"),
    msg = glue("{fn}: the data frame spatial_data_df does not have 'sf' class")
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

pull_query_url <- function(geo_code_field, lookup, rs) {
  fn <- "pull_query_url"
  s1 <- opengeo_schema |>
    dplyr::filter(
      if_any(any_of(geo_code_field), \(x) !is.na(x)) &
      if_any("service_name", \(x) gregg(x, "^{toupper(lookup)}.*{rs}"))
    ) |>
    janitor::remove_empty("cols")
  assert_that(nrow(s1) > 0, msg = no_table_msg(fn))

  # Any row of results should give the desired geo data. (No need to do the
  # option handling we do in other functions.) Just take #1.
  s1[["service_url"]][[1]]
}
