#' Use an existing tibble as the basis for a spatial query
#'
#' If you have a tibble such as those produced by `lookup()` - that is, there is
#'  a column of geographical ONS codes ending in 'cd' - simply use this table as
#'  the basis for retrieving the relevant boundaries or centroids.
#'  `add_geometry_to_table()` will use the lefthand-most column ending with
#'  "cd".
#'
#' @param tbl A tibble with a column containing ONS geographical codes.
#' @inheritParams bounds
#'
#' @returns If successful, will return the initial table with an additional
#'  geometry column added. Duplicate rows will be removed.
#' @examples
#' tibble::tibble(wd23cd = c("S13003001", "N08000520", "W05001522")) |>
#'   add_geometry_to_table()
#' @export
add_geometry_to_table <- function(
    tbl,
    opts = boundr_options(),
    geometry = c("boundaries", "centroids")) {
  fn <- "add_geometry_to_table"
  gm_type <- arg_match(geometry)
  rs <- if (gm_type == "centroids") "(PopCentroids|PWC|AWC)" else opts[["rs"]]
  return_width <- opts[["rw"]]
  crs <- opts[["crs"]]
  query_option <- opts[["opt"]]

  # select the leftmost column name ending with "cd"
  gcf <- first(grep("cd$", names(tbl), value = TRUE))

  assert_that(
    length(gcf) == 1 && !is.na(gcf),
    msg = glue("{.fn {fn}}: no valid field found from {.var {tbl}}")
  )

  lookup_level <- stringr::str_extract(gcf, ".*(?=\\d{2}cd$)")
  query_url <- pull_query_url(gcf, lookup_level, rs)

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
    janitor::clean_names()

  assert_that(
    inherits(spatial_data_df, "tbl_df"),
    msg = glue("{fn}: spatial_data could not be row-bound into a tibble")
  )

  assert_that(
    inherits(spatial_data_df, "sf"),
    msg = glue("{fn}: the data frame spatial_data_df does not have 'sf' class")
  )

  join_vars <- intersect(names(tbl), names(spatial_data_df))

  spatial_data_df |>
    dplyr::left_join(tbl, by = {{ join_vars }}) |>
    dplyr::relocate(names(tbl)) |>
    dplyr::select(!any_of(drop_cols(crs))) |>
    janitor::remove_empty("cols") |>
    dplyr::distinct()
}

#' @rdname add_geometry_to_table
#' @export
add_geometry <- add_geometry_to_table


# Helper functions --------------------------------

pull_query_url <- function(geo_code_field, lookup_level, rs) {
  ul <- toupper(lookup_level)
  s1 <- opengeo_schema |>
    dplyr::filter(
      if_any(.data[[geo_code_field]], \(x) !is.na(x)) &
      if_any("service_name", \(x) gregg(x, "^{ul}.*{rs}"))
    ) |>
    arrange_service_nms_by_res() |>
    janitor::remove_empty("cols") |>
    rlang::with_options(lifecycle_verbosity = "quiet")
  assert_that(nrow(s1) > 0, msg = no_table_msg("pull_query_url"))

  # Any row of results should give the desired geo data. (No need to do the
  # option handling we do in other functions.) Just take #1.
  s1[["service_url"]][[1]]
}

#' Arrange geo schema by preferred resolutions (only applies when no user pref)
#'
#' @param x schema tibble
#' @param r vector of resolution codes e.g. "BGC" "BFE"
#' @keywords internal
arrange_service_nms_by_res <- function(x, r = res_codes()) {
  safe_min <- \(x) suppressWarnings(min(x)) # It's OK if we get an Inf!
  score <- \(nm) safe_min(which(purrr::map_lgl(r, \(x) grepl(x, nm))))
  dplyr::arrange(x, purrr::map_dbl(.data[["service_name"]], score))
}
