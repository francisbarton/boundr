#' Return centroid points at a specified level and area from the ONS OG API
#'
#' @inheritParams create_lookup_table
#' @inheritParams spatial_data_req
#'
#' @returns an `sfc` tibble (data frame with geometry)
#' @export
points <- function(
    lookup,
    within,
    within_names = NULL,
    within_codes = NULL,
    return_width = c("tidy", "full", "minimal"),
    lookup_year = NULL,
    within_year = NULL,
    country_filter = c("UK|EW|EN|WA", "UK", "EW", "EN", "WA"),
    option = NULL,
    include_welsh = FALSE,
    crs = 4326
) {
  lookup <- process_aliases(lookup)
  within <- process_aliases(within)
  return_width <- match.arg(return_width)
  country_filter <- match.arg(country_filter)

  lookup_table <- create_lookup_table(
    lookup,
    within,
    within_names,
    within_codes,
    return_width,
    lookup_year,
    within_year,
    country_filter,
    option,
    include_welsh)

  assert_that(nrow(lookup_table) > 0,
              msg = "points: create_lookup_table() has returned a table with 0 rows")

  # This isn't watertight: if you have a table with eg "lad16cd" and "lad21cd"
  # columns, it will pull the leftmost column to use for the geometry query,
  # which may not be what you want. This should only occur when you use the
  # 'full' return_width option.
  # It is fairly rare that more than 1 column name will match, and it can be
  # avoided in most cases by choosing a more appropriate 'option' parameter.
  # A 'tidy' or 'minimal' return_width parameter will eliminate this risk.
  geo_code_field <- lookup_table |>
    dplyr::select(starts_with(lookup) & ends_with("cd")) |>
    dplyr::select(1) |> # select the leftmost matching column
    names()

  assert_that(length(geo_code_field) == 1 & !is.na(geo_code_field),
              msg = "points: suitable geo_code_field not found from lookup table")

  query_base_url <- pull_centroid_query_url(geo_code_field, lookup)


  area_codes <- lookup_table |>
    dplyr::pull({{ geo_code_field }}) |>
    batch_it(50) |> # turns out this limit is rather crucial!
    purrr::map(\(x) paste_area_codes(var = geo_code_field, vec = x))

  ids <- area_codes |>
    purrr::map(\(x) return_result_ids(url = query_base_url, where = x)) |>
    purrr::list_c()

  assert_that(is.vector(ids) & !is.list(ids) & length(ids),
              msg = "points: return_result_ids() has not returned a vector of IDs.")


  points_data <- ids |>
    purrr::map(\(x) return_spatial_data(x, query_base_url, crs))

  assert_that(is.list(points_data) & length(points_data),
              msg = "points: return_bounds_data() has not returned a list of length > 0")

  points_data_df <- points_data |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    janitor::clean_names()

  assert_that(inherits(points_data_df, "data.frame"),
              msg = "points: bounds_data could not be row-bound into a data frame")

  assert_that(inherits(points_data_df, "sf"),
              msg = "points: the data frame bounds_data_df does not have 'sf' class")


  join_vars <- intersect(names(lookup_table), names(points_data_df))

  if (return_width %in% c("tidy", "minimal")) {
    points_data_df <- points_data_df |>
      dplyr::select(all_of(c(join_vars, "geometry")))
  }

  points_data_df |>
    dplyr::left_join(lookup_table, join_vars) |>
    dplyr::relocate(names(lookup_table)) |>
    dplyr::select(!any_of(c("fid", "objectid", "global_id"))) |>
    janitor::remove_empty("cols") |>
    dplyr::distinct()
}







#' @noRd
pull_centroid_query_url <- function(field, lookup) {
  results <- opengeo_schema |>
    dplyr::filter(if_any({{ field }}, \(x) !is.na(x))) |>
    dplyr::filter(
      if_any("service_name", \(x) stringr::str_starts(x, toupper(lookup)))) |>
    dplyr::filter(
      if_any("service_name", \(x) stringr::str_detect(x, "PWC|Centroids"))) |>
    janitor::remove_empty("cols")

  assert_that(nrow(results) > 0,
              msg = paste0(
                "No centroid data was found for the parameters supplied. ",
                "Try a different year or a different resolution?"))

  results |>
    # We assume any row of results will give the desired geo data. So take #1.
    dplyr::slice(1) |>
    dplyr::pull(all_of("service_url"))
}
