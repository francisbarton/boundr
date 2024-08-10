#' Use an existing tibble as the basis for a spatial query
#'
#' If you have a tibble such as those produced by `create_lookup_table()` -
#' that is, there is a column of geographical ONS codes ending in 'cd' -
#' simply use this table as the basis for retrieving the relevant boundaries.
#'
#' @param tbl A tibble with a column for geographical codes. This function will
#'  use the lefthand-most column ending in 'cd' as the basis for retrieving
#'  boundary or point data
#' @param centroids Whether to retrieve centroids or boundaries (the default)
#' @inheritParams bounds
#'
#' @returns If successful, will return the initial table with an additional
#'  geometry column added. Duplicate rows will be removed.
#'
#' @export
add_bounds_to_table <- function(
  tbl,
  resolution = c("BGC", "BSC", "BUC", "BFC", "BFE"),
  centroids = FALSE,
  crs = NULL) {

  if (is.null(crs)) {
    if (!is.null(sf::st_crs(tbl))) {
      crs <- sf::st_crs(tbl)
    } else {
      crs <- 4326 # default
    }
  }

  tbl <- sf::st_drop_geometry(tbl)

  geo_code_field <- tbl |>
    dplyr::select(ends_with("cd")) |>
    dplyr::select(1) |> # select the leftmost matching column
    names()

  lookup <- grep("^[Aa-Zz]+", geo_code_field, value = TRUE)

  if (centroids) {
    query_base_url <- pull_centroid_query_url(geo_code_field, lookup)
  } else {
    query_base_url <- pull_bounds_query_url(geo_code_field, lookup, resolution)
  }

  area_codes <- tbl |>
    dplyr::pull({{ geo_code_field }}) |>
    batch_it(50) |> # turns out this limit is rather crucial!
    purrr::map(\(x) build_flat_query(var = geo_code_field, vec = x))

  ids <- area_codes |>
    purrr::map(\(x) return_result_ids(url = query_base_url, where = x)) |>
    purrr::list_c()

  assert_that(is.vector(ids) & !is.list(ids) & length(ids),
    msg = "bounds: return_result_ids() has not returned a vector of IDs.")

  bounds_data <- ids |>
    purrr::map(\(x) return_spatial_data(x, query_base_url, crs))

  assert_that(is.list(bounds_data) & length(bounds_data),
    msg = "bounds: return_spatial_data() has not returned a list of length > 0")

  bounds_data_df <- bounds_data |>
    # purrr::list_rbind() |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    janitor::clean_names() |>
    dplyr::select(all_of(c(geo_code_field, "geometry")))

  assert_that(inherits(bounds_data_df, "data.frame"),
    msg = "bounds: bounds_data could not be row-bound into a data frame")

  assert_that(inherits(bounds_data_df, "sf"),
    msg = "bounds: the data frame bounds_data_df does not have 'sf' class")

  bounds_data_df |>
    dplyr::left_join(tbl, {{ geo_code_field }}) |>
    dplyr::relocate(names(tbl)) |>
    dplyr::distinct()
  }

