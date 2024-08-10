#' Return boundary data at a specified level and area from the ONS OG API
#'
#' @inheritParams create_lookup_table
#' @inheritParams spatial_data_req
#' @param resolution character. See argument options for available options. How
#'  generalised should the boundary be, and whether coastal boundaries should
#'  adhere to the coastline or to the full territorial extent. BGC by default
#'  (G = Generalised (20m), C = limited to the coastline.) F indicates Full
#'  resolution; S indicates Super-generalised (200m); U indicates Ultra-
#'  generalised (500m) boundary resolution. Use E instead of C for full
#'  extent boundaries (e.g. BFE). Not all combinations are available.
#'
#' @examples
#' bounds("msoa", "lad", "Swansea")
#' bounds("msoa", "lad", "Shepway", lookup_year = 2011, within_year = 2015)
#' bounds("rgn", country_filter = "EN", resolution = "BUC")
#' bounds("parish", "utla", "Isles of Scilly")
#' bounds("spr")
#' bounds("npark", within_names = "Bannau Brycheiniog")
#'
#'
#' @returns an `sfc` tibble (data frame with geometry)
#' @export
bounds <- function(
  lookup,
  within = NULL,
  within_names = NULL,
  within_codes = NULL,
  return_width = c("tidy", "full", "minimal"),
  lookup_year = NULL,
  within_year = NULL,
  country_filter = c("UK|GB|EW|EN|SC|WA", "UK", "GB", "EW", "EN", "SC", "WA"),
  resolution = c("BGC", "BSC", "BUC", "BFC", "BFE"),
  option = NULL,
  crs = 4326
) {
  new_lookup <- process_aliases(lookup)
  country_filter <- match.arg(country_filter)
  resolution <- match.arg(resolution)

  if (!is.null(within)) {
    new_within <- process_aliases(within)
    return_width <- match.arg(return_width)

    lookup_table <- create_lookup_table(
      new_lookup,
      new_within,
      within_names,
      within_codes,
      return_width,
      lookup_year,
      within_year,
      country_filter,
      option)

    assert_that(
      nrow(lookup_table) > 0,
      msg = "bounds: `create_lookup_table()` has returned a table with 0 rows"
    )

    # This isn't watertight: if you have a table with eg "lad16cd" and "lad21cd"
    # columns, it will pull the leftmost column to use for the geometry query,
    # which may not be what you want. This should only occur when you use the
    # 'full' return_width option.
    # It is fairly rare that more than 1 column name will match, and it can be
    # avoided in most cases by choosing a more appropriate 'option' parameter.
    # A 'tidy' or 'minimal' return_width parameter will eliminate this risk.
    geo_code_field <- lookup_table |>
      dplyr::select(starts_with(new_lookup) & ends_with("cd")) |>
      dplyr::select(1) |> # select the leftmost matching column
      names()

    assert_that(
      length(geo_code_field) == 1 & !is.na(geo_code_field),
      msg = "bounds: suitable geo_code_field not found from lookup table"
    )

    area_codes <- lookup_table |>
      dplyr::pull({{ geo_code_field }}) |>
      batch_it(50) |> # turns out this limit is rather crucial!
      purrr::map(\(x) build_flat_query(var = geo_code_field, vec = x))

    query_base_url <- pull_bounds_query_url(geo_code_field, new_lookup, resolution)

    ids <- area_codes |>
      purrr::map(\(x) return_result_ids(url = query_base_url, where = x)) |>
      purrr::list_c()
  } else {
    lookup_table <- NULL

    bounds_query_info <- return_spatial_query_info(
      new_lookup,
      lookup_year,
      option
    )
    query_base_url <- bounds_query_info[["query_url"]]

    lookup_code_field <- bounds_query_info[["lookup_field"]]
    lookup_name_field <- sub("cd$", "nm", lookup_code_field)


    if (is.null(within_names) & is.null(within_codes)) {
      within_string <- "1=1"
    } else if (!is.null(within_names)) {
      within_string <- build_flat_query(lookup_name_field, within_names)
    } else if (!is.null(within_codes)) {
      within_string <- build_flat_query(lookup_code_field, within_codes)
    } else {
      within_string <- NULL
    }

    assert_that(!is.null(within_string),
                msg = "bounds: The within_names or within_codes arguments have not resulted in a valid query string.")

    ids <- query_base_url |>
      return_result_ids(where = within_string) |>
      unique()
  }


  assert_that(is.vector(ids) & !is.list(ids) & length(ids),
      msg = "bounds: return_result_ids() has not returned a vector of IDs.")

  bounds_data <- ids |>
    purrr::map(\(x) return_spatial_data(x, query_base_url, crs))

  assert_that(is.list(bounds_data) & length(bounds_data),
    msg = "bounds: return_spatial_data() has not returned a list of length > 0")

  bounds_data_df <- bounds_data |>
    dplyr::bind_rows() |>
    dplyr::distinct() |>
    janitor::clean_names()

  assert_that(inherits(bounds_data_df, "data.frame"),
    msg = "bounds: bounds_data could not be row-bound into a data frame")

  assert_that(inherits(bounds_data_df, "sf"),
    msg = "bounds: the data frame bounds_data_df does not have 'sf' class")


  if (!is.null(lookup_table)) {
    join_vars <- intersect(names(lookup_table), names(bounds_data_df))

    if (return_width %in% c("tidy", "minimal")) {
      bounds_data_df <- bounds_data_df |>
        dplyr::select(all_of(c(join_vars, "geometry")))
    }

    bounds_data_df <- bounds_data_df |>
      dplyr::left_join(lookup_table, join_vars) |>
      dplyr::relocate(names(lookup_table))
  }

  bounds_data_df |>
    dplyr::select(!any_of(c("fid", "objectid", "global_id"))) |>
    janitor::remove_empty("cols") |>
    dplyr::distinct()
}







# Helper functions --------------------------------

#' @noRd
pull_bounds_query_url <- function(field, lookup, resolution) {
  results <- opengeo_schema |>
    dplyr::filter(if_any("has_geometry")) |>
    dplyr::filter(if_any({{ field }}, \(x) !is.na(x))) |>
    dplyr::filter(if_any(
      "service_name", \(x) stringr::str_starts(x, toupper(lookup)))) |>
    dplyr::filter(if_any(
      "service_name", \(x) stringr::str_detect(x, toupper(resolution)))) |>
    janitor::remove_empty("cols")

  assert_that(nrow(results) > 0,
              msg = paste0(
              "No boundary data was found for the parameters supplied. ",
              "Try a different year or a different resolution?"))

  results |>
    # We assume any row of results will give the desired geo data. So take #1.
    dplyr::slice(1) |>
    dplyr::pull("service_url")
}


#' @noRd
build_flat_query <- function(var, vec) {
  y <- stringr::str_flatten(glue::glue("'{unique(vec)}'"), collapse = ",")
  glue::glue("{var} IN ({y})")
}


#' @noRd
batch_it <- function(x, batch_size) {
  f <- rep(1:ceiling(length(x) / batch_size), each = batch_size) |>
    head(length(x))
  split(x, f)
}
