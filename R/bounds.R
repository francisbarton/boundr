#' Return boundary data at a specified level and area from the ONS OG API
#'
#' @inheritParams lookup
#' @param opts Should be set with the `boundr_options()` function. See
#'  `?boundr_options` for detail on what can be set by the user, and on the
#'  default values.
#' @param geometry character. Two options: "boundaries" (the default) and
#'  "centroids". By default, `bounds()` will return area boundaries. Set this
#'  explicitly to "centroids" to get area centroids instead.
#'
#' @examples
#' bounds("msoa", "lad", "Swansea")
#' bounds("wd", "lad", "Shepway", within_year = 2016) # Shepway no longer exists
#' bounds("rgn", opts = boundr_options(resolution = "BUC"))
#' bounds("par", "lad", "Isles of Scilly") # par = "parish"
#' bounds("spr")
#' bounds("npark", within_names = "Bannau Brycheiniog")
#' bounds("msoa", "utla", "Swindon", geometry = "centroids")
#'
#' @seealso boundr_options
#' @returns an `sfc` tibble (data frame with geometry)
#' @export
bounds <- function(
    lookup_level,
    within_level = NULL,
    within_names = NULL,
    within_codes = NULL,
    lookup_year = NULL,
    within_year = NULL,
    # Reduce clutter! https://design.tidyverse.org/argument-clutter.html
    opts = boundr_options(),
    # "Prefer an enum"! https://design.tidyverse.org/boolean-strategies.html
    geometry = c("boundaries", "centroids")) {
  gm_type <- arg_match(geometry)
  lookup_level <- tolower(lookup_level)
  rs <- if (gm_type == "centroids") "(PopCentroids|PWC|AWC)" else opts[["rs"]]
  return_width <- opts[["rw"]]
  crs <- opts[["crs"]]
  query_option <- opts[["opt"]]

  if (is.null(within_level)) {
    if (is.null(within_codes) && is.null(within_names)) {
      assert_that(
        !lookup_level %in% c("oa", "lsoa"),
        msg = paste0(
          "{boundr} won't let you download OA or LSOA geometry data without a ",
          "`within*` argument in place to filter the results a bit!\n",
          "Example: `bounds('lsoa', 'lad', within_names = 'Leeds')`"
        )
      )
    }
    query_info <- return_narrow_bounds_info(lookup_level, lookup_year, rs)
    query_data <- query_info |>
      process_query_info(within_names, within_codes, return_width, query_option)

    tbl <- process_spatial_query_data(query_data, crs) |>
      dplyr::bind_rows() |>
      janitor::clean_names() |>
      dplyr::select(!any_of(drop_cols(crs))) |>
      dplyr::distinct()
  } else {
    lookup_tbl <- common_lookup(
      lookup_level,
      within_level,
      within_names,
      within_codes,
      lookup_year,
      within_year,
      opts,
      joinable = TRUE
    )
    tbl <- add_geometry_to_table(lookup_tbl, opts, gm_type)
  }

  if (lookup_level == "msoa") tbl <- add_msoa_names(tbl)
  if (return_width != "full") tbl <- remove_nmw(tbl)

  tbl |>
    dplyr::distinct() |>
    janitor::remove_empty("cols")
}


#' Use this to set custom options for `bounds()`
#'
#' `opts()` is an alias for this function.
#'
#' @param resolution character. See `res_codes()` for the potential options. How
#'  generalised should the boundary be, and whether coastal boundaries should
#'  adhere to the coastline or to the full territorial extent. BGC by default
#'  (G = Generalised (20m), C = limited to the coastline.) F indicates Full
#'  resolution; S indicates Super-generalised (200m); U indicates Ultra-
#'  generalised (500m) boundary resolution. Use E instead of C for full
#'  extent boundaries (e.g. BFE). Not all combinations are available.
#'  Ignored if `geometry` is set to "centroids".
#' @param return_width character. How many of the possible columns in the
#'  returned table to keep. Options are "tidy", "full" or "minimal". "Tidy" aims
#'  to return four data columns (usually) - two columns for the lookup level
#'  codes and names, and two for the within level codes and names. Plus a
#'  geometry column. "Full" aims to return all data columns from the lookup.
#'  "Minimal" aims to return just the two (usually!) data columns relating to
#'  `lookup_level`.
#'  If `within` is not supplied then "tidy" will be equivalent to "minimal".
#' @param query_option numeric. Defaults to 1, which means that the URL will
#'  just be the first one from the list of possible services resulting from the
#'  level and year filters above. If this does not give you what you want, you
#'  can run the function again with a different option from the list.
#' @inheritParams api_data_req
#'
#' @examplesIf FALSE
#' boundr_options(crs = 27700) # Set the CRS to British National Grid
#' boundr_options(return_width = "full") # Ask boundr to return all data columns
#' @export
boundr_options <- function(
    resolution = res_codes(),
    return_width = c("tidy", "full", "minimal"),
    crs = 4326,
    query_option = NULL) {
  rw <- arg_match(return_width)
  list(rs = condense(resolution), rw = rw, crs = crs, opt = query_option)
}

#' @rdname boundr_options
#' @export
opts <- boundr_options

# Possible argument values ----------------

#' A list of all available resolutions for boundary geometries in the current
#' OpenGeography schema. Not all resolutions are available for all area types!
#' The most common ones are listed first, with the "generalised" (20m
#' resolution) BGC being the preferred option if you don't specify one.
#' @export
res_codes <- function() {
  c(
    "BGC", "BSC", "BUC", "BFC", "BGE", "BFE", "BUE",
    "GCB", "SGCB", "UGB", "UGCB", "FCB", "FEB", "BGG"
  )
}

# Helper functions -----------------------


#' @keywords internal
condense <- \(vec) glue("({paste0(vec, collapse = '|')})")


#' @keywords internal
return_narrow_bounds_info <- function(lookup, lookup_year, rs) {
  fn <- "return_narrow_bounds_info"
  ul <- toupper(lookup)

  # create initial filtered schema
  sp <- opengeo_schema |>
    dplyr::filter(if_any("service_name", \(x) gregg(x, "^{ul}.*_{rs}"))) |>
    janitor::remove_empty("cols")
  assert_that(nrow(sp) > 0, msg = no_table_msg(fn))
  sp_years <- as.numeric(stringr::str_extract(sp[["service_name"]], "\\d{4}"))
  lookup_year <- ifnull(lookup_year, max(sp_years))
  lu_code_field <- return_field_code(lookup, cd_colnames(sp), lookup_year, fn)
  assert_that(!is.null(lu_code_field), msg = no_lu_msg(fn))

  s2 <- dplyr::filter(sp, !if_any(.data[[lu_code_field]], is.na)) |>
    arrange_service_nms_by_res() |>
    janitor::remove_empty("cols") |>
    rlang::with_options(lifecycle_verbosity = "quiet")

  if (is_interactive()) cli_alert_info("Using {.val {lu_code_field}}")
  list(
    schema = s2,
    lookup_code = lu_code_field,
    within_code = NULL
  )
}


#' @keywords internal
cd_colnames <- \(x) colnames(dplyr::select(x, ends_with("cd")))


#' @keywords internal
drop_cols <- function(crs = NULL) {
  to_drop <- as.character(ifnull(crs, 0)) |>
    switch("4326" = c("bng_e", "bng_n"), "27700" = c("long", "lat"), NULL)
  c("fid", "object_id", "global_id", "chngind", to_drop)
}
