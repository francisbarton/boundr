# Building requests -------------------------------


#' Basic OpenGeography API request
#'
#' @param url The API URL to query
#' @param append string to be appended to the URL path
#' @param format data format to return from the API, `"pjson"` by default
#' @param user_agent Allows user to specify a certain User Agent string in the
#'  header of each query. Sends the string `"boundr R package"` by default
#' @keywords internal
opengeo_api_req <- function(
    url,
    append = "0/query",
    format = "pjson",
    user_agent = "boundr R package (https://codeberg.org/francisbarton/boundr)"
) {
  url |>
    httr2::request() |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_url_path_append(append) |>
    httr2::req_url_query(f = format)
}



#' Build an API request for IDs only
#'
#' @inheritParams opengeo_api_req
#' @param where any geographical filters to be applied
#' @param ... any arguments to be passed to `opengeo_api_req()`
#' @keywords internal
build_id_req <- function(url, where, ...) {
  url |>
    opengeo_api_req(...) |>
    httr2::req_url_query(where = where) |>
    httr2::req_url_query(returnGeometry = "false") |>
    httr2::req_url_query(returnIdsOnly = "true")
}


#' Build an API request for table data or spatial data
#'
#' @inheritParams opengeo_api_req
#' @param ids The IDs of the data to be requested
#' @param fields Which fields to include in the table returned
#' @param geo Whether this query should "returnGeometry" (TRUE) or not
#' @param crs The Coordinate Reference System (CRS) code to use
#' @param ... Any arguments to be passed to `opengeo_api_req()`
#' @keywords internal
api_data_req <- function(ids, url, fields, geo = TRUE, crs = NULL, ...) {
  return_geo <- if (geo) "true" else "false"
  ids <- stringr::str_flatten(ids, collapse = ",")
  fields <- stringr::str_flatten(fields, collapse = ",")
  req <- url |>
    opengeo_api_req(...) |>
    httr2::req_url_query(objectIds = ids) |>
    httr2::req_url_query(outFields = fields) |>
    httr2::req_url_query(returnGeometry = return_geo) |>
    httr2::req_url_query(returnIdsOnly = "false")
  if (is.null(crs)) req else httr2::req_url_query(req, outSR = crs)
}



# Perform requests -------------------------------

#' Perform an API query using a request object
#'
#' @param req An `httr2` request object
#' @param max_tries integer. 3 by default (for now). Passed to `httr2::req_retry
#'  ()` and controls how many times to try to perform the request, should
#'  initial attempt(s) fail.
#' @param verbosity integer. 0 by default. Passed to `httr2::req_perform()` and
#'  controls the verbosity of the printed output from performing the request.
#'  Can be any integer from 0 to 3, or `NULL`. See `?req_perform` for more
#'  detail.
#' @keywords internal
query_opengeo_api <- function(req, max_tries = 3, verbosity = 0) {
  assert_that(
    verbosity %in% 0:3 | is.null(verbosity),
    msg = cli::format_error(c(
      "{.fn query_opengeo_api}: invalid value for `verbosity` parameter. ",
      "It must be an integer between 0 and 3, or NULL."
    ))
  )

  assert_that(
    is.numeric(max_tries) || is.null(max_tries),
    msg = cli::format_error(c(
      "{.fn query_opengeo_api}: {.var max_tries} must be a numeric (integer) ",
      "value, or {.var NULL}."
    ))
  )

  req |>
    httr2::req_retry(max_tries = max_tries) |>
    httr2::req_perform(verbosity = verbosity) |>
    httr2::resp_check_status()
}

#' @keywords internal
possibly_query_opengeo_api <- function(...) {
  purrr::possibly(query_opengeo_api)(...)
}

#' @keywords internal
possibly_parse_json <- function(...) {
  purrr::possibly(httr2::resp_body_json)(...)
}


# Response handling ----------------------------


#' Perform an API query and extract the returned IDs
#'
#' For large queries, just return IDs that can then be batched for full queries.
#' @inheritParams build_id_req
#' @inheritParams query_opengeo_api
#' @keywords internal
return_result_ids <- function(url, where, max_tries = 3, verbosity = 0, ...) {
  ret <- build_id_req(url, where, ...) |>
    possibly_query_opengeo_api(max_tries = max_tries, verbosity = verbosity)

  if (is.null(ret)) {
    cli::cli_abort("{.fn return_result_ids} returned NULL data")
    ret
  } else {
    data <- possibly_parse_json(ret)
    # Sometimes it gets returned with the wrong application type?
    # And it's JSON but not marked as such... needs to be parsed as plain text.
    if (is.null(data)) data <- jsonlite::fromJSON(httr2::resp_body_string(ret))

    purrr::list_c(purrr::pluck(data, "objectIds"))
  }
}

#' Perform an API query and handle the returned table data
#'
#' @inheritParams api_data_req
#' @inheritParams query_opengeo_api
#' @keywords internal
return_table_data <- function(
    ids,
    url,
    fields,
    max_tries = 3,
    verbosity = 0,
    ...) {
  ret <- api_data_req(ids, url, fields, geo = FALSE, ...) |>
    possibly_query_opengeo_api(max_tries = max_tries, verbosity = verbosity)

  if (is.null(ret)) {
    cli::cli_abort("{.fn return_table_data} returned NULL data")
    ret
  } else {
    # slightly more verbose than using purrr::map_df ... but safer?
    # https://github.com/tidyverse/purrr/issues/1007#issuecomment-1373624353
    tibble::tibble(
      data = ret |>
        httr2::resp_body_json() |>
        purrr::pluck("features") |>
        purrr::map("attributes")
      ) |>
      tidyr::unnest_wider("data") |>
      janitor::clean_names()
  }
}

#' Perform an API query and handle the returned spatial data
#'
#' @inheritParams api_data_req
#' @inheritParams query_opengeo_api
#' @keywords internal
return_spatial_data <- function(
    ids,
    url,
    fields,
    crs = 4326,
    max_tries = 3,
    verbosity = 0,
    ...) {
  ret <- api_data_req(ids, url, fields, geo = TRUE, crs, ...) |>
    possibly_query_opengeo_api(max_tries = max_tries, verbosity = verbosity)
  if (is.null(ret)) {
    cli::cli_abort("{.fn return_spatial_data} returned NULL data")
    ret
  } else {
    ret |>
      httr2::resp_body_string() |>
      sf::read_sf()
  }
}
