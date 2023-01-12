# Building requests -------------------------------


#' Basic OpenGeography API request
#' 
#' @param url The API URL to query
#' @param append string to be appended to the URL path
#' @param format data format to return from the API, `"pjson"` by default
#' @param user_agent Allows user to specify a certain User Agent string in the
#'   header of each query. Sends the string `"boundr R package"` by default
opengeo_api_req <- function(
  url,
  append = "0/query",
  format = "pjson",
  user_agent = "boundr R package (github.com/francisbarton/boundr)") {
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
build_id_req <- function(url, where, ...) {
  url |>
    opengeo_api_req(...) |>
    httr2::req_url_query(where = where) |>
    httr2::req_url_query(returnGeometry = "false") |>
    httr2::req_url_query(returnIdsOnly = "true")
}


#' Build an API request for table data
#' 
#' @inheritParams opengeo_api_req
#' @param ids the IDs of the data to be requested
#' @param fields which fields to include in the table returned
#' @param ... any arguments to be passed to `opengeo_api_req()`
table_data_req <- function(ids, url, fields, ...) {
  ids <- stringr::str_flatten(ids, collapse = ",")
  url |>
    opengeo_api_req(...) |>
    httr2::req_url_query(objectIds = ids) |>
    httr2::req_url_query(outFields = fields) |>
    httr2::req_url_query(returnGeometry = "false") |>
    httr2::req_url_query(returnIdsOnly = "false")
}



#' Build an API request for spatial boundary (geojson) data
#' 
#' @inheritParams table_data_req
#' @param crs the Coordinate Reference System (CRS) code to use. 4326 by
#'  default.
bounds_data_req <- function(ids, url, crs = 4326, ...) {
  ids <- stringr::str_flatten(ids, collapse = ",")
  url |>
    opengeo_api_req(format = "geojson", ...) |>
    httr2::req_url_query(objectIds = ids) |>
    httr2::req_url_query(outFields = "*") |>
    httr2::req_url_query(returnGeometry = "true") |>
    httr2::req_url_query(returnIdsOnly = "false") |>
    httr2::req_url_query(outSR = crs)
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
query_opengeo_api <- function(req, max_tries = 3, verbosity = 0) {
  assertthat::assert_that(verbosity %in% 0:3 | is.null(verbosity),
      msg = "query_opengeo_api: invalid value for verbosity parameter. it ust be an integer between 0 and 3, or NULL.")
  assertthat::assert_that(is.numeric(max_tries) | is.null(max_tries),
      msg = "query_opengeo_api: max_tries must be a numeric (integer) value, or NULL.")

  # just make sure it is an integer
  if (!is.null(max_tries)) max_tries <- round(max_tries)

  req |>
    httr2::req_retry(max_tries = max_tries) |>
    httr2::req_perform(verbosity = verbosity)
}

#' @noRd
possibly_query_opengeo_api <- function(...) {
  purrr::possibly(query_opengeo_api)(...)
}




# Response handling ----------------------------


#' Perform an API query and extract the returned IDs
#' 
#' For large queries, just return IDs that can then be batched for full queries.
#' @inheritParams build_id_req
#' @inheritParams query_opengeo_api
return_result_ids <- function(url, where, max_tries = 3, verbosity = 0, ...) {
  ret <- build_id_req(url, where, ...) |>
    possibly_query_opengeo_api(max_tries = max_tries, verbosity = verbosity)

  if (!is.null(ret)) {
    ret |>
      httr2::resp_body_json() |>
      purrr::pluck("objectIds") |>
      purrr::list_c()
  } else ret
}

#' Perform an API query and handle the returned table data
#' 
#' @inheritParams table_data_req
#' @inheritParams query_opengeo_api
return_table_data <- function(
    ids,
    url,
    fields,
    max_tries = 3,
    verbosity = 0,
    ...
  ) {
  ret <- table_data_req(ids, url, fields, ...) |>
    possibly_query_opengeo_api(max_tries = max_tries, verbosity = verbosity)

  if (!is.null(ret)) {
    ret |>
      httr2::resp_body_json() |>
      purrr::pluck("features") |>
      purrr::map("attributes") |>
      purrr::list_rbind() |>
      janitor::clean_names()
  } else ret
}

#' Perform an API query and handle the returned spatial data
#' 
#' @inheritParams bounds_data_req
#' @inheritParams query_opengeo_api
return_bounds_data <- function(
    ids,
    url,
    crs = 4326,
    max_tries = 3,
    verbosity = 0,
    ...
  ) {
  ret <- bounds_data_req(ids, url, crs, ...) |>
    possibly_query_opengeo_api(max_tries = max_tries, verbosity = verbosity)
  if (!is.null(ret)) {
    ret |>
      httr2::resp_body_string() |>
      sf::st_read(quiet = TRUE)
  } else ret
}
