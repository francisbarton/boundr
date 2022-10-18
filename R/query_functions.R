# Building requests -------------------------------


#' Basic OpenGeography API request
#' @param url The API URL to query
#' @param append string to be appended to the URL path
#' @param format data format to return from the API, `"pjson"` by default
#' @param user_agent Allows user to specify a certain User Agent string in the
#'   header of each query. Sends the string `"boundr R package"` by default
opengeo_api_req <- function(
  url,
  append = "/0/query",
  format = "pjson",
  user_agent = "boundr R package") {
  url %>% 
    httr2::request() %>%
    httr2::req_headers(UserAgent = user_agent) %>%
    httr2::req_url_path_append(append) %>% 
    httr2::req_url_query(f = format)
}



#' Build an API request for IDs only
#' @inheritParams opengeo_api_req
#' @param where any geographical filters to be applied
build_id_req <- function(url, where) {
  url %>% 
    opengeo_api_req() %>% 
    httr2::req_url_query(where = where) %>%
    httr2::req_url_query(returnGeometry = "false") %>%
    httr2::req_url_query(returnIdsOnly = "true")
}


#' Build an API request for table data
#' @inheritParams opengeo_api_req
#' @param ids the IDs of the data to be requested
#' @param fields which fields to include in the table returned
table_data_req <- function(ids, url, fields) {
  ids <- stringr::str_c(ids, collapse = ",")
  url %>%
    opengeo_api_req() %>% 
    httr2::req_url_query(objectIds = ids) %>%
    httr2::req_url_query(outFields = fields) %>%
    httr2::req_url_query(returnGeometry = "false") %>%
    httr2::req_url_query(returnIdsOnly = "false")
}



#' Build an API request for spatial boundary (geojson) data
#' @inheritParams table_data_req
#' @param crs the Coordinate Reference System (CRS) code to use
bounds_data_req <- function(ids, url, crs = 4326) {
  ids <- stringr::str_c(ids, collapse = ",")
  url %>%
    # opengeo_api_req(append = "0/query") %>%
    opengeo_api_req(format = "geojson") %>%
    httr2::req_url_query(objectIds = ids) %>%
    httr2::req_url_query(outFields = "*") %>%
    httr2::req_url_query(returnGeometry = "true") %>%
    httr2::req_url_query(returnIdsOnly = "false") %>%
    httr2::req_url_query(outSR = crs)
}





# Perform requests -------------------------------

#' Perform an API query using a request object
#' @param req An `httr2` request object
query_opengeo_api <- function(req) {
  req %>% 
    httr2::req_retry(max_tries = 3) %>% 
    httr2::req_perform()
}

#' In order to use purrr::safely() in the package. See R/zzz.R also.
safely_query_opengeo_api <- function(...) "dummy"




# Response handling ----------------------------


#' Perform an API query and extract the returned IDs
#' For large queries, just return IDs that can then be batched for full queries
#' @inheritParams build_id_req
return_result_ids <- function(url, where) {
  build_id_req(url, where) %>% 
    query_opengeo_api() %>% 
    # httr2::resp_body_json(check_type = FALSE) %>%
    httr2::resp_body_json() %>%
    purrr::pluck("objectIds") %>%
    purrr::flatten_int()
}

#' Perform an API query and handle the returned table data
#' @inheritParams table_data_req
return_table_data <- function(ids, url, fields) {
  table_data_req(ids, url, fields) %>% 
    query_opengeo_api() %>% 
    httr2::resp_body_json() %>%
    purrr::pluck("features") %>%
    purrr::map_df("attributes") %>%
    janitor::clean_names()
}

#' Perform an API query and handle the returned spatial data
#' @inheritParams bounds_data_req
return_bounds_data <- function(ids, url, crs) {
  bounds_data_req(ids, url, crs) %>% 
    query_opengeo_api() %>% 
    httr2::resp_body_string() %>%
    sf::st_read(quiet = TRUE)
}