create_lookup_table <- function(lookup, within, within_names = NULL, within_codes = NULL, return_width = c("tidy", "basic", "full", "minimal"), lookup_year = NULL, within_year = NULL, country_filter = c("UK|EW|EN|WA", "UK", "EW", "EN", "WA"), option = 1) {

  # https://developers.arcgis.com/rest/services-reference/
  # enterprise/query-feature-service-layer-.htm

  country_filter <- match.arg(country_filter)
  return_width <- match.arg(return_width)
  lookup_query_data <- pull_lookup_query_url(lookup, within, lookup_year, within_year, country_filter, option)

  query_base_url <- lookup_query_data[["service_url"]]

  lookup_code_field <- lookup_query_data[["x_code"]]
  lookup_name_field <- lookup_code_field %>%
    gsub("CD$", "NM", .)
  within_code_field <- lookup_query_data[["y_code"]]
  within_name_field <- within_code_field %>%
    gsub("CD$", "NM", .)

  if (is.null(within_names) & is.null(within_codes)) {
    within <- "1=1"
  } else {
    within <- c(
      within_name_field %>%
        paste0(
          " = ",
          (within_names %>%
             toupper %>%
             paste0("'", ., "'"))) %>%
        utils::head(length(within_names)),
      within_code_field %>%
        paste0(
          " = ",
          (within_codes %>%
             toupper %>%
             paste0("'", ., "'"))) %>%
        utils::head(length(within_codes))
    ) %>%
      stringr::str_c(collapse = " OR ")}

  ids <- query_base_url %>%
    return_result_ids(within) %>%
    unique() %>%
    batch_it(2000)

  fields <- switch(return_width,
                   "tidy" = "*",
                   "basic" = c(lookup_code_field, lookup_name_field, within_code_field, within_name_field),
                   "full" = "*",
                   "minimal" = c(lookup_code_field, lookup_name_field)) %>%
    paste(., collapse = ",")

  out <- ids %>%
    purrr::map_df(return_query_data, query_base_url, fields)

  if (return_width == "tidy") {
    out <- out %>%
      dplyr::select(!!rlang::sym(tolower(lookup_code_field)):!!rlang::sym(tolower(within_name_field)))
  }

  out %>%
    dplyr::distinct() %>%
    janitor::remove_empty("cols")
}


return_result_ids <- function(url, within) {
  url %>%
    httr2::request() %>%
    httr2::req_url_path_append("/0/query") %>%
    httr2::req_url_query(where = within) %>%
    httr2::req_url_query(returnGeometry = "false") %>%
    httr2::req_url_query(returnIdsOnly = "true") %>%
    httr2::req_url_query(f = "json") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    purrr::pluck("objectIds") %>%
    purrr::flatten_int()
}

return_query_data <- function(ids, url, fields) {
  ids <- stringr::str_c(ids, collapse = ",")
  url %>%
    httr2::request() %>%
    httr2::req_url_path_append("/0/query") %>%
    httr2::req_url_query(objectIds = ids) %>%
    httr2::req_url_query(outFields = fields) %>%
    httr2::req_url_query(returnGeometry = "false") %>%
    httr2::req_url_query(returnIdsOnly = "false") %>%
    httr2::req_url_query(returnCountOnly = "false") %>%
    httr2::req_url_query(f = "json") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json() %>%
    purrr::pluck("features") %>%
    purrr::map_df("attributes") %>%
    janitor::clean_names()
}

batch_it <- function(x, batch_size) {
  f <- rep(1:ceiling(length(x) / batch_size), each = batch_size) %>%
    utils::head(length(x))
  split(x, f)
}
