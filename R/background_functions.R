#' @param x Lower level area code eg "lsoa", "wd", "lad". Equivalent to the `lookup` parameter in `bounds()`.
#' @param y Higher level area code eg "lad", "cty". Equivalent to the `within` parameter in `bounds()`.
#' @param year_x A specific year for data relating to parameter `x`, if needed. Defaults to `NULL`, which will return the most recent data.
#' @param year_y A specific year for data relating to parameter `y`, if needed. Defaults to `NULL`, which will return the most recent data.
#' @param country_filter Open Geography datasets are sometimes available just within certain countries. Specify a country code if you want your results restricted to a certain country only - eg "WA" for Wales, "EW" for England and Wales. By default returns all options.
#' @param option Defaults to 1, which means that the URL will just be the first one from the list of possible services resulting from the level and year filters above. If this does not give you what you want, you can run the script again with a different option from the list.
#' @returns Aims to return the a list of length 3: the query URL, the lower level (`x`) code (eg `lsoa11cd`), and the higher level (`y`) code.
return_lookup_query_url <- function(x, y, year_x = NULL, year_y = NULL, country_filter = c("UK|EW|EN|WA", "UK", "EW", "EN", "WA"), option = 1) {

  country_filter <- match.arg(country_filter)
  
  
  assert_that(is.numeric(option))

  # filter only lookup tables from the schema
  # and those with the right country filter
  schema_lookups <- opengeo_schema %>%
    dplyr::filter(stringr::str_detect(service_name, "_LU$")) %>%
    dplyr::filter(stringr::str_detect(service_name, country_filter)) %>%
    janitor::remove_empty("cols")

  # make list of codes used in lookups
  schema_names <- schema_lookups %>%
    dplyr::select(ends_with("cd")) %>%
    names()

  return_field_code <- function(prefix, year = NULL, names_vec) {
    if (is.null(year)) {
      years <- names_vec %>% 
        stringr::str_extract(stringr::str_glue("(?<=^{prefix})\\d+"))  %>% 
        as.numeric()
      year_out <- dplyr::case_when(
        years > 30 ~ years + 1900,
        TRUE ~ years + 2000
      ) %>% 
      sort() %>% 
      tail(1) %>% 
      stringr::str_extract("\\d{2}$")
    } else {
      year_out <- year %>% 
      stringr::str_extract("\\d{1,2}$") %>% 
      stringr::str_pad(width = 2, "left", pad = "0")
    }

    field_code <- stringr::str_glue("{prefix}{year_out}cd")
    assert_that(field_code %in% names_vec,
    msg = "That combination of area levels and years has not returned a result. Please try a different year.")

    # return
    field_code
  }

  x_field <- return_field_code(x, year_x, schema_names)




  # reduce schema to only those matching x_field
  schema2 <- schema_lookups %>%
    dplyr::filter(if_any(.env[["x_field"]], ~ !is.na(.))) %>%
    janitor::remove_empty("cols")

  schema2_names <- schema2 %>%
    dplyr::select(ends_with("cd")) %>%
    names()

  y_field <- return_field_code(y, year_y, schema2_names)




  usethis::ui_info(
    stringr::str_glue("Using codes {x_field}, {y_field}.")
  )




  results <- schema2 %>%
    dplyr::filter(!is.na(y_field)) %>%
    janitor::remove_empty("cols")  %>% 
    dplyr::arrange(desc(edit_date))

  msg <- "No result was found for the parameters supplied. Try a different year or a different country filter?"
  assertthat::assert_that(nrow(results) > 0, msg = msg)

  if (nrow(results) > 1) {
      stringr::str_c(
        "More than 1 result found:",
        stringr::str_c(
          paste0(
            "\t(",
            seq(nrow(results)),
            ") ",
            results$service_name),
          collapse = "\n"),
        "Using option {option}. (Change the `option` parameter to use a different one.)", sep = "\n") %>% 
    usethis::ui_info()
  }

  # return query URL and x_field and y_fiel in a list,
  # to be passed on to create_lookup_table()
  results %>%
    dplyr::slice(option) %>%
    dplyr::select(service_url, x_code = .env[["x_field"]], y_code = .env[["y_field"]]) %>%
    as.list()
}



query_opengeo_api <- function(url, append = "0") {
  url %>%
    httr2::request() %>%
    httr2::req_headers(UserAgent = "boundr R package") %>%
    httr2::req_url_path_append(append) %>%
    httr2::req_url_query(f = "pjson") %>%
    httr2::req_retry(max_tries = 3) %>% 
    httr2::req_perform()
}

# safely_query_opengeo_api <- purrr::safely(query_opengeo_api)
safely_query_opengeo_api <- function(...) "dummy"

