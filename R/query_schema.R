pull_geo_query_url <- function(geo_code_field, resolution, geo_option = 1) {

  # codes <- opengeo_schema %>%
  #   dplyr::filter(!stringr::str_detect(service_name, "_LU$")) %>%
  #   janitor::remove_empty("cols") %>%
  #   dplyr::select(ends_with("cd")) %>%
  #   janitor::remove_empty("rows") %>%
  #   names()
  #
  # x <- test_prefixes(codes, x)
  # x_code <- find_codes(codes, x, year)
  #
  # usethis::ui_info(
  #   stringr::str_glue("Using code {x_code}.")
  # )

  results <- opengeo_schema %>%
    dplyr::arrange(desc(edit_date)) %>%
    dplyr::filter(!stringr::str_detect(service_name, "_LU$")) %>%
    dplyr::filter(stringr::str_detect(service_name, toupper(resolution))) %>%
    dplyr::filter(!is.na(geo_code_field)) %>%
    janitor::remove_empty("cols")

  msg <- stringr::str_wrap(
    "No result was found for the parameters supplied. Try a different year or a different resolution?", 72)
  assertthat::assert_that(nrow(results) > 0, msg = msg)

  if (nrow(results) > 1) {
    usethis::ui_info(
      stringr::str_c(
        "More than 1 result found:",
        stringr::str_c(
          paste0(
            "\t(",
            seq(nrow(results)),
            ") ",
            results$service_name),
          collapse = "\n"),
        "Using option {geo_option}. (Change the `geo_option` parameter to use a different one.)", sep = "\n"))
  }

  results %>%
    dplyr::slice(geo_option) %>%
    dplyr::select(service_url, x_code = geo_code_field) %>%
    as.list()
}


pull_lookup_query_url <- function(x, y, year_x = NULL, year_y = NULL, country_filter = c("UK|EW|EN|WA", "UK", "EW", "EN", "WA"), option = 1) {

  countries <- match.arg(country_filter)

  # filter only lookup tables from the schema
  schema_lookups <- opengeo_schema %>%
    dplyr::filter(stringr::str_detect(service_name, "_LU$")) %>%
    janitor::remove_empty("cols")

  # make list of codes used in lookups
  schema1_names <- schema_lookups %>%
    dplyr::select(ends_with("cd")) %>%
    names()

  # make sure that x matches one of the
  x <- test_prefixes(schema1_names, x)
  x_code <- find_codes(schema1_names, x, year_x)

  schema2 <- schema1 %>%
    dplyr::filter(!across(x_code, is.na)) %>%
    janitor::remove_empty("cols")

  codes2 <- schema2 %>%
    dplyr::select(ends_with("cd")) %>%
    names()

  y <- test_prefixes(codes2, y)
  y_code <- find_codes(codes2, y, year_y)

  usethis::ui_info(
    stringr::str_glue("Using codes {x_code}, {y_code}.")
  )

  results <- schema2 %>%
    dplyr::filter(!across(y_code, is.na)) %>%
    dplyr::filter(stringr::str_detect(service_name, countries)) %>%
    dplyr::arrange(desc(edit_date))

  msg <- "No result was found for the parameters supplied. Try a different year or a different country filter?"
  assertthat::assert_that(nrow(results) > 0, msg = msg)

  if (nrow(results) > 1) {
    usethis::ui_info(
      stringr::str_c(
        "More than 1 result found:",
        stringr::str_c(
          paste0(
            "\t(",
            seq(nrow(results)),
            ") ",
            results$service_name),
          collapse = "\n"),
        "Using option {option}. (Change the `option` parameter to use a different one.)", sep = "\n"))
  }

  results %>%
    dplyr::slice(option) %>%
    dplyr::select(service_url, x_code = x_code, y_code = y_code) %>%
    as.list()
}





test_prefixes <- function(schema_names, prefix) {

  # create list of all available prefixes from the schema column names
  poss_prefixes <- unique(sub("(^[a-z]+)(.*)", "\\1", schema_names))


  if (prefix %in% poss_prefixes) prefix
  else {
    new_prefix <- grep(paste0("^", prefix), poss_prefixes, perl = TRUE, value = TRUE)[1]
    new_prefix <- stringr::str_match(poss_prefixes, paste0("^", prefix))
    if (nzchar(new_prefix)) {
      stringr::str_glue("Prefix not found. Using {new_prefix} instead.") %>%
        usethis::ui_info()
      new_prefix
    } else {
      stringr::str_glue("Prefix {prefix} not found.") %>%
        usethis::ui_stop()
    }
  }
}




find_codes <- function(codes, prefix, year = NULL, cutoff = 30) {

  out <- NULL

  poss_codes <- grep(paste0("(?<=^", prefix, ")\\d+"), codes, perl = TRUE, value = TRUE)
  assertthat::assert_that(length(poss_codes) > 0,
    msg = paste0("No codes found for prefix `", prefix, "`."))

  poss_years <- substr(poss_codes, nchar(prefix) + 1, nchar(prefix) + 2)
  assertthat::assert_that(length(poss_years) > 0,
    msg = paste0("No year codes found for prefix `", prefix, "`."))

  while (is.null(out) & cutoff > 10) {
    if (is.null(year)) {
      max_year <- poss_years %>%
        `[`(which(as.numeric(.) < cutoff)) %>%
        as.numeric() %>%
        max()
      out <- grep(paste0("^", prefix, max_year), codes, value = TRUE)[1]
    } else {
      if (nchar(year) == 1) year <- paste0(0, year)
      year <- sub(".*(?=[0-9]{2}$)", "", year, perl = TRUE)

      if (year %in% poss_years) {
        out <- grep(paste0("^", prefix, year), codes, value = TRUE)[1]
      } else {
        # find_codes(codes, prefix, year = NULL, cutoff = as.numeric(year))
        cutoff = as.numeric(year)
        year <- NULL
      }
    }
  }

  msg <- paste0("No matching code found for `",
                prefix,
                "`, `",
                year,
                "`.")
  assertthat::assert_that(is.character(out), msg = msg)
  assertthat::assert_that(nzchar(out), msg = msg)
  assertthat::assert_that(length(out) == 1, msg = msg)

  # return
  out
}
