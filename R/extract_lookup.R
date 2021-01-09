#' Helper function
#'
#' @param x an API query string (from build_api_query())
#' @return a data frame
extract_lookup <- function(x) {
  x %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("features", "attributes") %>%
    janitor::clean_names()
}
