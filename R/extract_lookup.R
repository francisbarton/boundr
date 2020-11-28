#' Helper function
#'
#' @param x a JSON return from an API query
#' @return a data frame
extract_lookup <- function(x) {
  x %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("features", "attributes") %>%
    janitor::clean_names()
}
