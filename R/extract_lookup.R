#' Helper function
#'
#' @param x an API query string
#' @keywords internal
#' @return a data frame
extract_lookup <- function(x) {
  x %>%
    jsonlite::read_json() %>%
    purrr::pluck("features") %>%
    purrr::map_df("attributes") %>%
    janitor::clean_names()
}
