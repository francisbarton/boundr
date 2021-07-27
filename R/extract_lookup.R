#' Helper function
#'
#' @param x an API query string (from build_api_query())
#' @keywords internal
#' @return a data frame
extract_lookup <- function(x) {
  x %>%
    # jsonlite::fromJSON() %>%
    jsonlite::read_json() %>%
    # purrr::pluck("features", "attributes") %>%
    purrr::pluck("features") %>%
    purrr::map_df("attributes") %>%
    janitor::clean_names()
}
