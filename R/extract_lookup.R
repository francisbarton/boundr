extract_lookup <- function(x) {
  x %>%
    jsonlite::fromJSON() %>%
    purrr::pluck("features", "attributes") %>%
    janitor::clean_names()
}
