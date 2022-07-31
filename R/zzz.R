.onLoad <- function(lib, pkg) {
  safely_query_opengeo_api <<- purrr::safely(query_opengeo_api)
}
