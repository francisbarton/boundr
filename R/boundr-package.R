#' @keywords internal
"_PACKAGE"

#' @importFrom assertthat assert_that
#' @importFrom cli cli_alert_info
#' @importFrom dplyr across desc first if_all if_any if_else pick
#' @importFrom glue glue glue_data
#' @importFrom rlang .data arg_match caller_env is_interactive is_missing
#' @importFrom tidyselect all_of any_of ends_with
NULL

#' The base part of the OpenGeography API standard URLs, for convenience
#' 
#' @keywords internal
og_ <- \() "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services"

#' Batch a vector into a list
#' 
#' @param x a vector
#' @param batch_size the number of items in each batch of the returned list
#' @keywords internal
batch_it <- function(x, batch_size) {
  f <- rep(1:ceiling(length(x) / batch_size), each = batch_size)[seq_along(x)]
  unname(split(x, f))
}

#' This is the same as `%||%` but I prefer bracketed functions to infixes
#' 
#' @param x,y R objects
#' @keywords internal
ifnull <- \(x, y) if (is.null(x)) y else x

#' Conveniently wrap a regex in glue::glue and pass it to grepl
#' @param x A character vector that you want to check
#' @param rx A string that when processed by glue() will be used as a regex
#' @param ... arguments passed onto grepl()
#' @keywords internal
gregg <- \(x, rx, ...) grepl(glue(rx)[[1]], x, ...)
