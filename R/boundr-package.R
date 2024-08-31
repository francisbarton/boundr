#' @keywords internal
"_PACKAGE"

#' @importFrom assertthat assert_that
#' @importFrom cli cli_abort cli_alert_info
#' @importFrom dplyr across desc first if_all if_any if_else pick
#' @importFrom glue glue
#' @importFrom rlang is_interactive arg_match
#' @importFrom tidyselect all_of any_of ends_with
NULL

#' @keywords internal
base_url <- function() {
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services"
}

#' @keywords internal
batch_it <- function(x, batch_size) {
  f <- rep(1:ceiling(length(x) / batch_size), each = batch_size)[seq_along(x)]
  split(x, f)
}

#' This is the same as `%||%` but I prefer to use bracketed fns than infixes
#' @keywords internal
ifnull <- \(x, y) if (is.null(x)) y else x

#' @keywords internal
gregg <- \(x, rx, ...) grepl(glue(rx)[[1]], x, ...)
