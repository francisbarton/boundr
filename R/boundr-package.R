#' @keywords internal
"_PACKAGE"

#' @importFrom assertthat assert_that
#' @importFrom cli cli_alert_info
#' @importFrom dplyr across desc first if_all if_any if_else pick
#' @importFrom glue glue glue_data
#' @importFrom rlang .data arg_match caller_env is_interactive is_missing
#' @importFrom tidyselect all_of any_of contains ends_with
NULL

#' The base part of the OpenGeography API standard URLs, for convenience
#'
#' @keywords internal
ogu <- \() "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services"

#' Batch a vector or list into a list of elements with a maximum size
#'
#' @param x A vector or list
#' @param batch_size integer. The size (length) of batches to create
#' @examples \dontrun{batch_it(letters, 6L)}
#' @returns A list
#' @keywords internal
batch_it <- function(x, batch_size) {
  assertthat::assert_that(
    rlang::is_vector(x),
    rlang::is_scalar_integerish(batch_size),
    batch_size >= 1L
  )
  bsize <- min(length(x), batch_size)

  # Create a vector of factors of length length(x), then pass this as the factor
  # argument to [split()].
  f <- rep(seq_len(ceiling(length(x) / bsize)), each = bsize)[seq_along(x)]
  unname(split(x, f))
}

#' This is the same as `%||%` but I just prefer bracketed functions to infixes
#'
#' @param x,y R objects
#' @keywords internal
ifnull <- \(x, y) if (is.null(x)) y else x


#' grepl a glued regex
#' @description Facilitates using regex in search/filter patterns, and puts the
#'  arguments "the right way round" (x first, then pattern), unlike [grepl()]
#' @param x A character vector to check
#' @param rx A string that after processing by [glue::glue_data()] will be used
#'  as a regex pattern in [grepl()]
#' @returns A logical value
#' @keywords internal
gregg <- \(x, rx, g = parent.frame()) grepl(glue::glue_data(g, rx), x)
