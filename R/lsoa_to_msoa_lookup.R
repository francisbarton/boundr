#' Helper Function to Convert LSOA names to MSOA Codes and Names Including HOCL Names
#'
#' @param df data frame with at least column \code{lsoa11nm} and possibly \code{lsoa11cd}
#' @param hocl_names_version latest version of the House of Commons Library MSOA Names. Needs to be a string e.g. "1.8" (default)
#' @param nmw whether to keep the Welsh language MSOA names (ONS and HOCL) or not. Boolean. Default TRUE.
#' @param keep whether to keep the LSOA column(s) or just return a df starting at MSOA level. Default FALSE.
#'
#' @importFrom dplyr mutate select left_join relocate distinct all_of any_of
#' @importFrom httr GET content status_code
#' @importFrom janitor clean_names
#' @importFrom readr read_csv
#' @importFrom stringr str_remove
#'
#' @return a data frame (a lookup table)
#' @export
#'
#' @examples
lsoa_to_msoa_lookup <- function(
                                df,
                                hocl_names_version = "1.8",
                                nmw = TRUE,
                                keep = FALSE) {

    create_msoa_lookup <- function(version, keep_nmw = TRUE) {


    hocl_msoa_names <- function(version, nmw = TRUE) {
      msoa_cols <- 1:5
      # don't include Welsh language names (codes ending "nmw")
      if (!nmw) msoa_cols <- c(1:2, 4)

      # HoC Library friendly MSOA names
      # https://visual.parliament.uk/msoanames
      hocl_url <- paste0(
        "https://visual.parliament.uk/msoanames/static/",
        "MSOA-Names-",
        version,
        ".csv"
      )

      httr_out <- httr::GET(hocl_url)

      if (httr::status_code(httr_out) == 200) {
        hocl_msoa_names <- httr::content(httr_out, as = "text") %>%
        readr::read_csv()
      }

      hocl_msoa_names %>%
        dplyr::select(dplyr::all_of(msoa_cols))
    }

    # see data-raw/datasets.R for source
    lsoa11cdnm %>%
      janitor::clean_names() %>%
      dplyr::select(1:2) %>%
      dplyr::mutate(
        msoa11nm = stringr::str_remove(lsoa11nm, "[A-Z]{1}$")
      ) %>%
      dplyr::left_join(hocl_msoa_names(
        version = version,
        nmw = keep_nmw
      )) %>%
      dplyr::relocate(msoa11nm, .after = msoa11cd)
  }

  # get msoa lookup (above)
  msoa_lookup <- create_msoa_lookup(version = hocl_names_version, keep_nmw = nmw)

  df_out <- df %>%
    dplyr::select(dplyr::any_of(c("lsoa11cd", "lsoa11nm"))) %>%
    dplyr::left_join(msoa_lookup) %>%
    dplyr::left_join(df)

  if (keep) {
    return(df_out)
  }

  df_out %>%
    dplyr::select(!dplyr::any_of(c("lsoa11cd", "lsoa11nm", "wd19cd", "wd19nm"))) %>%
    dplyr::distinct()
}
