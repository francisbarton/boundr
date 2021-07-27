#' Helper Function to Convert LSOA names to MSOA Codes + Names
#'
#' @param df data frame with at least column \code{lsoa11nm} and possibly
#'   \code{lsoa11cd}.
#' @param nmw whether to keep the Welsh language MSOA names (ONS and HoCL) or
#'   not. Boolean. Default TRUE.
#' @param keep whether to keep the LSOA column(s) or just return a df starting
#'   at MSOA level. Default FALSE.
#' @keywords internal
#' @return a data frame (a lookup table)
#'
lsoa_to_msoa_lookup <- function(
                                df,
                                nmw = TRUE,
                                keep = FALSE) {


  create_msoa_lookup <- function(keep_nmw = TRUE) {

    hocl_msoa_names <- function(nmw = TRUE) {
      msoa_cols <- 1:5
      # don't include Welsh language names (codes ending "nmw")
      if (!nmw) msoa_cols <- c(1:2, 4)

      # The House of Commons Library's friendly MSOA names
      # https://visual.parliament.uk/msoanames
      paste0(
        "https://visual.parliament.uk/msoanames/static/",
        "MSOA-Names-Latest.csv") %>%
        readr::read_csv() %>%
        dplyr::select(dplyr::all_of(msoa_cols))
    }

    # see data-raw/datasets.R for source
    lsoa11cdnm %>%
      dplyr::mutate(
        msoa11nm = stringr::str_remove(lsoa11nm, "[A-Z]{1}$")
      ) %>%
      dplyr::left_join(hocl_msoa_names(nmw = keep_nmw)) %>%
      dplyr::relocate(msoa11nm, .after = msoa11cd)
  }


  # get msoa lookup (above)
  msoa_lookup <- create_msoa_lookup(keep_nmw = nmw)


  df_out <- df %>%
    # dplyr::select(dplyr::any_of(c("oa11cd", "lsoa11cd", "lsoa11nm"))) %>%
    dplyr::select(dplyr::starts_with(c("oa", "lsoa"))) %>%
    dplyr::distinct() %>%
    dplyr::left_join(msoa_lookup) %>%
    dplyr::left_join(df)

  if (keep) {
    df_out
  } else {
    df_out %>%
      dplyr::select(
        !dplyr::starts_with(c("oa", "lsoa", "wd"))
        ) %>%
      dplyr::distinct()
  }
}
