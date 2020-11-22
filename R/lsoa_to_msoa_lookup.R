create_msoa_lookup <- function(hocl_names_version, nmw = TRUE) {

  hocl_msoa_names <- function(version, nmw = nmw) {

    msoa_cols <- 1:5
    # don't include Welsh language names (codes ending "nmw")
    if (!nmw) msoa_cols <- c(1:2, 4)

    # HoC Library friendly MSOA names
    # https://visual.parliament.uk/msoanames
    paste0(
      "https://visual.parliament.uk/msoanames/static/",
      "MSOA-Names-",
      version,
      ".csv"
    ) %>%
      readr::read_csv() %>%
      dplyr::select(msoa_cols, lad19nm = 6)
  }

  build_api_query(
    table_code = "LSOA_2011_EW_NC",
    fields = c("lsoa11cd", "lsoa11nm"),
  ) %>%
    extract_lookup() %>%
    dplyr::mutate(
      msoa11nm = stringr::str_remove(lsoa11nm, "[A-Z]{1}$")
    ) %>%
    dplyr::left_join(hocl_msoa_names(hocl_names_version)) %>%
    dplyr::relocate(msoa11nm, .after = msoa11cd)

}


lsoa_to_msoa_lookup <- function(df, hocl_names_version, nmw = TRUE, keep = FALSE) {

  # get msoa lookup (above)
  # select lsoa columns only from df
  # do lookup to msoa (left_join)
  # do left_join with original df %>%
  # {
  # only if (!keep):
  # remove lsoa columns
  # distinct()
  # }

}
