lsoa_to_msoa_lookup <- function(
  df,
  hocl_names_version = "1.7",
  nmw = TRUE,
  keep = FALSE) {

  create_msoa_lookup <- function(version, keep_nmw = TRUE) {

    hocl_msoa_names <- function(version, nmw = TRUE) {

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
        dplyr::select(
          all_of(msoa_cols)
          # , lad19nm = 6
          )
    }

    readr::read_csv(
      "https://opendata.arcgis.com/datasets/3ce71e53d9254a73b3e887a506b82f63_0.csv") %>%
      janitor::clean_names() %>%
      dplyr::select(1:2) %>%
      dplyr::mutate(
        msoa11nm = stringr::str_remove(lsoa11nm, "[A-Z]{1}$")
      ) %>%
      dplyr::left_join(hocl_msoa_names(
        version = version,
        nmw = keep_nmw)) %>%
      dplyr::relocate(msoa11nm, .after = msoa11cd)

  }

  # get msoa lookup (above)
  msoa_lookup <- create_msoa_lookup(version = hocl_names_version, keep_nmw = nmw)

  df_out <- df %>%
    dplyr::select(all_of(c("lsoa11cd", "lsoa11nm"))) %>%
    dplyr::left_join(msoa_lookup) %>%
    dplyr::left_join(df)

  if (keep) return(df_out)

  df_out %>%
    dplyr::select(!all_of(c("lsoa11cd", "lsoa11nm"))) %>%
    dplyr::distinct()

}
