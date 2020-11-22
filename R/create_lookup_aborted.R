create_lookup <- function(

  retrieve_all,
  within,
  lookup_level



) {

  get_serious <- function(x) {
    area_code_lookup %>%
      dplyr::filter(friendly == x) %>%
      dplyr::pull(serious)
  }

  msoa_lookup <- function(

    retrieve_all,
    within,
    lookup_level


  ) {





    add_next_columns <- function(df, lookup_level, within) {

      next_col_lookup <- dplyr::tribble(
        ~ last_col, ~ next_col, ~ ref,
        "wd",     "lad19nm",   3,
        "lad",    "utla19nm",  7,
        "ltla",   "utla19nm",  7,
        "utla",   "cauth19nm", 8,
        "cauth",  "rgn19nm",   4,
        "rgn",    "ctry19nm",  5
      )



      next_col_row <- df %>%
        colnames() %>%
        tail() %>%
        stringr::str_match("^[:alpha:]+") %>%
        tolower() %>%
        dplyr::filter(.data = next_col_lookup, last_col == .)

      table_code <- next_col_row %>%
        dplyr::pull(ref) %>%
        dplyr::filter(.data = lookup_lookup_table, ref == .) %>%
        dplyr::pull(table_code)

      if (next_col_row %>%
          dplyr::pull(next_col) == lookup_level) {

      }


        build_api_query()





    }


    attempt_lookup <- function(df, lookup_level, within) {
      if (lookup_level %in% colnames(df)) {
        df <- df %>%
          dplyr::filter(!!ensym(lookup_level) == within)
        return(df)
      } else {
        df <- df %>%
          add_next_columns(within)
      }

    }




    create_msoa_lookup <- function(hocl_names_version) {

      hocl_msoa_names <- function(version) {

        # HoC Library friendly MSOA names
        # https://visual.parliament.uk/msoanames
        paste0(
          "https://visual.parliament.uk/msoanames/static/",
          "MSOA-Names-",
          version,
          ".csv"
        ) %>%
          readr::read_csv() %>%
          dplyr::rename(lad19nm = 6)
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



    msoa_table <- create_msoa_lookup("1.7")

    if (lookup_level %in% colnames(msoa_table)) {
      msoa_table <- msoa_table %>%
      dplyr::filter(!!ensym(lookup_level) == within)
      return(msoa_table)
    } else {
      msoa_table <- msoa_table %>%
        add_lad_columns()
    }

    if (lookup_level %in% colnames(msoa_table)) {
      msoa_table <- msoa_table %>%
      dplyr::filter(!!ensym(lookup_level) == within)
      return(msoa_table)
    } else {
      msoa_table <- msoa_table %>%
        add_region_columns()
    }

    if (lookup_level %in% colnames(msoa_table)) {
      msoa_table <- msoa_table %>%
        dplyr::filter(!!ensym(lookup_level) == within)
      return(msoa_table)
    } else {
      msoa_table <- msoa_table %>%
        add_country_columns()
    }


  }


  if ("msoa" %in% c(retrieve_all, lookup_level)) {
    msoa_table <- msoa_lookup(
      retrieve_all = paste0(get_serious(retrieve_all), "cd"),
      within = within,
      lookup_level = paste0(get_serious(lookup_level), "nm")
    )
    return(msoa_table)
  }


  table_code <- lookup_lookup_table %>%
    dplyr::filter(upper_level == lookup_level) %>%
    dplyr::filter(lower_level == retrieve_all) %>%
    dplyr::pull(table_code)

  if (is.null(table_code)) {
    usethis::ui_stop("No lookup is possible with those parameters.")
  }






}
