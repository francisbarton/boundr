#' Title
#'
#' @param bounds_level
#' @param within
#' @param within_level
#' @param return
#'
#' @return
#' @export
#'
#' @examples
create_custom_lookup <- function(
  bounds_level,
  within,
  within_level,
  include_msoa = NULL,
  return = "tidy" # can be "all", "simple" or "minimal"
  ) {


  if (include_msoa && !tolower(bounds_level) %in% c("lsoa", "msoa")) {
    usethis::ui_info(
      "'include_msoa' is set to TRUE but you are not retrieving data
      at a 'bounds_level' of LSOA or MSOA, so this will not work.
      Setting 'include_msoa' to FALSE."
      )
    include_msoa <- FALSE
  }

  if (is.null(include_msoa) && tolower(bounds_level) == "lsoa" && !to_lower(within_level) %in% c("wd", "ward") && !return %in% c("simple", "minimal")) {
    include_msoa <- TRUE
  } else if (is.null(include_msoa)) {
    include_msoa <- FALSE
  }


  if (tolower(bounds_level) == "msoa") {
    bounds_level <- "lsoa"
    keep_lsoa_cols <- FALSE
    include_msoa <- TRUE
  }


  area_code_lookup <- dplyr::tribble(
    ~ friendly, ~ serious,
    "lsoa",    "lsoa11",
    "msoa",    "msoa11",
    "wd",      "wd19",
    "ward",    "wd19",
    "lad",     "lad19",
    "ltla",    "ltla19",
    "lower",   "ltla19",
    "utla",    "utla19",
    "upper",   "utla19",
    "county",  "cty19",
    "cauth",   "cauth19",
    "rgn",     "rgn19",
    "region",  "rgn19",
    "ctry",    "ctry19",
    "country", "ctry19"
  )

  table_code_ref_lookup <- dplyr::tribble(
    ~ bounds_level, ~ within_level, ~ table_code_ref1, ~ table_code_ref2,

    "wd",     "lad",    1,    NULL,
    "wd",     "cty",    1,    NULL,
    "wd",     "rgn",    1,    NULL,
    "wd",     "ctry",   1,    NULL,
    "lad",    "cty",    1,    NULL,
    "lad",    "rgn",    1,    NULL,
    "lad",    "ctry",   1,    NULL,
    "cty",    "rgn",    1,    NULL,
    "lad",    "cauth",  2,    NULL,
    "ltla",   "utla",   3,    NULL,
    "lsoa",   "utla",   4,    NULL,
    "msoa",   "utla",   4,    NULL,
    "lsoa",   "wd",     5,    NULL,
    "lsoa",   "lad",    5,    NULL,
    "msoa",   "lad",    5,    NULL
    # "utla",   "rgn"     1,    3,
    # "lsoa",   "cauth"   2,    5,
    # "msoa",   "cauth"   2,    5,
    # "msoa",   "rgn"     1,    5,
    # "msoa",   "ctry"    1,    5
  ) %>%
    dplyr::mutate(bounds_level = dplyr::case_when(
      stringr::str_ends(bounds_level, "oa") ~ paste0(bounds_level, "11cd"),
      TRUE ~ paste0(bounds_level, "19cd")
    )) %>%
    dplyr::mutate(within_level = paste0(within_level, "19nm"))


  get_serious <- function(x) {
    area_code_lookup %>%
      dplyr::filter(friendly %in% tolower(x)) %>%
      dplyr::pull(serious)
  }

  extract_lookup <- function(x) {
    x %>%
      jsonlite::fromJSON() %>%
      purrr::pluck("features", "attributes") %>%
      janitor::clean_names()
  }


  fields <- c(bounds_level, within_level) %>%
    get_serious() %>%
    rep(each = 2) %>%
    paste0(c("cd", "nm"))


  table_code_refs <- table_code_ref_lookup %>%
    dplyr::filter(bounds_level == fields[1]) %>%
    dplyr::filter(within_level == fields[4]) %>%
    dplyr::select(3:4) %>%
    # c() %>%
    unlist()


  return_fields <- "*"
  end_col <- 4


  if (return == "simple") {
    return_fields <- fields
  }

  if (return == "minimal") {
    return_fields <- fields[1:2]
    end_col <- 2
  }

  treat_results <- function(df, return) {

    if (!return %in% c("tidy", "all", "simple", "minimal")) {
      usethis::ui_warn("return parameter not correctly specified")
    }

    if (return == "all") {
      df <- df %>%
        dplyr::select(!!rlang::sym(fields[1]):!!rlang::sym(fields[end_col])) %>%
        dplyr::distinct()
    }

    if (return == "tidy") {
      df <- df %>%
        dplyr::select(!!rlang::sym(fields[1]):!!rlang::sym(fields[end_col])) %>%
        dplyr::distinct() %>%
        janitor::remove_empty("cols")
    }

    if (return %in% c("simple", "minimal")) {
      df <- df %>%
        dplyr::distinct()
    }

    df
  }

  # create another lookup (if necessary) for automatically looking
  # up the type and server parameters required for each table_code;
  # currently assuming that type="census" and server="feature" work for all!?
  build_api_query(
    table_code_ref = table_code_refs[1],
    search_within = fields[4],
    locations = within,
    fields = return_fields
    ) %>%
    extract_lookup() %>%
    treat_results(return = return) # %>%
    # convert_lsoa_to_msoa



}


create_custom_lookup(bounds_level = "lsoa", within = "Swindon", within_level = "lad", return = "simple")



