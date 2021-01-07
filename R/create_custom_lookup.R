#' Uses build_api_query() to Retrieve, Tidy and Return a Lookup Table
#'
#' Roughly equivalent to \code{geo_get(boundaries = FALSE)}
#'
#'
#' @param bounds_level The lowest level at which to return codes and names, eg
#'   "LSOA". Has to be one of "lsoa", "msoa", "wd/ward", "lad",
#'   "cty/county". Case-insensitive.
#' @param within The name of a geographic area to filter by eg "Swindon",
#'   "Gloucestershire", "Wales".
#' @param within_level Upper geographic level to filter at. eg if filtering to
#'   find all LSOAs in a local authority, \code{within_level} will be "lad". Has
#'   to be one of "wd/ward", "lad", "cty/county", "utla/upper", "rgn/region",
#'   "cauth" or "ctry/country". Case-insensitive. Not all combinations of
#'   \code{bounds_level} and \code{within_level} make sense or are possible! NB
#'   "county" includes metropolitan counties such as "Inner London", "Tyne and
#'   Wear" and "West Midlands".
#' @param include_msoa If \code{bounds_level} is LSOA and return_style is "tidy",
#'   whether to also include MSOA columns (in "tidy" return style). If
#'   \code{bounds_level} is MSOA, this will be forced to \code{TRUE}.
#' @param return_style "tidy" (the default) means all available columns between
#'   \code{bounds_level} and \code{within_level} will be returned, but with any
#'   empty columns removed. "simple" means that only the code and name (cd and
#'   nm) columns for \code{bounds_level} and \code{within_level} are returned -
#'   other columns are omitted. "minimal" means 'only return the columns for
#'   \code{bounds_level}'.
#' @param include_welsh_names Only makes a difference when \code{bounds_level} =
#'   msoa, or when \code{bounds_level} = lsoa and \code{return_style} = "tidy".
#'   \code{FALSE} returns no Welsh language columns. \code{TRUE} attempts to
#'   return Welsh language LAD and MSOA names where relevant. \code{NULL} (the
#'   default) means that an educated decision will be made by the program,
#'   based on whether any of the areas returned have "^W" codes.
#'
#' @return a data frame (tibble)
#' @examples
#' \dontrun{
#' create_custom_lookup(
#'   bounds_level = "msoa",
#'   within = "Swindon",
#'   within_level = "lad",
#'   return_style = "simple"
#' )
#' }
#' \dontrun{
#' create_custom_lookup(
#'   bounds_level = "msoa",
#'   within = "Swansea",
#'   within_level = "lad",
#'   return_style = "tidy"
#' )
#' }
#' # TODO add in more (and more interesting) examples?
create_custom_lookup <- function(
                                 bounds_level,
                                 within,
                                 within_level,
                                 include_msoa = NULL,
                                 return_style = "tidy",
                                 include_welsh_names = NULL) {

  # when looking up LSOA -> MSOA, retain LSOA cols?
  keep_lsoa_cols <- TRUE

  # When returning LSOAs but not Wards, and return_style is "tidy" or
  # "full", tend to include MSOA columns, unless overridden by user param
  if (is.null(include_msoa) && tolower(bounds_level) == "lsoa" && !tolower(within_level) %in% c("wd", "ward") && return_style == "tidy") {
    include_msoa <- TRUE
  } else if (is.null(include_msoa)) {
    include_msoa <- FALSE
  }

  # notify if 'include_msoa' is set where it doesn't make any sense to
  if (include_msoa && !tolower(bounds_level) %in% c("lsoa", "msoa")) {
    usethis::ui_oops(
      "'include_msoa' is set to TRUE but you are not retrieving data
      at a 'bounds_level' of LSOA or MSOA, so this will not work.
      Setting 'include_msoa' to FALSE."
    )
    include_msoa <- FALSE
  }

  if (tolower(bounds_level) == "msoa") {
    bounds_level <- "lsoa" # because OG doesn't have MSOA level lookups
    keep_lsoa_cols <- FALSE
    include_msoa <- TRUE
  }


  area_code_lookup <- dplyr::tribble(
    ~friendly, ~serious,
    "lsoa",    "lsoa11",
    "msoa",    "msoa11",
    "wd",      "wd19",
    "ward",    "wd19",
    "lad",     "lad19",
    "utla",    "utla19",
    "upper",   "utla19",
    "cty",     "cty19",
    "county",  "cty19",
    "cauth",   "cauth19",
    "rgn",     "rgn19",
    "region",  "rgn19",
    "ctry",    "ctry19",
    "country", "ctry19"
  )



  # can only work with a single ref currently - ideally I will enable it
  # to work with two (or more?) refs for two-step lookups
  # eg MSOA to region (via LAD)
  table_code_ref_lookup <- dplyr::tribble(
    ~bounds_level, ~within_level, ~table_code_ref1, ~table_code_ref2,

    "wd",     "lad",    1,  NULL,
    "wd",     "cty",    1,  NULL,
    "wd",     "rgn",    1,  NULL,
    "wd",     "ctry",   1,  NULL,
    "lad",    "cty",    1,  NULL,
    "lad",    "utla",   1,  NULL, # utla needs to be renamed to cty here
    "lad",    "rgn",    1,  NULL,
    "lad",    "ctry",   1,  NULL,
    "cty",    "rgn",    1,  NULL,
    "cty",    "ctry",   1,  NULL,
    "lad",    "cauth",  2,  NULL,
    "lsoa",   "utla",   3,  NULL,
    "lsoa",   "cty",    3,  NULL, # cty needs to be renamed to utla here
    "msoa",   "utla",   3,  NULL,
    "msoa",   "cty",    3,  NULL, # cty needs to be renamed to utla here
    "lsoa",   "wd",     4,  NULL,
    "lsoa",   "lad",    4,  NULL,
    "msoa",   "lad",    4,  NULL
    # "utla",   "rgn"     1,     3,
    # "lsoa",   "cauth"   2,     4,
    # "msoa",   "cauth"   2,     4,
    # "lsoa",   "rgn"     1,     4,
    # "msoa",   "rgn"     1,     4,
    # "lsoa",   "ctry"    1,     4
    # "msoa",   "ctry"    1,     4
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


  # create a vector of field codes from the upper and lower levels supplied
  fields <- c(bounds_level, within_level) %>%
    get_serious() %>%
    rep(each = 2) %>%
    paste0(c("cd", "nm"))


  table_code_refs <- table_code_ref_lookup %>%
    dplyr::filter(bounds_level == fields[1]) %>%
    dplyr::filter(within_level == fields[4]) %>%
    dplyr::select(3:4) %>%
    unlist() %>%
    unname() %>%
    c(recursive = TRUE)


  end_col <- length(fields)
  return_fields <- "*" # default for return_style = "tidy"


  # use return_style to decide how many columns to return
  if (return_style == "simple") {
    return_fields <- fields
  }

  # there's a question as to whether "minimal" should push include_welsh_names
  # to FALSE (unless TRUE is stipulated by the user)
  if (return_style == "minimal") {
    return_fields <- fields[1:2]
  }

  # maybe extract to an external helper function in another file?
  treat_results <- function(df, return_style) {

    # check that the parameter is valid
    if (!return_style %in% c("tidy", "simple", "minimal")) {
      usethis::ui_warn("'return_style' parameter not correctly specified.
                       Options are \"tidy\", \"simple\", \"minimal\".
                       Setting to \"tidy\"")
      return_style <- "tidy"
    }

    ## removed as negligible advantage over "tidy"
    # if (return_style == "all") {
    #   df <- df %>%
    #     # return all columns between first and last specified fields
    #     dplyr::select(!!rlang::sym(fields[1]):!!rlang::sym(fields[end_col])) %>%
    #     dplyr::distinct()
    # }

    if (return_style == "tidy") {
      df <- df %>%
        # return all columns between first and last specified fields
        # dplyr::select(fields[1]:fields[end_col]) %>%
        dplyr::select(!!rlang::sym(fields[1]):!!rlang::sym(fields[end_col])) %>%
        dplyr::distinct() %>%
        janitor::remove_empty("cols")
    }

    if (return_style %in% c("simple", "minimal")) {
      df <- df %>%
        dplyr::distinct()
    }

    df
  }

  # create another lookup (if necessary) for automatically looking
  # up the type and server parameters required for each table_code
  df_out <- build_api_query(
    table_code_ref = table_code_refs[1],
    within_level = fields[4],
    within = within,
    fields = return_fields,
    # TRUE is the default value, but I'm being explicit about it
    # for debugging purposes
    distinct = TRUE
  ) %>%
    extract_lookup() %>%
    treat_results(return_style = return_style)


  # if not specified by the user, make an educated decision about
  # including Welsh language MSOA and LAD names (LAD19NMW / MSOA11NMW /
  # MSOA11HOCLNMW, where relevant)
  # maybe extract to an external helper function in another file?
  if (is.null(include_welsh_names)) {
    check_w <- df_out %>%
      dplyr::select(dplyr::ends_with("cd")) %>%
      dplyr::pull(1) %>%
      stringr::str_match("^[A-Z]{1}") %>%
      unique()

    if ("W" %in% check_w) {
      include_welsh_names <- TRUE
    } else {
      include_welsh_names <- FALSE
    }
  }

  # add Welsh language LAD names if desired
  # maybe extract to an external helper function in another file?
  if ("lad19nm" %in% colnames(df_out) && include_welsh_names) {
    lad19nmw_lookup <- jsonlite::fromJSON("https://opendata.arcgis.com/datasets/35de30c6778b463a8305939216656132_0.geojson") %>%
      purrr::pluck("features", "properties") %>%
      janitor::clean_names() %>%
      dplyr::select(-fid)

    df_out <- df_out %>%
      dplyr::left_join(lad19nmw_lookup) %>%
      dplyr::relocate(lad19nmw, .after = lad19nm)
  }


  if (!include_msoa) {
    df_out
  } else {
    df_out %>%
      lsoa_to_msoa_lookup(keep = keep_lsoa_cols, nmw = include_welsh_names)
  }
}
