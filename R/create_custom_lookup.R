#' Uses build_api_query() to Retrieve, Tidy and Return a Lookup Table
#'
#' Roughly equivalent to \code{geo_get(boundaries = FALSE)}
#'
#' @param bounds_level The lowest level at which to return codes and names, eg
#'   "LSOA". Has to be one of "lsoa", "msoa", "wd/ward", "lad",
#'   "cty/county". Case-insensitive.
#' @param within The name of a geographic area to filter by eg "Swindon",
#'   "Gloucestershire", "Wales", or a set of area codes (with bounds_cd or
#'   within_cd).
#' @param within_level Upper geographic level to filter at. eg if filtering to
#'   find all LSOAs in a local authority, \code{within_level} will be "lad".
#'   Has to be one of "wd/ward", "lad/ltla", "cty/county", "utla/upper",
#'   "rgn/region", "cauth" or "ctry/country". Case-insensitive. Not all
#'   combinations of \code{bounds_level} and \code{within_level} make sense or
#'   are possible! NB "county" includes metropolitan counties such as "Tyne and
#'   Wear" and "West Midlands".
#' @param bounds_cd When you just supply a list of area codes for places you
#'   want boundaries for. NB this relates to lower areas, "bounds" level.
#' @param within_cd Usually you'll build the query with a place name to search
#'   within. But sometimes you may wish to pass in a vector of area codes
#'   instead (if that's all you have, or more likely if you are querying within
#'   wards, which don't have unique names (there's a lot of Abbey wards in
#'   England!)). If you're passing in area codes not names, set this to TRUE.
#'   NB this only applies to the higher, "within", level.
#' @param include_msoa If \code{bounds_level} = LSOA and return_style = "tidy",
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
#'   based on whether any of the areas returned have "W" codes.
#'
#' @keywords internal
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
create_custom_lookup <- function(bounds_level,
                                 within,
                                 within_level,
                                 bounds_cd = FALSE,
                                 within_cd = FALSE,
                                 include_msoa = NULL,
                                 return_style = "tidy",
                                 include_welsh_names = NULL) {


  # When returning LSOAs but not Wards, and return_style is "tidy" or
  # "full", tend to include MSOA columns, unless overridden by user param
  if (is.null(include_msoa) &&
      tolower(bounds_level) %in% c("oa", "coa", "lsoa") &&
      !tolower(within_level) %in% c("wd", "ward") &&
      return_style == "tidy") {
    include_msoa <- TRUE
  } else if (is.null(include_msoa)) {
    include_msoa <- FALSE
  }

  # notify if 'include_msoa' is set where it doesn't make any sense to
  if (include_msoa &&
      !tolower(bounds_level) %in% c("oa", "lsoa", "msoa")) {
    usethis::ui_oops(
      "'include_msoa' is set to TRUE but you are not retrieving data
      at a 'bounds_level' of OA, LSOA or MSOA, so this will not work.
      Setting 'include_msoa' to FALSE."
    )
    include_msoa <- FALSE
  }


  # when looking up LSOA -> MSOA, retain LSOA cols?
  keep_lsoa_cols <- TRUE

  if (tolower(bounds_level) == "msoa") {
    bounds_level <- "lsoa" # because OG doesn't have MSOA level lookups (?)
    keep_lsoa_cols <- FALSE
    include_msoa <- TRUE
  }



  # can only work with a single ref currently - ideally I will enable it
  # to work with two (or more?) refs for two-step lookups
  # eg MSOA to region (via LAD)
  table_code_ref_lookup <- dplyr::tribble(
    ~bounds_level, ~within_level, ~table_code_ref,

    "oa",     "oa",     1,
    "oa",     "lsoa",   1,
    "lsoa",   "lsoa",   1,
    "oa",     "msoa",   1,
    "msoa",   "msoa",   1,
    "oa",     "rgn",    1,
    "lsoa",   "rgn",    1,
    "oa",     "lad",    1, # 2 would return Wards
    "oa",     "wd",     2,
    "wd",     "wd",     3,
    "wd",     "lad",    3,
    "wd",     "cty",    3,
    "wd",     "rgn",    3,
    "wd",     "ctry",   3,
    "lad",    "cty",    3,
    "lad",    "rgn",    3,
    "ltla",    "rgn",    3,
    "lad",    "ctry",   3,
    "cty",    "rgn",    3,
    "cty",    "ctry",   3,
    "rgn",    "ctry",   3,
    "lad",    "cauth",  4,
    "lsoa",   "utla",   5,
    "lsoa",   "cty",    5, # cty needs to be renamed to utla here
    "lad",    "utla",   6, # lad needs to be renamed to ltla here
    "ltla",   "utla",   6,
    "lsoa",   "wd",     7, # important to keep this best-fit lookup, not to reverse-engineer from an OA:WD lookup (eg ref 2)
    "lsoa",   "lad",    7,
    "lsoa",   "ltla",   7,  # ltla needs to be renamed to lad here
    "lad",    "lad",    7,
    "rgn",    "rgn",    7
    # "lsoa",   "cauth",   2,     4,
    # "lsoa",   "ctry",    1,     4
  ) %>%
    dplyr::mutate(bounds_level = dplyr::case_when(
      stringr::str_ends(bounds_level, "oa") ~ paste0(bounds_level, "11cd"),
      stringr::str_ends(bounds_level, "la") ~ paste0(bounds_level, "21cd"),
      bounds_level == "rgn" ~ paste0(bounds_level, "21cd"),
      TRUE ~ paste0(bounds_level, "20cd")
    )) %>%
    dplyr::mutate(within_level = dplyr::case_when(
      stringr::str_ends(within_level, "oa") ~ paste0(within_level, "11nm"),
      stringr::str_ends(within_level, "la") ~ paste0(within_level, "21nm"),
      within_level == "rgn" ~ paste0(within_level, "21nm"),
      TRUE ~ paste0(within_level, "20nm")
    ))


  if (bounds_level == "lsoa") {
    if (within_level == "cty") {
      within_level <- "utla"
    }
    # if (within_level == "ltla") {
    #   within_level <- "lad"
    # }
  }

  if (bounds_level == "lad" && within_level %in% c("utla", "rgn")) {
    bounds_level <- "ltla"
  }


  # create a vector of field codes from the upper and lower levels supplied
  fields <- c(bounds_level, within_level) %>%
    build_fields()

  end_col <- length(fields)

  # "oa11nm" doesn't exist, of course, but you can cheekily get away with
  # requesting oa11cd twice and it seems not to mind, just returning a single
  # oa11cd column.
  # dplyr::select() doesn't mind if you pass a duplicated column name to it,
  # either... BRILLIANT!
  if (fields[2] == "oa11nm") fields[2] <- "oa11cd" # cheeky

  if (bounds_level == "oa" && within_level %in% c("wd", "ward")) {
    return_style <- "tidy"
  }

  if (fields[2] == fields[end_col]) {
    return_style <- "minimal"
  }


  table_code_ref <- table_code_ref_lookup %>%
    dplyr::filter(bounds_level == fields[1]) %>%
    dplyr::filter(within_level == fields[end_col]) %>%
    dplyr::pull(3)

  # A special case where we want MSOAs not Wards back with OA:LAD lookup.
  # Requires include_msoa = TRUE to be explicitly passed.
  # (if include_msoa is NULL|FALSE then ref will be 2, and wards returned)
  if (bounds_level == "oa" && within_level == "lad" && include_msoa) {
    table_code_ref <- 1
  }

  return_fields <- "*" # default for return_style = "tidy"


  # This is to avoid getting both Wards and MSOAs, where things get messy
  # because the lookups are all overlapped and you get more than one row
  # per L/MSOA, and download a load of duplicate boundaries.
  if (return_style == "tidy" &
      bounds_level == "lsoa" &
      within_level %in% c("lad", "ltla")
  ) {
    return_style <- "simple"
  }

  # this combo won't work! So switch to simple
  if (within_cd && return_style == "minimal") {
    return_style <- "simple"
  }


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
  treat_results <- function(df, return_style = "tidy") {

    # check that the parameter is valid
    # TODO: I think I should do this with match.args() or sth
    if (!return_style %in% c("tidy", "simple", "minimal")) {
      usethis::ui_warn("'return_style' parameter not correctly specified.
                       Options are \"tidy\", \"simple\", \"minimal\".
                       Setting to \"tidy\"")
      return_style <- "tidy"
    }

    if (return_style == "minimal") {
      df1 <- df %>%
        # return just first two columns
        dplyr::select(!!rlang::sym(fields[1]):!!rlang::sym(fields[2]))
    } else if (return_style == "simple") {
      df1 <- df %>%
        # return just the columns in "fields"
        dplyr::select(any_of(fields))
    } else {
      df1 <- df %>%
        # return all columns between first and last specified fields
        dplyr::select(!!rlang::sym(fields[1]):!!rlang::sym(fields[end_col]))
    }

    df1 %>%
      dplyr::distinct() %>%
      janitor::remove_empty("cols")
  }


  # use last field by default, but use penultimate if using codes not names
  nth_field <- -1
  if (within_cd) nth_field <- -2
  if (bounds_cd) nth_field <- 1

  where_level <- dplyr::nth(fields, nth_field)

  if (all(fields %in% names(oa_lad21_lookup))) {
    df1 <- oa_lad21_lookup %>%
      dplyr::filter(.data[[where_level]] %in% within) %>%
      dplyr::distinct()
  } else {
    # and for other things use the API as usual:
    df1 <- within %>%
      batch_it_simple(batch_size = 25) %>% # from my myrmidon pkg
      purrr::map_df( ~ build_api_query(
      ref = table_code_ref,
      where_level = where_level,
      where = .,
      fields = return_fields
      ) %>%
      extract_lookup()
      )
  }

  df_out <- df1 %>%
    treat_results(return_style = return_style)


  # if not specified by the user, make an educated decision about
  # including Welsh language MSOA and LAD names (LAD19NMW / MSOA11NMW /
  # MSOA11HCLNMW, where relevant)
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
  if ("lad21nm" %in% colnames(df_out) && include_welsh_names) {
    df_out <- df_out %>%
      dplyr::left_join(lad21nmw_lookup) %>%
      dplyr::relocate(lad21nmw, .after = lad21nm)
  }


  if (!include_msoa) {
    df_out
  } else {
    df_out %>%
      lsoa_to_msoa_lookup(keep = keep_lsoa_cols, nmw = include_welsh_names)
  }
}
