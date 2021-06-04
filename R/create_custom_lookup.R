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
#' @param within_cd Usually you'll build the query with a place name to search
#'   within. But sometimes you may wish to pass in a vector of area codes
#'   instead (if that's all you have, or more likely if you are querying within
#'   wards, which don't have unique names (there's a lot of Abbey wards in
#'   England!)). If you're passing in area codes not names, set this to TRUE.
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
create_custom_lookup <- function(
                                 bounds_level,
                                 within,
                                 within_level,
                                 include_msoa = NULL,
                                 return_style = "tidy",
                                 within_cd = FALSE,
                                 include_welsh_names = NULL) {



  # when looking up LSOA -> MSOA, retain LSOA cols?
  keep_lsoa_cols <- TRUE

  # When returning LSOAs but not Wards, and return_style is "tidy" or
  # "full", tend to include MSOA columns, unless overridden by user param
  if (is.null(include_msoa) &
      tolower(bounds_level) %in% c("oa", "coa", "lsoa") &
      !tolower(within_level) %in% c("wd", "ward") &
      return_style == "tidy") {
    include_msoa <- TRUE
  } else if (is.null(include_msoa)) {
    include_msoa <- FALSE
  }

  # notify if 'include_msoa' is set where it doesn't make any sense to
  if (include_msoa &
      !tolower(bounds_level) %in% c("oa", "lsoa", "msoa")) {
    usethis::ui_oops(
      "'include_msoa' is set to TRUE but you are not retrieving data
      at a 'bounds_level' of OA, LSOA or MSOA, so this will not work.
      Setting 'include_msoa' to FALSE."
    )
    include_msoa <- FALSE
  }

  if (tolower(bounds_level) == "msoa") {
    bounds_level <- "lsoa" # because OG doesn't have MSOA level lookups (?)
    keep_lsoa_cols <- FALSE
    include_msoa <- TRUE
  }


  # the order of these really matters! bounds_level has to be higher in the
  # table than within_level :-) :-O Due to the way the 'fields' vector works.
  # Which all makes sense, it just means you can't add extra options willy-nilly
  # at the bottom. Don't ask how I know this.
  area_code_lookup <- dplyr::tribble(
    ~friendly, ~serious,
    "oa",      "oa11",
    "coa",     "oa11",
    "lsoa",    "lsoa11",
    "msoa",    "msoa11",
    "wd",      "wd20",
    "ward",    "wd20",
    "lad",     "lad20",
    "ltla",    "ltla21",
    "utla",    "utla21",
    "upper",   "utla21",
    "cty",     "cty20",
    "county",  "cty20",
    "cauth",   "cauth20",
    "rgn",     "rgn20",
    "region",  "rgn20",
    "ctry",    "ctry20",
    "country", "ctry20"
  )



  # can only work with a single ref currently - ideally I will enable it
  # to work with two (or more?) refs for two-step lookups
  # eg MSOA to region (via LAD)
  table_code_ref_lookup <- dplyr::tribble(
    ~bounds_level, ~within_level, ~table_code_ref1, ~table_code_ref2,

    "oa",     "lsoa",   1,  NULL,
    "oa",     "msoa",   1,  NULL,
    "oa",     "rgn",    1,  NULL,
    "lsoa",   "rgn",    1,  NULL,
    "oa",     "lad",    2,  NULL, # 1 would work: returns MSOAs; 2 retns Wards
    "oa",     "wd",     2,  NULL,
    "wd",     "lad",    3,  NULL,
    "wd",     "cty",    3,  NULL,
    "wd",     "rgn",    3,  NULL,
    "wd",     "ctry",   3,  NULL,
    "lad",    "cty",    3,  NULL,
    "lad",    "rgn",    3,  NULL,
    "cty",    "rgn",    3,  NULL,
    "lad",    "ctry",   3,  NULL,
    "cty",    "ctry",   3,  NULL,
    "rgn",    "ctry",   3,  NULL,
    "lad",    "cauth",  4,  NULL,
    "lsoa",   "utla",   5,  NULL,
    "lsoa",   "cty",    5,  NULL, # cty needs to be renamed to utla here
    "lad",    "utla",   6,  NULL, # lad needs to be renamed to ltla here
    "ltla",   "utla",   6,  NULL,
    "lsoa",   "wd",     7,  NULL,
    "lsoa",   "lad",    7,  NULL,
    "lsoa",   "ltla",   7,  NULL  # ltla needs to be renamed to lad here
    # "utla",   "rgn"     1,     3,
    # "utla",   "ctry"    1,     3,
    # "lsoa",   "cauth"   2,     4,
    # "lsoa",   "ctry"    1,     4
  ) %>%
    dplyr::mutate(bounds_level = dplyr::case_when(
      stringr::str_ends(bounds_level, "oa") ~ paste0(bounds_level, "11cd"),
      stringr::str_ends(bounds_level, "la") ~ paste0(bounds_level, "21cd"),
      TRUE ~ paste0(bounds_level, "20cd")
    )) %>%
    dplyr::mutate(within_level = dplyr::case_when(
      stringr::str_ends(within_level, "oa") ~ paste0(within_level, "11nm"),
      stringr::str_ends(within_level, "la") ~ paste0(within_level, "21nm"),
      TRUE ~ paste0(within_level, "20nm")
    ))


  if (bounds_level == "lsoa") {

    if (within_level == "cty") {
      within_level <- "utla"
    }
    if (within_level == "ltla") {
      within_level <- "lad"
    }

  }

  if (bounds_level == "lad" & within_level == "utla") {
    bounds_level <- "ltla"
  }




  # filter the area_code_lookup table above to get formal field codes
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


  # oa11nm doesn't exist of course, but you can cheekily get away with
  # requesting oa11cd twice and it seems not to mind, just returning a single
  # oa11cd column.
  # dplyr::select() doesn't mind if you pass a duplicated column name to it,
  # either... BRILLIANT!
  if (bounds_level == "oa") fields[2] <- "oa11cd" # cheeky

  if (bounds_level == "oa" & within_level %in% c("wd", "ward")) {
    return_style <- "tidy"
  }

  # TODO: write some tests for OA queries


  table_code_refs <- table_code_ref_lookup %>%
    dplyr::filter(bounds_level == fields[1]) %>%
    dplyr::filter(within_level == fields[4]) %>%
    dplyr::select(3:dplyr::last_col()) %>%
    unlist() %>%
    unname() %>%
    c(recursive = TRUE)


  end_col <- length(fields)
  return_fields <- "*" # default for return_style = "tidy"


  # I think I did this to avoid getting both Wards and MSOAs? Things get messy
  # if you do that because the lookups are all overlapped and you get more
  # than one row per LSOA, and end up downloading a load of duplicate
  # boundaries. That makes sense, but I can't remember exactly.
  if (return_style == "tidy" &
      bounds_level == "lsoa" &
      within_level == "lad"    # I don't think I need to cater for "ltla" here
  ) {
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
  treat_results <- function(df, return_style) {

    # check that the parameter is valid
    # TODO: I think I should do this with match.args() or sth
    if (!return_style %in% c("tidy", "simple", "minimal")) {
      usethis::ui_warn("'return_style' parameter not correctly specified.
                       Options are \"tidy\", \"simple\", \"minimal\".
                       Setting to \"tidy\"")
      return_style <- "tidy"
    }

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


  # use last field by default, but use penultimate if using codes not names
  nth_field <- -1
  if (within_cd) nth_field <- -2


  # An attempt to enable using more than one table_code -> 2-stage lookup.
  # But not there yet: would need to extend `fields` and manipulate it so that
  # the first query uses fields[1:4] and the second query uses [3:6] or whatever
  df_out <- table_code_refs %>%
    purrr::map( ~ build_api_query(
      table_code_ref = .,
      within_level = dplyr::nth(fields, nth_field),
      within = within,
      fields = return_fields
      ) %>%
      extract_lookup() %>%
      treat_results(return_style = return_style)
    ) %>%
    purrr::reduce(dplyr::left_join)


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
  if ("lad20nm" %in% colnames(df_out) && include_welsh_names) {
    lad20nmw_lookup <- jsonlite::fromJSON("https://opendata.arcgis.com/datasets/4094644cae32481f95fe7030334c8589_0.geojson") %>%
      purrr::pluck("features", "properties") %>%
      janitor::clean_names() %>%
      dplyr::select(-fid)

    df_out <- df_out %>%
      dplyr::left_join(lad20nmw_lookup) %>%
      dplyr::relocate(lad20nmw, .after = lad20nm)
  }


  if (!include_msoa) {
    df_out
  } else {
    df_out %>%
      lsoa_to_msoa_lookup(keep = keep_lsoa_cols, nmw = include_welsh_names)
  }
}
