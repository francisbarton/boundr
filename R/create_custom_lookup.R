#' Uses build_api_query() to Retrieve, Tidy and Return a Lookup Table
#'
#' @param bounds_level the lowest level at which to return codes and names, eg "LSOA". Has to be one of "lsoa", "msoa", "wd/ward", "lad", "ltla/lower", "utla/upper", "cty/county", "cauth", "rgn/region", "ctry/country". Case-insensitive.
#' @param within the name of a geographic area to filter by eg "Swindon", "Gloucestershire", "Wales"
#' @param within_level upper geographic level to filter at. eg if filtering to find all LSOAs in a local authority, within_level will be "lad" or "ltla" or "utla". Has to be one of "lad", "cty/county", "utla/upper", "rgn/region", "cauth" or "ctry/country". Case-insensitive.
#' @param include_msoa if bounds_level is LSOA, whether to also include MSOA columns (in "tidy" or "all" styles). If bounds_level is MSOA, this will be forced to TRUE.
#' @param return_style "tidy" (the default) means all available columns between bounds_level and within_level will be returned, but with any empty columns removed. "all" is as "tidy" except empty columns are retained. "simple" means that only the code and name (cd and nm) columns for bounds_level and within_level are returned - other columns are omitted. "minimal" means only return the columns for bounds_level.
#' @param include_welsh_names only makes a difference when bounds_level = msoa, or when bounds_level = lsoa and return_style = "all" or "tidy". FALSE returns no Welsh language columns. TRUE attempts to return Welsh language columns for MSOA names. NULL (default) means that a decision will be made by the program, based on whether lsoa11cd or msoa11cd columns contain "^W"
#'
#' @return a data frame (tibble)
#' @export
#'
#' @examples
#' create_custom_lookup(bounds_level = "msoa", within = "Swindon", within_level = "lad", return_style = "simple")
#' create_custom_lookup(bounds_level = "msoa", within = "Swansea", within_level = "lad", return_style = "tidy")
#' # TODO add in more examples
create_custom_lookup <- function(
                                 bounds_level,
                                 within,
                                 within_level,
                                 include_msoa = NULL,
                                 return_style = "tidy",
                                 include_welsh_names = NULL) {
  keep_lsoa_cols <- TRUE

  # automatically include MSOAs where it makes sense to, unless overridden by params
  if (is.null(include_msoa) && tolower(bounds_level) == "lsoa" && !tolower(within_level) %in% c("wd", "ward") && !return_style %in% c("simple", "minimal")) {
    include_msoa <- TRUE
  } else if (is.null(include_msoa)) {
    include_msoa <- FALSE
  }

  # throw an error if 'include_msoa' is set where it doesn't make any sense to
  if (include_msoa && !tolower(bounds_level) %in% c("lsoa", "msoa")) {
    usethis::ui_info(
      "'include_msoa' is set to TRUE but you are not retrieving data
      at a 'bounds_level' of LSOA or MSOA, so this will not work.
      Setting 'include_msoa' to FALSE."
    )
    include_msoa <- FALSE
  }

  if (tolower(bounds_level) == "msoa") {
    bounds_level <- "lsoa"
    keep_lsoa_cols <- FALSE
    include_msoa <- TRUE
  }


  area_code_lookup <- dplyr::tribble(
    ~friendly, ~serious,
    "lsoa", "lsoa11",
    "msoa", "msoa11",
    "wd", "wd19",
    "ward", "wd19",
    "lad", "lad19",
    "ltla", "ltla19",
    "lower", "ltla19",
    "utla", "utla19",
    "upper", "utla19",
    "cty", "cty19",
    "county", "cty19",
    "cauth", "cauth19",
    "rgn", "rgn19",
    "region", "rgn19",
    "ctry", "ctry19",
    "country", "ctry19"
  )

  table_code_ref_lookup <- dplyr::tribble(
    ~bounds_level, ~within_level, ~table_code_ref1, ~table_code_ref2,

    "wd", "lad", 1, NULL,
    "wd", "cty", 1, NULL,
    "wd", "rgn", 1, NULL,
    "wd", "ctry", 1, NULL,
    "lad", "cty", 1, NULL,
    "lad", "rgn", 1, NULL,
    "lad", "ctry", 1, NULL,
    "cty", "rgn", 1, NULL,
    "lad", "cauth", 2, NULL,
    "ltla", "utla", 3, NULL,
    "lsoa", "utla", 4, NULL,
    "msoa", "utla", 4, NULL,
    "lsoa", "wd", 5, NULL,
    "lsoa", "lad", 5, NULL,
    "msoa", "lad", 5, NULL
    # "utla",   "rgn"     1,    3,
    # "lsoa",   "cauth"   2,    5,
    # "msoa",   "cauth"   2,    5,
    # "lsoa",   "rgn"     1,    5,
    # "msoa",   "rgn"     1,    5,
    # "lsoa",   "ctry"    1,    5
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


  fields <- c(bounds_level, within_level) %>%
    get_serious() %>%
    rep(each = 2) %>%
    paste0(c("cd", "nm"))

  # TODO check if lad19nmw can reliably be added as a column when include_welsh_names = TRUE
  # https://geoportal.statistics.gov.uk/datasets/local-authority-districts-december-2019-names-and-codes-in-the-united-kingdom/
  # https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/LAD_DEC_2019_UK_NC/FeatureServer/0/query?outFields=*&where=1%3D1
  # https://opendata.arcgis.com/datasets/35de30c6778b463a8305939216656132_0.geojson (full dataset)
  # has the lad19nm to lad19nmw lookup


  table_code_refs <- table_code_ref_lookup %>%
    dplyr::filter(bounds_level == fields[1]) %>%
    dplyr::filter(within_level == fields[4]) %>%
    dplyr::select(3:4) %>%
    unlist() %>%
    unname() %>%
    c(recursive = TRUE)


  # doesn't work reliably, unfortunately
  # if (include_welsh_names && fields[4] == "lad19nm" ) fields <- c(fields, "lad19nmw")
  # if (include_welsh_names && fields[4] == "lad19nm" ) fields <- c(fields[1:2], "lad19nmw", fields[3:4])

  end_col <- length(fields)
  return_fields <- "*"


  if (return_style == "simple") {
    return_fields <- fields
  }

  if (return_style == "minimal") {
    return_fields <- fields[1:2]
  }

  treat_results <- function(df, return_style) {
    if (!return_style %in% c("tidy", "all", "simple", "minimal")) {
      usethis::ui_warn("'return_style' parameter not correctly specified.
                       Options are \"tidy\", \"all\", \"simple\", \"minimal\".
                       Setting to \"tidy\"")
      return_style <- "tidy"
    }

    if (return_style == "all") {
      df <- df %>%
        dplyr::select(!!rlang::sym(fields[1]):!!rlang::sym(fields[end_col])) %>%
        dplyr::distinct()
    }

    if (return_style == "tidy") {
      df <- df %>%
        dplyr::select(!!rlang::sym(fields[1]):!!rlang::sym(fields[end_col])) %>%
        dplyr::distinct() %>%
        janitor::remove_empty("cols")
    }

    if (return_style %in% c("simple", "minimal")) {
      df <- df %>%
        dplyr::distinct()
    }

    return(df)
  }

  # create another lookup (if necessary) for automatically looking
  # up the type and server parameters required for each table_code;
  # currently assuming that type="census" and server="feature" work for all!?
  df_out <- build_api_query(
    table_code_ref = table_code_refs[1],
    search_within = fields[4],
    locations = within,
    fields = return_fields
  ) %>%
    extract_lookup() %>%
    treat_results(return_style = return_style)


  if (!include_msoa) {
    return(df_out)
  }


  if (is.null(include_welsh_names)) {
    check_w <- df_out %>%
      dplyr::select(ends_with("cd")) %>%
      dplyr::pull(1) %>%
      stringr::str_match("^[A-Z]{1}") %>%
      unique()

    if ("W" %in% check_w) {
      include_welsh_names <- TRUE
    } else {
      include_welsh_names <- FALSE
    }
  }

  df_out %>%
    lsoa_to_msoa_lookup(keep = keep_lsoa_cols, nmw = include_welsh_names)
}
