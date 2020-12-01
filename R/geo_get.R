#' Get boundaries for small areas within a local authority area
#'
#' The ONS OpenGeography Portal (https://geoportal.statistics.gov.uk/)
#' is a great resource for area boundary and data lookups within the UK.
#' This program focuses on areas within England and Wales only, initially.
#'
#' @param bounds_level the lowest level at which to return codes and names, eg "LSOA". Has to be one of "lsoa", "msoa", "wd/ward", "lad", "ltla/lower", "utla/upper", "cty/county", "cauth", "rgn/region", "ctry/country". Case-insensitive.
#' @param within the name of a geographic area to filter by eg "Swindon", "Gloucestershire", "Wales"
#' @param within_level upper geographic level to filter at. eg if filtering to find all LSOAs in a local authority, within_level will be "lad" or "ltla" or "utla". Has to be one of "lad", "cty/county", "utla/upper", "rgn/region", "cauth" or "ctry/country". Case-insensitive.
#' @param include_msoa if bounds_level is LSOA, whether to also include MSOA columns (in "tidy" or "all" styles). If bounds_level is MSOA, this will be forced to TRUE.
#' @param return_style "tidy" (the default) means all available columns between bounds_level and within_level will be returned, but with any empty columns removed. "all" is as "tidy" except empty columns are retained. "simple" means that only the code and name (cd and nm) columns for bounds_level and within_level are returned - other columns are omitted. "minimal" means only return the columns for bounds_level.
#' @param include_welsh_names only makes a difference when bounds_level = msoa, or when bounds_level = lsoa and return_style = "all" or "tidy". FALSE returns no Welsh language columns. TRUE attempts to return Welsh language columns for MSOA names. NULL (default) means that a decision will be made by the program, based on whether lsoa11cd or msoa11cd columns contain "^W"
#' @param boundaries whether to retrieve object boundaries data from the API. Default TRUE. If FALSE, just returns a df with area codes and names etc.
#'
#' @return an sf (simple features) object (data frame with geometries)
#' @export
#'
geo_get <- function(
  bounds_level,
  within,
  within_level,
  include_msoa = NULL,
  return_style = "tidy",
  include_welsh_names = NULL,
  boundaries = TRUE) {


  # get the basic lookup table
  basic_df <- create_custom_lookup(
    bounds_level = bounds_level,
    within = within,
    within_level = within_level,
    include_msoa = include_msoa,
    return_style = return_style,
    include_welsh_names = include_welsh_names
  )


  if (!boundaries) return(basic_df)


  bounds_codes <- basic_df %>%
    dplyr::select(dplyr::ends_with("cd")) %>%
    dplyr::pull(1) %>%
    batch_it_simple() # borrowed from my myrmidon utils package


  bounds_query_level <- basic_df %>%
    dplyr::select(dplyr::ends_with("cd")) %>%
    dplyr::select(1) %>%
    colnames()

  if (bounds_query_level %in% c("cty19cd", "utla19cd")) bounds_query_level <- "ctyua19cd"
  if (bounds_query_level %in% c("ltla19cd")) bounds_query_level <- "lad19cd"

  return_fields <- c(bounds_query_level,
                     stringr::str_replace(bounds_query_level, "cd$", "nm"))


  table_code_ref_lookup <- dplyr::tribble(
    ~bounds_level, ~table_code_ref, ~type, ~server,

    "lsoa11cd",     6,    "census",    "feature",
    "msoa11cd",     7,    "census",    "feature",
    "wd19cd",       8,    "census",    "feature",
    "lad19cd",      9,    "admin",     "map",
    "ctyua19cd",   10,    "admin",     "map"

    # "ccg",
    # "https://ons-inspire.esriuk.com/arcgis/rest/services/Health_Boundaries/Clinical_Commissioning_Groups_April_2020_EN_BFC_V2/MapServer/1/query?where=1%3D1&outFields=*&outSR=4326&f=json",

    # Regions (December 2019) Boundaries EN BGC
    # https://geoportal.statistics.gov.uk/datasets/regions-december-2019-boundaries-en-bgc
    # "rgn19cd",
    # "Regions_December_2019_Boundaries_EN_BGC",
    # "admin",
    # "map"

    # Countries (December 2019) Boundaries UK BGC
    # https://geoportal.statistics.gov.uk/datasets/countries-december-2019-boundaries-uk-bgc
    # "ctry",
    # "Countries_December_2019_Boundaries_UK_BGC",
    # "admin",
    # "map"
  )


  table_code_refs <- table_code_ref_lookup %>%
    dplyr::filter(bounds_level == bounds_query_level)

  bounds_queries <- bounds_codes %>%
    purrr::map( ~ build_api_query(
      table_code_ref = table_code_refs[["table_code_ref"]],
      type = table_code_refs[["type"]],
      server = table_code_refs[["server"]],
      search_within = bounds_query_level,
      locations = .,
      fields = return_fields
    ))

  bounds_out <- bounds_queries %>%
    purrr::map_df(sf::st_read) %>%
    janitor::clean_names()


  dplyr::right_join(bounds_out, basic_df)

}

# test_out <- get_ward_bounds("Ealing")
# tmap::tm_shape(test_out) + tm_borders()
