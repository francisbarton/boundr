#' Get boundaries for small areas within a local authority area
#'
#' The ONS OpenGeography Portal (\url{https://geoportal.statistics.gov.uk/})
#' is a great resource for area boundary and data lookups within the UK.
#' This program focuses on areas within England and Wales only, initially.
#'
#' \emph{I want a better name for this function! Suggestions welcome...}
#'
#' @inheritParams create_custom_lookup
#' @param boundaries whether to retrieve object boundaries data from the API.
#'   Default \code{TRUE}. If \code{FALSE}, just returns a plain summary df with
#'   area codes and names etc but no geometry.
#'
#' @return an sf (simple features) object (data frame with geometries)
#' @export
#'
#' @examples
#' geo_get("wd", "Swindon", "lad")
#' geo_get("msoa", "Swansea", "lad")
#' geo_get("lsoa", "Zetland", "ward")
#' geo_get("lad", "Gloucestershire", "cty", return_style = "simple")
#' geo_get("lad", "Greater Manchester", "cauth")
#' geo_get("cty", "East of England", "rgn", boundaries = FALSE)
geo_get <- function(
                    bounds_level,
                    within,
                    within_level,
                    include_msoa = NULL,
                    return_style = "tidy",
                    include_welsh_names = NULL,
                    boundaries = TRUE) {


  # get the basic lookup table
  # create_custom_lookup() just inherits its params here from geo_get()
  basic_df <- create_custom_lookup(
    bounds_level = bounds_level,
    within = within,
    within_level = within_level,
    include_msoa = include_msoa,
    return_style = return_style,
    include_welsh_names = include_welsh_names
  )


  # if the user sets 'boundaries' to FALSE then they just get a summary table
  if (!boundaries) {
    return(basic_df)
  }


  bounds_codes <- basic_df %>%
    dplyr::select(dplyr::ends_with("cd")) %>%
    dplyr::pull(1) %>%
    batch_it_simple() # borrowed from my myrmidon utils package


  bounds_query_level <- basic_df %>%
    dplyr::select(dplyr::ends_with("cd")) %>%
    dplyr::select(1) %>%
    colnames()


  metro_counties <- c(
    "Greater Manchester",
    "Merseyside",
    "South Yorkshire",
    "West Midlands",
    "West Yorkshire",
    "Tyne and Wear"
  )

  if (bounds_query_level %in% c("cty19cd", "utla19cd")) {
    if (within %in% metro_counties) {
      bounds_query_level <- "mcty18cd"
    } else if (within %in% c("Inner London", "Outer London")) {
      usethis::ui_stop("Sorry but boundaries are not available for Inner London and Outer London")
    } else {
      bounds_query_level <- "ctyua19cd"
    }
  }
  if (bounds_query_level == "ltla19cd") bounds_query_level <- "lad19cd"

  return_fields <- c(
    bounds_query_level,
    stringr::str_replace(bounds_query_level, "cd$", "nm")
  )


  table_code_ref_lookup <- dplyr::tribble(
    ~bounds_level, ~table_code_ref, ~type, ~server,

    "lsoa11cd",     5,    "census",   "feature",
    "msoa11cd",     6,    "census",   "feature",
    "wd19cd",       7,    "census",   "feature",
    "lad19cd",      8,    "admin",    "map",
    "ctyua19cd",    9,    "admin",    "map",
    "mcty18cd",    10,    "other",    "map"

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
    purrr::map(~ build_api_query(
      table_code_ref = table_code_refs[["table_code_ref"]],
      type = table_code_refs[["type"]],
      server = table_code_refs[["server"]],
      within_level = bounds_query_level,
      within = .,
      fields = return_fields,
      distinct = FALSE
    ))

  bounds_out <- bounds_queries %>%
    purrr::map_df(sf::st_read) %>%
    janitor::clean_names()


  dplyr::right_join(bounds_out, basic_df)
}
