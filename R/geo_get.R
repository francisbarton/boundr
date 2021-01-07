#' Get boundaries for small areas within a local authority area
#'
#' The ONS OpenGeography Portal (\url{https://geoportal.statistics.gov.uk/})
#' is a great resource for area boundary and data lookups within the UK.
#' This program focuses on areas within England and Wales only, initially.
#'
#' \emph{I want a better name for this function! Suggestions welcome...}
#'
#' @inheritParams create_custom_lookup
#' @param spatial_ref The (EPSG) spatial reference of any returned geometry.
#'   Default value: 4326 ("WGS 84"). This parameter is ignored peacefully if
#'   no geometry is returned/returnable, eg lookup queries
#' @param return_boundaries whether to retrieve object boundaries data from
#'   the API. Default \code{TRUE}. If \code{return_boundaries} and
#'   \code{return_centroids} are both \code{FALSE}, a plain summary df
#'   without geometry will be returned.
#' @param return_centroids whether to retrieve area centroids instead of
#'   boundaries. Default \code{FALSE}. If set to TRUE then it will override
#'   \code{return_boundaries} whether that was set TRUE or otherwise. If
#'   \code{return_boundaries} and \code{return_centroids} are both \code{FALSE},
#'   a plain summary data frame without geometry will be returned.
#'
#' @return a data frame or an sf (simple features) object (data frame
#'   with geometries)
#' @export
#'
#' @examples
#' geo_get("wd", "Swindon", "lad")
#' geo_get("msoa", "Swansea", "lad", return_centroids = TRUE)
#' geo_get("lsoa", "Zetland", "ward", spatial_ref = 3857)
#' geo_get(bounds_level = "lad", within = "Gloucestershire",
#'   within_level = "cty", return_style = "simple", return_boundaries = FALSE)
#' geo_get("county", "East of England", "region", return_boundaries = FALSE)
geo_get <- function(
                    bounds_level,
                    within,
                    within_level,
                    include_msoa = NULL,
                    return_style = "tidy",
                    include_welsh_names = NULL,
                    spatial_ref = 4326,
                    return_boundaries = TRUE,
                    return_centroids = FALSE) {


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


  # if the user sets 'return_boundaries' FALSE then just return a summary table
  if (!return_boundaries && !return_centroids) {
    return(basic_df)
  }


  bounds_codes <- basic_df %>%
    dplyr::select(dplyr::ends_with("cd")) %>%
    dplyr::pull(1) %>%
    # 25 seems to be ok. Long queries (eg batch size 50) often return 404.
    batch_it_simple(batch_size = 25) # borrowed from my myrmidon utils package


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

  # don't need this actually - the 'cd' field is enough for the join to work
  # return_fields <- c(
  #   bounds_query_level,
  #   stringr::str_replace(bounds_query_level, "cd$", "nm")
  # )


  table_code_ref_lookup <- dplyr::tribble(
    ~bounds_level, ~table_code_ref, ~type, ~server, ~centroids,

    "lsoa11cd",     5,    "census",   "feature",    FALSE,
    "msoa11cd",     6,    "census",   "feature",    FALSE,
    "wd19cd",       7,    "census",   "feature",    FALSE,
    "lad19cd",      8,    "admin",    "map",        FALSE,
    "ctyua19cd",    9,    "admin",    "map",        FALSE,
    "mcty18cd",    10,    "other",    "map",        FALSE,
    "msoa11cd",    11,    "centroid", "map",        TRUE

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
    dplyr::filter(bounds_level == bounds_query_level) %>%
    # centroids is used here to filter, this is why the setting of
    # return_centroids as TRUE will override the setting of boundaries to TRUE
    dplyr::filter(centroids == return_centroids)

  bounds_queries <- bounds_codes %>%
    purrr::map(~ build_api_query(
      table_code_ref = table_code_refs[["table_code_ref"]],
      type = table_code_refs[["type"]],
      server = table_code_refs[["server"]],
      within_level = bounds_query_level,
      within = .,
      fields = bounds_query_level,
      sr = spatial_ref,
      distinct = FALSE
    ))

  bounds_out <- bounds_queries %>%
    purrr::map_df(sf::st_read) %>%
    janitor::clean_names()


  dplyr::right_join(bounds_out, basic_df)
}
