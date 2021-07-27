#' filter a lookup table to get formal field codes from informal inputs
#'
#' @param x an input string or vector of strings
#' @keywords internal
get_serious <- function(x) {

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
    "ctyua",   "ctyua20",
    "mcty",    "mcty18",
    "cauth",   "cauth20",
    "rgn",     "rgn20",
    "region",  "rgn20",
    "ctry",    "ctry20",
    "country", "ctry20"
  )

  area_code_lookup %>%
    dplyr::filter(friendly %in% tolower(x)) %>%
    dplyr::pull(serious)
}
