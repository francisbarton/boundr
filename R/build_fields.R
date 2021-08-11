#' filter a lookup table to get formal field codes from informal inputs
#'
#' @param x a string or vector of strings
#' @keywords internal
build_fields <- function(x) {

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

  if (all(tolower(x) %in% area_code_lookup$friendly)) {
    area_code_lookup %>%
      dplyr::filter(friendly %in% tolower(x)) %>%
      dplyr::pull(serious) %>%
      rep(each = 2) %>%
      paste0(c("cd", "nm")) %>%
      unique()
  } else if (any(tolower(x) %in% area_code_lookup$friendly)) {
    usethis::ui_stop(
      "If you specify 1 of bounds_level and within_level in full, you need also to specify the other (ie both)."
    )
  } else {
    x %>%
      stringr::str_remove_all("(cd|nm)$") %>%
      rep(each = 2) %>%
      paste0(c("cd", "nm")) %>%
      unique()
  }
}
