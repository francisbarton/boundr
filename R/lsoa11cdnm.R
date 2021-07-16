#' A list of all LSOAs in England and Wales: codes and names
#'
#' @format A data frame with 34753 rows and 2 variables:
#' \describe{
#'   \item{lsoa11cd}{LSOA code}
#'   \item{lsoa11nm}{LSOA name}
#' }
"lsoa11cdnm"

# lsoa11cdnm <- paste0(
#   "https://opendata.arcgis.com/",
#   "datasets/",
#   "3ce71e53d9254a73b3e887a506b82f63_0.csv"
# ) %>%
#   readr::read_csv() %>%
#   janitor::clean_names() %>%
#   dplyr::select(1:2)
