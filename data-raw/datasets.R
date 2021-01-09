
# List of all LSOAs in England and Wales: codes and names

lsoa11cdnm <- paste0(
  "https://opendata.arcgis.com/",
  "datasets/",
  "3ce71e53d9254a73b3e887a506b82f63_0.csv"
) %>%
  readr::read_csv()


# backup local copy of House of Commons Library MSOA names

# hocl_msoa_names <- paste0(
#   "https://visual.parliament.uk/msoanames/static/",
#   "MSOA-Names-",
#   "Latest",
#   ".csv"
# ) %>%
#   readr::read_csv()


usethis::use_data(lsoa11cdnm, internal = TRUE, overwrite = TRUE)

# TODO: document datasets

# usethis::use_data(lsoa11cdnm, internal = FALSE)
# usethis::use_data(hocl_msoa_names, internal = FALSE)
