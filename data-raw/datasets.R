hocl_msoa_names <- paste0(
  "https://houseofcommonslibrary.github.io/",
  "msoanames/MSOA-Names-Latest.csv") |>
  readr::read_csv(col_types = "cccccc") |>
  dplyr::select(!Laname)


opengeo_schema <- build_schema()

usethis::use_data(hocl_msoa_names, opengeo_schema, overwrite = TRUE, internal = TRUE, compress = "bzip2")
