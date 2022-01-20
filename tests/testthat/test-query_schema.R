library(testthat)
devtools::load_all(".")
test_codes <- opengeo_schema %>%
  dplyr::select(ends_with("cd")) %>%
  names()

"find codes 1a" %>%
  testthat::test_that({
  testthat::expect_identical(
    find_codes(test_codes, "lad", "19"),
    "lad19cd"
  )}
  )
"find codes 1b" %>%
  testthat::test_that({
    testthat::expect_identical(
      find_codes(test_codes, "lsoa", 2020),
      "lsoa11cd"
    )}
  )


"find codes 2a" %>%
  testthat::test_that({
    testthat::expect_identical(
      find_codes(test_codes, "wd"),
      "wd21cd"
    )}
  )
"find codes 2b" %>%
  testthat::test_that({
    testthat::expect_identical(
      find_codes(test_codes, "lsoa"),
      "lsoa11cd"
    )}
  )

"find codes 3a" %>%
  testthat::test_that({
    testthat::expect_identical(
      find_codes(codes = test_codes, prefix = "wd", year = 2022),
      "wd21cd"
    )}
  )
"find codes 3b" %>%
  testthat::test_that({
    testthat::expect_identical(
      find_codes(codes = test_codes, prefix = "msoa", year = 12),
      "msoa11cd"
    )}
  )


"find codes 4a" %>%
  testthat::test_that({
    poss_codes <- grep(paste0("(?<=^", "wd", ")\\d+"), test_codes, perl = TRUE, value = TRUE)
    testthat::expect_gte(
      length(poss_codes),
      1
    )}
  )
"find codes 4b" %>%
  testthat::test_that({
    poss_codes <- grep(paste0("(?<=^", "wd", ")\\d+"), test_codes, perl = TRUE, value = TRUE)
    testthat::expect_identical(
      poss_codes[1],
      "wd15cd"
    )}
  )

"find codes 5" %>%
  testthat::test_that({
    prefix <- "wd"
    poss_codes <- grep(paste0("(?<=^", prefix, ")\\d+"), test_codes, perl = TRUE, value = TRUE)
    poss_years <- substr(poss_codes, nchar(prefix) + 1, nchar(prefix) + 2)
    testthat::expect_gte(
      length(poss_years),
      1
    )}
  )

"find codes 6" %>%
  testthat::test_that({
    prefix <- "wd"
    cutoff <- 30
    poss_codes <- grep(paste0("(?<=^", prefix, ")\\d+"), test_codes, perl = TRUE, value = TRUE)
    poss_years <- substr(poss_codes, nchar(prefix) + 1, nchar(prefix) + 2)
    max_year <- poss_years %>%
      `[`(which(as.numeric(.) < cutoff)) %>%
      as.numeric() %>%
      max()
    testthat::expect_identical(
      max_year,
      21
    )}
  )

"find codes 7" %>%
  testthat::test_that({
    prefix <- "wd"
    cutoff <- 30
    poss_codes <- grep(paste0("(?<=^", prefix, ")\\d+"), test_codes, perl = TRUE, value = TRUE)
    poss_years <- substr(poss_codes, nchar(prefix) + 1, nchar(prefix) + 2)
    max_year <- poss_years %>%
      `[`(which(as.numeric(.) < cutoff)) %>%
      as.numeric() %>%
      max()
    testthat::expect_vector(
      grep(paste0("^", prefix, max_year), test_codes, value = TRUE)
    )}
  )


"find codes 8" %>%
  testthat::test_that({
    prefix <- "wd"
    cutoff <- 30
    poss_codes <- grep(paste0("(?<=^", prefix, ")\\d+"), test_codes, perl = TRUE, value = TRUE)
    poss_years <- substr(poss_codes, nchar(prefix) + 1, nchar(prefix) + 2)
    max_year <- poss_years %>%
      `[`(which(as.numeric(.) < cutoff)) %>%
      as.numeric() %>%
      max()
    testthat::expect_identical(
      grep(paste0("^", prefix, max_year), test_codes, value = TRUE)[1],
      "wd21cd"
    )}
  )

"find codes 9" %>%
  testthat::test_that({
    year <- 2022
    testthat::expect_identical(
      sub(".*(?=[0-9]{2}$)", "", year, perl = TRUE),
      "22"
    )}
  )

"find codes 10a" %>%
  testthat::test_that({
    prefix <- "wd"
    cutoff <- 30
    poss_codes <- grep(paste0("(?<=^", prefix, ")\\d+"), test_codes, perl = TRUE, value = TRUE)
    poss_years <- substr(poss_codes, nchar(prefix) + 1, nchar(prefix) + 2)
    year <- 2022
    testthat::expect_false(
      sub(".*(?=[0-9]{2}$)", "", year, perl = TRUE) %in% poss_years
    )}
  )
"find codes 10b" %>%
  testthat::test_that({
    prefix <- "wd"
    cutoff <- 30
    poss_codes <- grep(paste0("(?<=^", prefix, ")\\d+"), test_codes, perl = TRUE, value = TRUE)
    poss_years <- substr(poss_codes, nchar(prefix) + 1, nchar(prefix) + 2)
    year <- 2021
    testthat::expect_true(
      sub(".*(?=[0-9]{2}$)", "", year, perl = TRUE) %in% poss_years
    )}
  )

"test prefixes 1a" %>%
  testthat::test_that({
    testthat::expect_identical(
    test_prefixes(test_codes, "lad"),
    "lad"
    )
  })
"test prefixes 1b" %>%
  testthat::test_that({
    testthat::expect_identical(
      test_prefixes(test_codes, "wd"),
      "wd"
    )
  })


schema1 <- opengeo_schema %>%
  dplyr::filter(stringr::str_detect(service_name, "_LU$")) %>%
  janitor::remove_empty("cols")
codes1 <- schema1 %>%
  dplyr::select(ends_with("cd")) %>%
  names()

"find codes 20" %>%
  testthat::test_that({
    testthat::expect_identical(
      find_codes(codes1, "lad", NULL),
      "lad21cd"
    )
  })

schema2 <- schema1 %>%
  dplyr::filter(!across("lad21cd", is.na)) %>%
  janitor::remove_empty("cols")
codes2 <- schema2 %>%
  dplyr::select(ends_with("cd")) %>%
  names()
"find codes 21" %>%
  testthat::test_that({
    testthat::expect_length(
      codes2,
      9
    )
  })
"find codes 22" %>%
  testthat::test_that({
    testthat::expect_error(
      find_codes(codes2, "wd", NULL)
    )
  })



"lookup query 1" %>%
  testthat::test_that({
    testthat::expect_error(
      pull_lookup_query_url("lad", "wd")
    )
  })
"lookup query 2" %>%
  testthat::test_that({
    testthat::expect_identical(
      pull_lookup_query_url("wd", "lad"),
      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/WD20_REGD20_LAD20_EW_LU/FeatureServer"
    )
  })
"lookup query 3" %>%
  testthat::test_that({
    testthat::expect_identical(
      pull_lookup_query_url("lad", "wd", 2020),
      "https://services1.arcgis.com/ESMARspQHYMw9BZ9/ArcGIS/rest/services/WD20_REGD20_LAD20_EW_LU/FeatureServer"
    )
  })
