test_that("arrange_service_nms_by_res works", {
  # c(
  #  "BGC", "BSC", "BUC", "BFC", "BGE", "BFE", "BUE",
  #  "GCB", "SGCB", "UGB", "UGCB", "FCB", "FEB", "BGG"
  #)
  r <- res_codes()
  expect_equal(length(r), 14)

  t1 <- tibble::tribble(
    ~id, ~service_name,
    1, "test_FEB",  # item 13
    2, "test_SGCB_v2", # item 9
    3, "test_BGC",  # item 1
    4, "test_NAC", # not found in codes (should return NA?)
    5, "test_BGC_v2" # should get same match as row 3 but stay sorted below
  )
  t2 <- purrr::map_lgl(res_codes(), \(x) grepl(x, t1[["service_name"]][[1]]))
  expect_equal(which(t2), 13)
  t3 <- t1[["service_name"]] |>
    purrr::map(\(x) purrr::map_lgl(res_codes(), \(cd) grepl(cd, x)))
  expect_length(t3, nrow(t1))
  t4 <- t1[["service_name"]] |>
    purrr::map_int(
      \(x) {
        min(if (length(
          v <- which(purrr::map_lgl(res_codes(), \(cd) grepl(cd, x)))
        ) == 0) nrow(t1) else v)
      }
    )
  expect_length(t4, nrow(t1))
  expect_equal(t4[[1]], 13)
  expect_equal(t4[[3]], 1)
  expect_equal(t4[[4]], nrow(t1))
  safe_min <- \(x) suppressWarnings(min(x)) # It's OK if we get an Inf!
  score <- \(nm) safe_min(which(purrr::map_lgl(r, \(x) grepl(x, nm))))
  t5 <- purrr::map_dbl(t1[["service_name"]], score)
  expect_equal(t5, c(13, 8, 1, Inf, 1))
  t6 <- dplyr::arrange(t1, t5)
  expect_equal(t6[["id"]], c(3, 5, 2, 1, 4))
  t7 <- arrange_service_nms_by_res(t1)
  expect_equal(t7[["id"]], c(3, 5, 2, 1, 4))
})
