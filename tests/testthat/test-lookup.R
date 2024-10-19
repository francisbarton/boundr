test_that("overall lookup() test", {
  expect_no_error(lookup("spr"))
  expect_no_error(lookup("npark"))
  expect_no_error(lookup("spc", "spr"))
  # expect_no_error(lookup("oa", "pcds")) # test case - was broken, now fixed
  expect_no_error(lookup("npark", within_names = "Exmoor"))
  expect_no_error(
    lookup("msoa", "lad", "Stroud", opts = opts(return_width = "full"))
  )
  expect_no_error(lookup("lad", "ctry", "Wales", lookup_year = 2022))
  expect_no_error(lookup("ltla", "iol", "inner london"))
  expect_no_error(lookup("itl2", "itl1", within_codes = "TLH"))
  expect_no_error(lookup("icb", "nhser", "South East")) # check nrow() accurate
  expect_no_error(lookup("sener", within_names = "North Wales"))
  expect_error(lookup("sener", within_names = "Gogledd Cymru")) # to_fix
})



