# tests for loading function

test_that("loading without TS works", {
  fl_DownloadFjord(fjord = "kong")
  test_dat <- fl_LoadFjord("kong", "~/FjordLight/tests/testthat/test_file/")
  expect_type(test_dat, "list")
  expect_equal(length(test_dat$longitude), 321)
  expect_equal(round(test_dat$AreaOfShallowZone), 106)
})
unlink("~/FjordLight/tests/testthat/FjordLight.d", recursive = TRUE)

test_that("TS loading works", {
  fl_DownloadFjord(fjord = "kong")
  test_dat <- fl_LoadFjord("kong", "~/FjordLight/tests/testthat/test_file/", TS = TRUE)
  expect_type(test_dat, "list")
  expect_type(test_dat$MonthlyPARbottom, "double")
})
unlink("~/FjordLight/tests/testthat/FjordLight.d", recursive = TRUE)
