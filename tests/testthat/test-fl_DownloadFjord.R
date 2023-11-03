# tests for download function

test_that("fl_DownloadFjord error messages signal correctly", {
  expect_error(fl_DownloadFjord(fjord = "banana"), "banana not available")
})

test_that("fl_DownloadFjord temp directory works and doesn't re-download", {
  test_dl <- fl_DownloadFjord(fjord = "kong")
  test_dl <- fl_DownloadFjord(fjord = "kong")
  expect_type(test_dl, "NULL")
})
unlink("~/FjordLight/tests/testthat/FjordLight.d", recursive = TRUE)

test_that("fl_DownloadFjord custom directory works and doesn't re-download", {
  test_dl <- fl_DownloadFjord(fjord = "kong", dirdata = "PAR_test")
  test_dl <- fl_DownloadFjord(fjord = "kong", dirdata = "PAR_test")
  expect_type(test_dl, "NULL")
})
unlink("~/FjordLight/tests/testthat/PAR_test", recursive = TRUE)
