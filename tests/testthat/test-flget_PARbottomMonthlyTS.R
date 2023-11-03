# tests for Monthly TS function

test_that("flget_PARbottomMonthlyTS error messages signal correctly", {
  dat_no_TS <- flget_PARbottomMonthlyTS()
  expect_error(fl_DownloadFjord(fjord = "banana"), "banana not available")
})
