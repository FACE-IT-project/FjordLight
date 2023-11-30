# Tests for fl_DownloadFjord

test_that("fl_DownloadFjord error messages signal correctly", {
  skip_if_offline()
  expect_error(fl_DownloadFjord(fjord = "banana"), "banana not available")
  expect_error(fl_DownloadFjord(fjord = "test"),
               "Please provide the pathway to where you would like to download the data.")
  expect_error(fl_DownloadFjord(fjord = "test", dirdata = "mango"),
               "Please ensure that the chosen directory exists.")
})

test_that("fl_DownloadFjord gets the 'test.nc' file only once", {
  skip_if_offline()
  test_dl <- fl_DownloadFjord(fjord = "test", tempdir())
  test_dl <- fl_DownloadFjord(fjord = "test", tempdir())
  expect_type(test_dl, "NULL")
  fjord_test <- fl_LoadFjord("test", TS = FALSE, tempdir())
  expect_length(fjord_test$glob_attributes$available_months_by_year, 1)
})
