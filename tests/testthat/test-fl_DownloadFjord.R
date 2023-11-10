# Tests for fl_DownloadFjord

test_that("fl_DownloadFjord error messages signal correctly", {
  expect_error(fl_DownloadFjord(fjord = "banana"), "banana not available")
  expect_error(fl_DownloadFjord(fjord = "test"),
               "Please provide the pathway to where you would like to download the data.")
  expect_error(fl_DownloadFjord(fjord = "test", dirdata = "mango"),
               "Please ensure that the chosen directory exists.")
})

test_that("fl_DownloadFjord gets the 'test.nc' file only once", {
  test_dl <- fl_DownloadFjord(fjord = "test", tempdir())
  test_dl <- fl_DownloadFjord(fjord = "test", tempdir())
  expect_type(test_dl, "NULL")
})

test_that("fl_DownloadFjord gets the most current real data files", {
  test_kong <- fl_DownloadFjord(fjord = "kong", tempdir())
  fjord_kong <- fl_LoadFjord("kong", TS = FALSE, tempdir())
  expect_type(test_kong, "NULL")
  expect_length(fjord_kong$glob_attributes$available_months_by_year, 1)
})
