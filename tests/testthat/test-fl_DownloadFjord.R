# Tests for fl_DownloadFjord

test_that("fl_DownloadFjord error messages signal correctly", {
  skip_if_offline()
  expect_error(fl_DownloadFjord(fjord = "banana"), "banana not available")
  expect_error(fl_DownloadFjord(fjord = "test"),
               "Please provide the pathway to where you would like to download the data.")
  expect_error(fl_DownloadFjord(fjord = "test", dirdata = "mango"),
               "Please ensure that the chosen directory exists.")
  expect_error(fl_DownloadFjord(fjord = "kong", monthly = "kiwi", dirdata = tempdir()),
               "Please ensure the 'monthly' value is either 'PAR_B' or 'K_PAR'")
})

test_that("fl_DownloadFjord gets the 'kong.nc' file only once", {
  skip_if_offline()
  test_dl <- fl_DownloadFjord(fjord = "kong", dirdata = tempdir())
  test_dl <- fl_DownloadFjord(fjord = "kong", dirdata = tempdir())
  expect_type(test_dl, "NULL")
  fjord_test <- fl_LoadFjord("kong", TS = FALSE, dirdata = tempdir())
  expect_length(fjord_test$glob_attributes$available_months_by_year, 1)
})

test_that("fl_DownloadFjord gets K_PAR data files", {
  skip_if_offline()
  expect_error(fl_DownloadFjord(fjord = "kong", monthly = "banana", tempdir()),
               "Please ensure the 'monthly' value is either 'PAR_B' or 'K_PAR")
  test_dl <- fl_DownloadFjord(fjord = "kong", monthly = "K_PAR", tempdir())
  expect_type(test_dl, "NULL")
  fjord_test <- fl_LoadFjord("kong", "K_PAR", TS = FALSE, tempdir())
  expect_length(fjord_test$glob_attributes, 0)
})
