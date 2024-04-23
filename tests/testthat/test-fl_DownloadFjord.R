# Tests for fl_DownloadFjord

test_that("fl_DownloadFjord error messages signal correctly", {
  skip_if_offline()
  expect_error(fl_DownloadFjord(fjord = "banana"), "banana not available")
  expect_error(fl_DownloadFjord(fjord = "kong"),
               "Please provide the pathway to where you would like to download the data.")
  expect_error(fl_DownloadFjord(fjord = "kong", dirdata = "mango"),
               "Please ensure that the chosen directory exists.")
  expect_error(fl_DownloadFjord(fjord = "kong", layer = "kiwi", dirdata = tempdir()),
               "Please ensure the 'layer' value is either 'PAR_B', 'K_PAR', 'ClimSD', or 'YearlySD'")
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
  test_dl <- fl_DownloadFjord(fjord = "kong", layer = "K_PAR", tempdir())
  expect_type(test_dl, "NULL")
  fjord_test <- fl_LoadFjord("kong", "K_PAR", TS = FALSE, tempdir())
  expect_length(fjord_test$glob_attributes, 0)
})

test_that("fl_DownloadFjord gets ClimSD data files", {
  skip_if_offline()
  test_dl <- fl_DownloadFjord(fjord = "kong", layer = "ClimSD", tempdir())
  expect_type(test_dl, "NULL")
  fjord_test <- fl_LoadFjord("kong", "ClimSD", TS = FALSE, tempdir())
  expect_length(fjord_test$glob_attributes, 0)
})

test_that("fl_DownloadFjord gets YearlySD data files", {
  skip_if_offline()
  test_dl <- fl_DownloadFjord(fjord = "kong", layer = "YearlySD", tempdir())
  expect_type(test_dl, "NULL")
  fjord_test <- fl_LoadFjord("kong", "YearlySD", TS = FALSE, tempdir())
  expect_length(fjord_test$glob_attributes, 0)
})
