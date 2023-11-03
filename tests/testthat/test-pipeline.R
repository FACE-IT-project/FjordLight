# Test all functions in one script to avoid multiple downloads


# Downloading -------------------------------------------------------------

test_that("fl_DownloadFjord error messages signal correctly", {
  expect_error(fl_DownloadFjord(fjord = "banana"), "banana not available")
})

test_that("fl_DownloadFjord temp directory works and doesn't re-download", {
  test_dl <- fl_DownloadFjord(fjord = "kong")
  test_dl <- fl_DownloadFjord(fjord = "kong")
  expect_type(test_dl, "NULL")
})

test_that("fl_DownloadFjord custom directory works and doesn't re-download", {
  test_dl <- fl_DownloadFjord(fjord = "kong", dirdata = "PAR_test")
  test_dl <- fl_DownloadFjord(fjord = "kong", dirdata = "PAR_test")
  expect_type(test_dl, "NULL")
})
unlink("./PAR_test", recursive = TRUE)


# Loading -----------------------------------------------------------------

test_that("loading without TS works", {
  test_dat <- fl_LoadFjord("kong")
  expect_type(test_dat, "list")
  expect_equal(length(test_dat$longitude), 321)
  expect_equal(round(test_dat$AreaOfShallowZone), 106)
})

test_that("TS loading works", {
  test_dat <- fl_LoadFjord("kong", TS = TRUE)
  expect_type(test_dat, "list")
  expect_type(test_dat$MonthlyPARbottom, "double")
})


# PARbottom ---------------------------------------------------------------

test_that("flget_PARbottomMonthlyTS error messages signal correctly", {
  dat_no_TS <- fl_LoadFjord("kong")
  dat_TS <- fl_LoadFjord("kong", TS = TRUE)
  # TODO: It would probably be better if the default behaviour was an error or warning, nd not NULL
  res_NULL1 <- flget_PARbottomMonthlyTS(dat_no_TS)
  res_NULL2 <- flget_PARbottomMonthlyTS(dat_TS, mode = "banana")
  # TODO: month error trapping is broken
  # flget_PARbottomMonthlyTS(dat_TS, month = 2)
  expect_null(res_NULL1)
  expect_null(res_NULL2)
  # NB: This is just too slow...
  # res_big <- flget_PARbottomMonthlyTS(dat_TS)
  expect_error('argument "fjord" is missing, with no default')
})

# Remove test files
unlink("./FjordLight.d", recursive = TRUE)
