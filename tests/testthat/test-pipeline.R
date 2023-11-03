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


# Area --------------------------------------------------------------------

test_that("flget_area works", {
  dat_no_TS <- fl_LoadFjord("kong")
  res_rast <- flget_area(dat_no_TS, mode = "raster")
  res_df <- flget_area(dat_no_TS, mode = "3col")
  expect_s4_class(res_rast, "RasterLayer")
  expect_s3_class(res_df, "data.frame")
})


# Geo-parameters ----------------------------------------------------------

test_that("flget_geoparameters works", {
  dat_no_TS <- fl_LoadFjord("kong")
  res_geo <- flget_geoparameters(dat_no_TS)
  expect_type(res_geo, "double")
})


# PARbottom ---------------------------------------------------------------

test_that("flget_PARbottomMonthlyTS error messages signal correctly", {
  dat_no_TS <- fl_LoadFjord("kong")
  dat_TS <- fl_LoadFjord("kong", TS = TRUE)
  res_rast <- flget_PARbottomMonthlyTS(dat_TS, month = 4, year = 2010, mode = "raster")
  res_df <- flget_PARbottomMonthlyTS(dat_TS, month = 4, year = 2010, mode = "3col")
  res_df_years <- flget_PARbottomMonthlyTS(dat_TS, month = 4, mode = "3col")
  # NB: This also fully tests flplot_PARbottomMonthlyTS
  res_plot <- flget_PARbottomMonthlyTS(dat_TS, month = 4, year = 2010, PLOT = TRUE)
  # TODO: It would probably be better if the default behaviour was an error or warning, not NULL
  expect_null(flget_PARbottomMonthlyTS(dat_no_TS))
  expect_type(flget_PARbottomMonthlyTS(dat_TS, month = 1), "character")
  expect_type(flget_PARbottomMonthlyTS(dat_TS, year = 2000), "character")
  expect_null(flget_PARbottomMonthlyTS(dat_TS, mode = "banana"))
  expect_s4_class(res_rast, "RasterStack")
  expect_s3_class(res_df, "data.frame")
  expect_s3_class(res_df_years, "data.frame")
  expect_s4_class(res_plot, "RasterStack")
})


# P-functions -------------------------------------------------------------



# Remove test files
unlink("./FjordLight.d", recursive = TRUE)
