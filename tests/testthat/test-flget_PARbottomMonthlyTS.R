# Tests for flget_PARbottomMonthlyTS
# This also tests flplot_PARbottomMonthlyTS

test_that("flget_PARbottomMonthlyTS functions correctly", {
  dat_no_TS <- fl_LoadFjord("test", dirdata = system.file("extdata", package = "FjordLight"))
  dat_TS <- fl_LoadFjord("test", dirdata = system.file("extdata", package = "FjordLight"), TS = TRUE)

  expect_error(flget_PARbottomMonthlyTS(dat_no_TS), "MonthlyPARbottom time series not loaded")
  expect_error(flget_PARbottomMonthlyTS(dat_TS, mode = 1), "Wrong mode, choose among: 'raster', 'df'")
  expect_error(flget_PARbottomMonthlyTS(dat_TS, mode = "banana"), "Wrong mode, choose among: 'raster', 'df'")

  # NB: Error messages are generated from objects so don't pass correctly through testthat
  expect_error(flget_PARbottomMonthlyTS(dat_TS, month = 1))
  expect_error(flget_PARbottomMonthlyTS(dat_TS, month = 1:6))
  expect_error(flget_PARbottomMonthlyTS(dat_TS, month = "banana"))
  expect_error(flget_PARbottomMonthlyTS(dat_TS, year = 2001))
  expect_error(flget_PARbottomMonthlyTS(dat_TS, year = 2000:2020))
  expect_error(flget_PARbottomMonthlyTS(dat_TS, year = "banana"))

  res_plot <- flget_PARbottomMonthlyTS(dat_TS, month = 4, year = 2010, PLOT = TRUE) # Tests flplot_PARbottomMonthlyTS

  res_rast <- flget_PARbottomMonthlyTS(dat_TS, month = 8:9, year = 2010:2013, mode = "raster")
  res_df <- flget_PARbottomMonthlyTS(dat_TS, month = c(4, 6, 8), year = c(2010, 2012, 2014), mode = "df")

  res_all_month <- flget_PARbottomMonthlyTS(dat_TS, year = 2010, mode = "df")
  res_all_year <- flget_PARbottomMonthlyTS(dat_TS, month = 8, mode = "df")

  expect_s4_class(res_plot, "RasterStack")
  expect_s4_class(res_rast, "RasterStack")
  expect_s3_class(res_df, "data.frame")
  expect_equal(ncol(res_df), 11)
  expect_equal(nrow(res_df), 42)
  expect_s3_class(res_all_month, "data.frame")
  expect_s3_class(res_all_year, "data.frame")
})
