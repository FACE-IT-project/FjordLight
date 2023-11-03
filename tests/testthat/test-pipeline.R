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


# Bathymetry --------------------------------------------------------------

test_that("P-functions error messages signal correctly", {
  dat_no_TS <- fl_LoadFjord("kong")

  # TODO: This is problematic. This first one should return an error, not a raster
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "banana"), "RasterLayer")
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "o"), "RasterLayer")
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "c"), "RasterLayer")
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "s"), "RasterLayer")
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "l"), "RasterLayer")
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "lo"), "RasterLayer")
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "lc"), "RasterLayer")
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "ls"), "RasterLayer")
  # TODO: Rather force specific character vectors so these options don't exist
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "ol"), "RasterLayer")
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "cl"), "RasterLayer")
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "sl"), "RasterLayer")

  expect_s3_class(flget_bathymetry(dat_no_TS, what = "l", mode = "3col", PLOT = TRUE), "data.frame")
  expect_s3_class(flget_bathymetry(dat_no_TS, what = "o", mode = "3col", PLOT = TRUE), "data.frame")
})


# PARclim -----------------------------------------------------------------

test_that("flget_PARbottomMonthlyTS functions correctly", {
  dat_no_TS <- fl_LoadFjord("kong")

  # Default
  expect_s4_class(flget_climatology(dat_no_TS), "RasterLayer")

  # TODO: It would probably be better if the default behaviour was an error or warning, not NULL
  expect_null(flget_climatology(dat_no_TS, mode = "banana"))
  expect_null(flget_climatology(dat_no_TS, optics = "papaya"))
  expect_null(flget_climatology(dat_no_TS, period = "mango"))
  expect_type(flget_climatology(dat_no_TS, period = "Clim"), "character")
  expect_type(flget_climatology(dat_no_TS, period = "Clim", month = 1), "character")
  expect_type(flget_climatology(dat_no_TS, period = "Yearly"), "character")
  expect_type(flget_climatology(dat_no_TS, period = "Yearly", year = 2000), "character")

  expect_s3_class(flget_climatology(dat_no_TS, period = "Clim", month = 8, mode = "3col"), "data.frame")

  # NB: This also tests flplot_PARbottomMonthlyTS
  expect_s4_class(flget_climatology(dat_no_TS, period = "Yearly", year = 2010, PLOT = TRUE), "RasterLayer")
  expect_s4_class(flget_climatology(dat_no_TS, period = "Clim", month = 4, PLOT = TRUE), "RasterLayer")
  expect_s4_class(flget_climatology(dat_no_TS, period = "Clim", optics = "PAR0m",
                                    month = 6, PLOT = TRUE), "RasterLayer")
  expect_s4_class(flget_climatology(dat_no_TS, period = "Clim", optics = "Kpar",
                                    month = 6, PLOT = TRUE), "RasterLayer")
})


# PARbottom ---------------------------------------------------------------

test_that("flget_PARbottomMonthlyTS functions correctly", {
  dat_no_TS <- fl_LoadFjord("kong")
  dat_TS <- fl_LoadFjord("kong", TS = TRUE)
  res_rast <- flget_PARbottomMonthlyTS(dat_TS, month = 4, year = 2010, mode = "raster")
  res_df <- flget_PARbottomMonthlyTS(dat_TS, month = 4, year = 2010, mode = "3col")
  res_df_years <- flget_PARbottomMonthlyTS(dat_TS, month = 4, mode = "3col")

  # NB: This also  tests flplot_PARbottomMonthlyTS
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

test_that("P-functions error messages signal correctly", {
  dat_no_TS <- fl_LoadFjord("kong")
  # TODO: These should rather be errors, not character vectors or NULL
  expect_type(flget_Pfunction(dat_no_TS, month = 4, year = 4), "character")
  expect_null(flget_Pfunction(dat_no_TS, type = "mango"))
  expect_null(flget_Pfunction(dat_no_TS, mode = "banana"))
  expect_null(flget_Pfunction(dat_no_TS, period = "papaya"))
  expect_type(flget_Pfunction(dat_no_TS, period = "Clim"), "character")
  expect_type(flget_Pfunction(dat_no_TS, period = "Clim", month = 1), "character")
  expect_type(flget_Pfunction(dat_no_TS, period = "Yearly"), "character")
  expect_type(flget_Pfunction(dat_no_TS, period = "Yearly", year = 2000), "character")

  res_year <- flget_Pfunction(dat_no_TS, period = "Yearly", year = 2010, mode = "2col")
  res_month <- flget_Pfunction(dat_no_TS, period = "Clim", month = 4, mode = "2col")
  res_func <- flget_Pfunction(dat_no_TS, period = "Clim", month = 4, mode = "function")

  # NB: This also  tests flplot_Pfunction
  res_plot <- flget_Pfunction(dat_no_TS, year = 2010, mode = "2col", PLOT = TRUE)
  res_plot_add <- flget_Pfunction(dat_no_TS, year = 2010, mode = "2col", PLOT = TRUE, add = TRUE)

  expect_s3_class(res_year, "data.frame")
  expect_s3_class(res_month, "data.frame")
  expect_type(res_func, "closure")
  expect_s3_class(res_plot, "data.frame")
})


# Remove test files
unlink("./FjordLight.d", recursive = TRUE)
