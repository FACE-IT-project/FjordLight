# Tests for flget_climatology
# This also tests flplot_climatology

test_that("flget_climatology functions correctly", {
  dat_no_TS <- fl_LoadFjord("test", system.file("extdata", package = "FjordLight"))

  expect_s4_class(flget_climatology(dat_no_TS), "RasterLayer")

  expect_error(flget_climatology(dat_no_TS, month = 5, year = 2020),
               "You have to indicate month or year, not both")

  expect_error(flget_climatology(dat_no_TS, mode = "banana"),
               "Wrong mode, choose among: 'raster', 'df'")
  expect_error(flget_climatology(dat_no_TS, optics = "papaya"),
               "Wrong optics, choose among: 'PARbottom', 'PAR0m', 'Kpar'")
  expect_error(flget_climatology(dat_no_TS, period = "mango"),
               "Wrong period, choose among: 'Clim', 'Yearly', 'Global'")

  expect_error(flget_climatology(dat_no_TS, period = "Clim"),
               "Please indicate the month")
  expect_error(flget_climatology(dat_no_TS, period = "Clim", month = 6:8),
               "Please select a single month")
  expect_error(flget_climatology(dat_no_TS, period = "Clim", month = 1),
               "Bad month: available months 3 4 5 6 7 8 9")
  expect_error(flget_climatology(dat_no_TS, period = "Clim", month = "banana"),
               "Bad month: available months 3 4 5 6 7 8 9")

  expect_error(flget_climatology(dat_no_TS, period = "Yearly"),
               "Please indicate the year")
  expect_error(flget_climatology(dat_no_TS, period = "Yearly", year = 2001:2003),
               "Please select a single year")
  expect_error(flget_climatology(dat_no_TS, period = "Yearly", year = 2001),
               "Bad year: available years 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022")
  expect_error(flget_climatology(dat_no_TS, period = "Yearly", year = "banana"),
               "Bad year: available years 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022")

  clim_plot1 <- flget_climatology(dat_no_TS, period = "Yearly", year = 2012, PLOT = TRUE) # Tests flplot_climatology
  clim_plot1 <- flget_climatology(dat_no_TS, period = "Clim", month = 8, optics = "PAR0m", PLOT = TRUE) # Tests flplot_climatology
  clim_plot1 <- flget_climatology(dat_no_TS, period = "Yearly", year = 2020, optics = "Kpar", PLOT = TRUE) # Tests flplot_climatology

  clim_rast <- flget_climatology(dat_no_TS, period = "Yearly", year = 2012, mode = "raster", PLOT = FALSE)
  clim_df <- flget_climatology(dat_no_TS, period = "Yearly", year = 2012, mode = "df", PLOT = FALSE)
  expect_s4_class(clim_rast, "RasterLayer")
  expect_s3_class(clim_df, "data.frame")
})
