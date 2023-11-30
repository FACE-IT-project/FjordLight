# Tests for flget_Pfunction
# This also tests flplot_Pfunction

test_that("Various P-functions calls work as expected", {
  dat_no_TS <- fl_LoadFjord("test", system.file("extdata", package = "FjordLight"))

  expect_type(flget_Pfunction(dat_no_TS), "closure")

  expect_error(flget_Pfunction(dat_no_TS, month = 5, year = 2020),
               "You have to indicate month or year, not both")

  expect_error(flget_Pfunction(dat_no_TS, mode = "banana"),
               "Wrong mode, choose among: 'function', 'df'")
  expect_error(flget_Pfunction(dat_no_TS, period = "mango"),
               "Wrong period, choose among: 'Clim', 'Yearly', 'Global'")
  expect_error(flget_Pfunction(dat_no_TS, period = "Clim"),
               "Please indicate the month")
  expect_error(flget_Pfunction(dat_no_TS, period = "Clim", month = 6:8),
               "Please select a single month")
  expect_error(flget_Pfunction(dat_no_TS, period = "Clim", month = 1),
               "Bad month: available months 3 4 5 6 7 8 9")
  expect_error(flget_Pfunction(dat_no_TS, period = "Clim", month = "banana"),
               "Bad month: available months 3 4 5 6 7 8 9")

  expect_error(flget_Pfunction(dat_no_TS, period = "Yearly"),
               "Please indicate the year")
  expect_error(flget_Pfunction(dat_no_TS, period = "Yearly", year = 2001:2003),
               "Please select a single year")
  expect_error(flget_Pfunction(dat_no_TS, period = "Yearly", year = 2001),
               "Bad year: available years 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022")
  expect_error(flget_Pfunction(dat_no_TS, period = "Yearly", year = "banana"),
               "Bad year: available years 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022")

  res_year <- flget_Pfunction(dat_no_TS, period = "Yearly", year = 2010, mode = "df")
  res_month <- flget_Pfunction(dat_no_TS, period = "Clim", month = 4, mode = "df")
  res_func <- flget_Pfunction(dat_no_TS, period = "Clim", month = 4, mode = "function")

  res_plot <- flget_Pfunction(dat_no_TS, period = "Global", mode = "df", PLOT = TRUE) # Tests flplot_Pfunction
  res_plot_add <- flget_Pfunction(dat_no_TS, period = "Yearly", year = 2010, mode = "df", PLOT = TRUE, add = TRUE) # Tests flplot_Pfunction

  expect_s3_class(res_year, "data.frame")
  expect_equal(ncol(res_year), 2)
  expect_s3_class(res_month, "data.frame")
  expect_equal(nrow(res_month), 101)
  expect_type(res_func, "closure")
  expect_s3_class(res_plot, "data.frame")
})
