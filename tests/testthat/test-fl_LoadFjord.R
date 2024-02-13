# Testing for fl_LoadFjords

test_that("loading errors are correct", {
  expect_error(fl_LoadFjord("test"),
               "Please provide the pathway from where you would like to load the data.")
  expect_error(fl_LoadFjord("test", dirdata = "guava"),
               "Please ensure that the chosen directory exists.")
  expect_error(fl_LoadFjord("test", dirdata = system.file("extdata", package = "FjordLight"), layer = "banana"),
               "Please ensure the 'layer' value is either 'PAR_B', 'K_PAR', 'ClimSD', or 'YearlySD'")
})

test_that("loading with and without TS works", {
  test_NO_TS <- fl_LoadFjord("test", dirdata = system.file("extdata", package = "FjordLight"))
  expect_type(test_NO_TS, "list")
  expect_equal(length(test_NO_TS$longitude), 7)
  expect_equal(round(test_NO_TS$AreaOfShallowZone), 106)
  test_TS <- fl_LoadFjord("test", dirdata = system.file("extdata", package = "FjordLight"), TS = TRUE)
  expect_type(test_TS, "list")
  expect_type(test_TS$MonthlyPARbottom, "double")
})

test_that("loading monthly K_PAR works", {
  test_NO_TS <- fl_LoadFjord("test", "K_PAR", system.file("extdata", package = "FjordLight"))
  expect_type(test_NO_TS, "list")
  expect_equal(length(test_NO_TS$glob_attributes), 0)
  test_TS <- fl_LoadFjord("test", "K_PAR", dirdata = system.file("extdata", package = "FjordLight"), TS = TRUE)
  expect_type(test_TS, "list")
  expect_type(test_TS$MonthlyKpar, "double")
})

test_that("loading ClimSD works", {
  test_TS <- fl_LoadFjord("test", "ClimSD", dirdata = system.file("extdata", package = "FjordLight"), TS = TRUE)
  expect_type(test_TS, "list")
  expect_equal(length(test_TS$glob_attributes), 0)
  expect_type(test_TS$ClimPARbottomSD, "double")
})

test_that("loading YearlySD works", {
  test_TS <- fl_LoadFjord("test", "YearlySD", dirdata = system.file("extdata", package = "FjordLight"), TS = TRUE)
  expect_type(test_TS, "list")
  expect_equal(length(test_TS$glob_attributes), 0)
  expect_type(test_TS$YearlyPARbottomSD, "double")
})
