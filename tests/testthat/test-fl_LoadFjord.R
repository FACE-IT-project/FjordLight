# Testing for fl_ListFjords

test_that("loading errors are correct", {
  expect_error(fl_LoadFjord("test"),
               "Please provide the pathway from where you would like to load the data.")
  expect_error(fl_LoadFjord("test", dirdata = "guava"),
               "Please ensure that the chosen directory exists.")
})

test_that("loading with and without TS works", {
  fl_DownloadFjord(fjord = "test", tempdir())
  test_NO_TS <- fl_LoadFjord("test", dirdata = tempdir())
  expect_type(test_NO_TS, "list")
  expect_equal(length(test_NO_TS$longitude), 7)
  expect_equal(round(test_NO_TS$AreaOfShallowZone), 106)
  test_TS <- fl_LoadFjord("test", dirdata = tempdir(), TS = TRUE)
  expect_type(test_TS, "list")
  expect_type(test_TS$MonthlyPARbottom, "double")
})
