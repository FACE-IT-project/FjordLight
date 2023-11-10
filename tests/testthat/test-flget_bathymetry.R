# Tests for flget_bathymetry
# This also tests flplot_bathymetry and flplot_land

test_that("Bathymetry retrieval works correctly", {
  fl_DownloadFjord(fjord = "test", tempdir())
  dat_no_TS <- fl_LoadFjord("test", dirdata = tempdir())

  expect_error(flget_bathymetry(dat_no_TS, what = "banana"),
               "Ensure that 'what' is one of the following options: 'o', 'ol', 'c', 'cl', 's', 'sl', 'l'")

  expect_s4_class(flget_bathymetry(dat_no_TS, what = "o"), "RasterLayer")
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "c"), "RasterLayer")
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "s", PLOT = TRUE), "RasterLayer") # Tests flplot_bathymetry
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "l", PLOT = TRUE), "RasterLayer") # Tests flplot_land
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "ol"), "RasterLayer")
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "cl"), "RasterLayer")
  expect_s4_class(flget_bathymetry(dat_no_TS, what = "sl", PLOT = TRUE), "RasterLayer") # Tests flplot_bathymetry

  expect_s3_class(flget_bathymetry(dat_no_TS, what = "ol", mode = "df"), "data.frame")
  expect_s3_class(flget_bathymetry(dat_no_TS, what = "o", mode = "df"), "data.frame")
  expect_s3_class(flget_bathymetry(dat_no_TS, what = "l", mode = "df"), "data.frame")
  expect_s3_class(flget_bathymetry(dat_no_TS, what = "s", mode = "df"), "data.frame")
  expect_s3_class(flget_bathymetry(dat_no_TS, what = "c", mode = "df"), "data.frame")
})
