# Tests for flget_area

test_that("flget_area works", {
  dat_no_TS <- fl_LoadFjord("test", dirdata = system.file("extdata", package = "FjordLight"))
  res_rast <- flget_area(dat_no_TS, mode = "raster")
  res_df <- flget_area(dat_no_TS, mode = "df")
  expect_s4_class(res_rast, "RasterLayer")
  expect_s3_class(res_df, "data.frame")
})
