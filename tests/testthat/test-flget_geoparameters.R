# Tests for flget_geoparameters

test_that("flget_geoparameters works", {
  dat_no_TS <- fl_LoadFjord("test", dirdata = system.file("extdata", package = "FjordLight"))
  res_geo <- flget_geoparameters(dat_no_TS)
  expect_type(res_geo, "double")
})
