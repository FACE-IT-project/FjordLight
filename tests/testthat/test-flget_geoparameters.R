# Tests for flget_geoparameters

test_that("flget_geoparameters works", {
  fl_DownloadFjord(fjord = "test", tempdir())
  dat_no_TS <- fl_LoadFjord("test", dirdata = tempdir())
  res_geo <- flget_geoparameters(dat_no_TS)
  expect_type(res_geo, "double")
})
