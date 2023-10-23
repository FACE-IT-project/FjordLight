# tests for colourscale functions

test_that("fl_topocolorscale works", {
  scale_test <- fl_topocolorscale(-10)
  expect_type(scale_test, "list")
  expect_equal(length(scale_test$brks), 6)
  expect_equal(scale_test$colors[2], "#146FDC")
})
