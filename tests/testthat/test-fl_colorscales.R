# Tests for colourscale functions

test_that("fl_topocolorscale works", {
  scale_test <- fl_topocolorscale(-10)
  expect_type(scale_test, "list")
  expect_equal(length(scale_test$brks), 6)
  expect_equal(scale_test$colors[2], "#146FDC")
})

test_that("cs_blue works", {
  scale_test <- cs_blue(8)
  expect_type(scale_test, "character")
  expect_equal(length(scale_test), 8)
  expect_equal(scale_test[2], "#1F5AAF")
})

test_that("cs_BuYlRd works", {
  scale_test <- cs_BuYlRd(6)
  expect_type(scale_test, "character")
  expect_equal(length(scale_test), 6)
  expect_equal(scale_test[2], "#74ADD1")
})

test_that("cs_blye works", {
  scale_test <- cs_blye(4)
  expect_type(scale_test, "character")
  expect_equal(length(scale_test), 4)
  expect_equal(scale_test[2], "#9DA6C4")
})
