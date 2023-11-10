# Testing for fl_ListFjords

test_that("fl_ListFjords works", {
  list_test <- fl_ListFjords()
  expect_type(list_test, "character")
  expect_equal(length(list_test), 7)
  expect_equal(list_test[2], "is")
})
