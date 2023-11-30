# Testing for fl_ListFjords

test_that("fl_ListFjords works", {
  skip_if_offline()
  list_test <- fl_ListFjords()
  expect_type(list_test, "character")
})
