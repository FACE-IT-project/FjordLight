# Testing for fl_ListFjords

test_that("fl_ListFjords works", {
  list_test <- fl_ListFjords()
  expect_type(list_test, "character")
  # These tests break if files are uploaded to the FTP server
  # expect_equal(length(list_test), 8)
  # expect_equal(list_test[2], "is")
})
