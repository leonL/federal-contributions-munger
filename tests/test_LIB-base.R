source('../R/lib/munger.R', chdir=TRUE)

test_that("An error is thrown when an inline test fails", {
  expect_error(test$Base(FALSE, "error message"), "error message")
})