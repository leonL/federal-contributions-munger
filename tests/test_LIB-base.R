source('../R/lib/base.R', chdir=TRUE)

flog.threshold(FATAL)

context("Base functions")

test_that("An error is thrown when an inline validation fails", {
  expect_error(validate$Base(FALSE, "error message"), "error message")
})