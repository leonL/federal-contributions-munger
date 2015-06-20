source('../R/lib/base.R', chdir=TRUE)

test_that("An error is thrown when an inline validation fails", {
  expect_error(validate$Base(FALSE, "error message"), "error message")
})