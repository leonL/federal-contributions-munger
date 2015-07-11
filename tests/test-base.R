source('../R/lib/base.R', chdir=TRUE)
source('config.R')

context("Base functions")

test_that("An error is thrown when an inline validation fails", {
  expect_error(validate$Base(FALSE, "error message"), "error message")
})

test_that("validate$IsNotNA...", {
  expect_error(validate$IsNotNA(c('str', NA), "error message"), "error message")
  expect_true(validate$IsNotNA(c('str'), "error message"))
})