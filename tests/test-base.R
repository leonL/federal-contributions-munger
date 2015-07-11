source('../R/lib/base.R', chdir=TRUE)
source('config.R')

context("Base functions")

test_that("An error is thrown when an inline validation fails", {
  expect_error(validate$Base(FALSE, "error message"), "error message")
})

test_that("IsNotNA...", {
  expect_error(validate$IsNotNA(c('str', NA), "error message"), "error message")
  expect_true(validate$IsNotNA(c('str'), "error message"))
})

test_that("validate$AllSubsetRowsAccountedFor...", {
  setCount <- 100
  validSubsetCounts <- c(30, 20, 50)
  invalidSubsetCounts <- c(10, 40, 66)
  expect_true(validate$AllSubsetRowsAccountedFor(setCount, validSubsetCounts))
  expect_error(validate$AllSubsetRowsAccountedFor(setCount, invalidSubsetCounts))
})