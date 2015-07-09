source('../R/lib/munger.R', chdir=TRUE)
source('config.R')

source('munger/munge.R')
source('munger/util.R')

mock <- within(list(), {
  dataSet <- util$ReadSrcCSV('contributions.csv', 'contributions')
})

context("Inline Validators")

test_that("validate$AllSubsetRowsAccountedFor...", {
  setCount <- 100
  validSubsetCounts <- c(30, 20, 50)
  invalidSubsetCounts <- c(10, 40, 66)
  expect_true(validate$AllSubsetRowsAccountedFor(setCount, validSubsetCounts))
  expect_error(validate$AllSubsetRowsAccountedFor(setCount, invalidSubsetCounts))
})

test_that("validate$AllRidingsNormalized...", {
  ids <- c(1,2,3)
  expect_true(validate$AllRidingsNormalized(ids))
  ids[4] <- NA
  expect_error(validate$AllRidingsNormalized(ids))
})

test_that("AllPostalCodesMerged", {
  mock$dataSet$contributor.riding_id <- 10001
  expect_true(validate$AllPostalCodesMerged(mock$dataSet, mock$dataSet))
  expect_error(validate$AllPostalCodesMerged(mock$dataSet, data.frame()))
  mock$dataSet$contributor.riding_id <- NA
  expect_error(validate$AllPostalCodesMerged(mock$dataSet, mock$dataSet))
})