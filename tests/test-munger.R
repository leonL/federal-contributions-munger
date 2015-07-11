source('../R/lib/munger.R', chdir=TRUE)
source('config.R')

source('munger/test-munge.R')
source('munger/test-util.R')

context("Inline Validators")

test_that("AllRidingsNormalized...", {
  ids <- c(1,2,3)
  expect_true(validate$AllRidingsNormalized(ids))
  ids[4] <- NA
  expect_error(validate$AllRidingsNormalized(ids))
})

test_that("AllPostalCodesMerged", {
  dataSet <- util$ReadSrcCSV('contributions.csv', 'contributions')
  dataSet$contributor.riding_id <- 10001
  expect_true(validate$AllPostalCodesMerged(dataSet, dataSet))
  expect_error(validate$AllPostalCodesMerged(dataSet, data.frame()))
  dataSet$contributor.riding_id <- NA
  expect_error(validate$AllPostalCodesMerged(dataSet, dataSet))
})