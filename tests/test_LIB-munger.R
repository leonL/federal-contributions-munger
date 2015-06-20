source('../R/lib/munger.R', chdir=TRUE)

a <- "test"

mock <- within(list(), {

  donorName <- "Alex Van Halen"

  dataSet <- data.frame(
    party_riding=c("Conservative Party of Canada / March 2014 / Quarterly",
      "Conservative Party of Canada", "York--Simcoe Conservative Association"),
    contrib.date=c("Jun 05, 2004", "Aug 01 2010", ""),
    postal_code=c("M6P 1N9", "L4C-9M2", "BADDPC"),
    donor.name=c(" estate of edward van halen", "Mr. and Estate of Mrs. Roth",
      donorName)
  )

})

k$PartyName <- "Conservative Party of Canada"
k$PartyTag <- "Conservative"

context("Munger functions")

test_that("munge$DoneeCols...", {
  dataSet <- munge$DoneeCols(mock$dataSet, k$PartyTag, k$PartyName)
  expect_equal(dataSet[1, 'party_riding'], k$PartyName)
  expect_equal(dataSet[2, 'party'], k$PartyTag)
  expect_false(dataSet[1, 'donee.riding_level'])
  expect_true(dataSet[3, 'donee.riding_level'])
})

test_that("munge$DateCols...", {
  currentYear <- "2007"
  dataSet <- munge$DateCols(mock$dataSet, currentYear)
  expect_equal(dataSet[1, 'contrib.month.day'], "06-05")
  expect_equal(dataSet[1, 'contrib.year'], currentYear)
  expect_error(dataSet['contrib.date'])
})

test_that("munge$FilterEstateContributions...", {
  dataSet <- munge$FilterEstateContributions(mock$dataSet, saveCSV=FALSE)
  expect_equal(nrow(dataSet), 1)
  expect_equal(as.character(dataSet[1, 'donor.name']), mock$donorName)
})

test_that("munge$NameCol...", {
  dataSet <- munge$NameCol(mock$dataSet)
  expect_equal(as.character(dataSet[1, 'donor.name']), "Estate Of Edward Van Halen")
})

test_that("munge$PostalCodeCol...", {
  dataSet <- munge$PostalCodeCol(mock$dataSet)
  expect_equal(dataSet[1, 'postal_code'], "M6P1N9")
  expect_equal(dataSet[2, 'postal_code'], "L4C9M2")
})

context("Utility functions")

test_that("util$TitleCase...", {
  str <- "eddie van halen"
  expect_equal(util$TitleCase(str), "Eddie Van Halen")
})

context("Inline Validators")


test_that("validate$AllRowsAccountedFor...", {
  setCount <- 100
  validSubsetCounts <- c(30, 20, 50)
  invalidSubsetCounts <- c(10, 40, 66)
  expect_true(validate$AllRowsAccountedFor(setCount, validSubsetCounts))
  expect_error(validate$AllRowsAccountedFor(setCount, invalidSubsetCounts))
})