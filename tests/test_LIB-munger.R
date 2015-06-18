source('../R/lib/munger.R', chdir=TRUE)

mock <- within(list(), {

  dataSet <- data.frame(
    party_riding=c("Conservative Party of Canada / March 2014 / Quarterly",
      "Conservative Party of Canada", "York--Simcoe Conservative Association"),
    contrib.date=c("Jun 05, 2004", "Aug 01 2010", ""),
    postal_code=c("M6P 1N9", "L4C9M2", "BADDPC")
  )

})

k$PartyName <- "Conservative Party of Canada"
k$PartyTag <- "Conservative"

test_that("munge$doneeCols is sane", {
  dataSet <- munge$doneeCols(mock$dataSet, k$PartyTag, k$PartyName)
  expect_equal(dataSet[1, 'party_riding'], k$PartyName)
  expect_equal(dataSet[2, 'party'], k$PartyTag)
  expect_true(dataSet[1, 'target.federal'])
  expect_false(dataSet[3, 'target.federal'])
})

test_that("munge$dateCols is sane", {
  currentYear <- "2007"
  dataSet <- munge$dateCols(mock$dataSet, currentYear)
  expect_equal(dataSet[1, 'contrib.month.day'], "06-05")
  expect_equal(dataSet[1, 'contrib.year'], currentYear)
  expect_error(dataSet['contrib.date'])
})