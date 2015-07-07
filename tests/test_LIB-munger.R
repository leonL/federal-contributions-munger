source('../R/lib/munger.R', chdir=TRUE)

a <- "test"

mock <- within(list(), {

  donorName <- "Alex Van Halen"

  dataSet <- data.frame(
    party_riding=c("Conservative Party of Canada / March 2014 / Quarterly",
      "Conservative Party of Canada", "York--Simcoe Conservative Association"),
    contrib.date=c("Jun 05, 2004", "Aug 01 2010", ""),
    postal_code=c("S0S 0S0", "L4C-9M2", "BADDPC"),
    donor.name=c(" estate of edward van halen", "Mr. and Estate of Mrs. Roth",
      donorName),
    contrib.amount=c(15050, 120000, 0)
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

test_that("munge$DonorNames...", {
  names <- munge$DonorNames(mock$dataSet$donor.name)
  expect_equal(as.character(names[1]), "Estate Of Edward Van Halen")
})

test_that("munge$PostalCodes...", {
  codes <- munge$PostalCodes(mock$dataSet$postal_code)
  expect_equal(codes[1], "S0S0S0")
  expect_equal(codes[2], "L4C9M2")
})

test_that("munge$ContribAmounts...", {
  amounts <- munge$ContribAmounts(mock$dataSet$contrib.amount)
  expect_equal(amounts[1], 150.50)
  expect_equal(amounts[2], 1200)
})

test_that("munge$FilterOutEstateContributions...", {
  dataSet <- munge$FilterOutEstateContributions(mock$dataSet, save.removedRows=FALSE)
  expect_equal(nrow(dataSet), 1)
  expect_equal(as.character(dataSet[1, 'donor.name']), mock$donorName)
})

test_that("munge$FilterOutInvalidPostalCodes...", {
  mock$dataSet$postal_code <- munge$PostalCodes(mock$dataSet$postal_code)
  dataSet <- munge$FilterOutInvalidPostalCodes(mock$dataSet, save.removedRows=FALSE)
  expect_equal(nrow(dataSet), 2)
  expect_equal(as.character(dataSet[1, 'postal_code']), "S0S0S0")
})

test_that("munge$FilterOutFakePostalCodes...", {
  mock$dataSet$postal_code <- munge$PostalCodes(mock$dataSet$postal_code)
  dataSet <- munge$FilterOutFakePostalCodes(mock$dataSet, save.removedRows=FALSE)
  expect_equal(nrow(dataSet), 2)
  expect_false("S0S0S0" %in% dataSet$postal_code)
})

context("Utility functions")

test_that("util$TitleCase...", {
  str <- "eddie van halen"
  expect_equal(util$TitleCase(str), "Eddie Van Halen")
})

test_that("util$GetPostalConcordSet caches data", {
  intialSet <- util$GetPostalConcordSet()
  expect_output(cacheSet <- util$GetPostalConcordSet(), "cache")
  expect_equal(intialSet, cacheSet)
})

test_that("GetAmbiguousPCodeConcordSubset returns only codes that reference multiple data points", {
  initialSubset <- util$GetAmbiguousPCodeConcordSubset()
  pcode <- initialSubset[1, 1]

  expect_equal(nrow(initialSubset), 2)
  expect_equal(pcode, "L4C9M2")
  expect_output(cacheSet <- util$GetAmbiguousPCodeConcordSubset(), "cache")
  expect_equal(initialSubset, cacheSet)
})

test_that("AmbiguousPostalCodesFilter", {
  mock$dataSet$postal_code <- munge$PostalCodes(mock$dataSet$postal_code)
  ambigSubset <- util$AmbiguousPostalCodesFilter(mock$dataSet)
  unambigSubset <- util$AmbiguousPostalCodesFilter(mock$dataSet, invertFilter=TRUE)

  expect_equal(nrow(ambigSubset), 1)
  expect_equal(ambigSubset$postal_code, "L4C9M2")
  expect_equal(nrow(unambigSubset), 2)
})

test_that("AmbiguousPostalCodeConcordResolver", {
  mock$dataSet$postal_code <- munge$PostalCodes(mock$dataSet$postal_code)
  contribRecord <- mock$dataSet[2,]
  contribRecord$target.riding_id <- 10001
  mergedRecord <- util$AmbiguousPostalCodeConcordResolver(contribRecord)

  expect_equal(mergedRecord$contributor.riding_id, 10001)

  contribRecord$target.riding_id <- 10002
  expect_output(util$AmbiguousPostalCodeConcordResolver(contribRecord), 'random')
})

context("Inline Validators")

test_that("validate$AllRowsAccountedFor...", {
  setCount <- 100
  validSubsetCounts <- c(30, 20, 50)
  invalidSubsetCounts <- c(10, 40, 66)
  expect_true(validate$AllRowsAccountedFor(setCount, validSubsetCounts))
  expect_error(validate$AllRowsAccountedFor(setCount, invalidSubsetCounts))
})

test_that("validate$AllRidingsNormalized...", {
  ids <- c(1,2,3)
  expect_true(validate$AllRidingsNormalized(ids))
  ids[4] <- NA
  expect_error(validate$AllRidingsNormalized(ids))
})