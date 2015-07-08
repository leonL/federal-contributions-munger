context("Munger functions")

mock <- within(list(), {
  dataSet <- util$ReadSrcCSV('contributions.csv', 'contributions')
})

test_that("munge$DoneeCols...", {
  partyName <- "Conservative Party of Canada"
  partyTag <- "Conservative"
  dataSet <- munge$DoneeCols(mock$dataSet, partyTag, partyName)
  expect_equal(dataSet[1, 'party_riding'], partyName)
  expect_equal(dataSet[2, 'party'], partyTag)
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
  expect_equal(as.character(dataSet[1, 'donor.name']), "Alex Van Halen")
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

test_that("MergeWithPCodeConcordance...", {
  # returns a data set with the same number of rows as the one that was passed into it
  dataSet <- util$ReadSrcCSV('ready_to_merge_pcodes.csv', 'contributions')
  result <- munge$MergeWithPCodeConcordance(dataSet)
  expect_equal(nrow(result), nrow(dataSet))
})

test_that("MergeWithPCodeConcordanceByRiding", {
  dataSet <- util$ReadSrcCSV('ready_to_merge_pcodes.csv', 'contributions')
  result <- munge$MergeWithPCodeConcordanceByRiding(dataSet)
  rowsWithConcord <- filter(result, !is.na(contributor.riding_name))

  expect_equal(nrow(rowsWithConcord), 1)
  expect_equal(rowsWithConcord$contributor.riding_name, "Avalon")
})