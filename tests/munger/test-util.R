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

test_that("GetDedupedPostalConcordSet...", {
  allConcords <- util$GetPostalConcordSet()
  L4C9M2 <- filter(allConcords, postal_code == 'L4C9M2')
  expect_equal(nrow(L4C9M2), 2)

  deduped <- util$GetDedupedPostalConcordSet()
  L4C9M2 <- filter(deduped, postal_code == 'L4C9M2')
  expect_equal(nrow(L4C9M2), 1)

  expect_output(util$GetDedupedPostalConcordSet(), "cache")
})

test_that("MatchingPostivieContribIndices...", {
  dataSet[1, c('contrib.date', 'year')] <- c('2004-06-01', '2004')
  negContrib <- otherContrib <- dataSet[1,]
  otherContrib$contrib.date <- '2004-07-01'
  dataSet <- rbind(dataSet, otherContrib)
  expect_equivalent(dataSet[nrow(dataSet),], otherContrib)

  negContrib$contrib.amount <- -negContrib$contrib.amount
  negContrib$contrib.date <- '2004-06-10'

  # returns at most one match for every negative contrib
  result <- util$MatchingPostivieContribIndices(negContrib, dataSet)
  expect_equal(result, 1)

  # does not conflate 'duplicated' negative contrib records
  otherNegContrib <- negContrib; otherNegContrib$contrib.date <- '2004-07-10'
  negContribs <- rbind(negContrib, otherNegContrib)
  result <- util$MatchingPostivieContribIndices(negContribs, dataSet)
  expect_equal(result, c(1, 4))


  # returns empty vector if no matches found
  dataSet[c(1,4), 'year'] <- '2005'
  result <- util$MatchingPostivieContribIndices(negContrib, dataSet)
  expect_equal(result, numeric())
})