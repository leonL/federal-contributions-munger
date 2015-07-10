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

test_that("FindMatchingPostivieContrib...", {
  dataSet[1, c('contrib.date', 'year')] <- c('2004-06-01', '2004')
  negContrib <- otherContrib <- dataSet[1,]
  otherContrib$contrib.date <- '2004-07-01'
  dataSet <- rbind(dataSet, otherContrib)
  expect_equivalent(dataSet[nrow(dataSet),], otherContrib)

  negContrib$contrib.amount <- -negContrib$contrib.amount
  negContrib$contrib.date <- '2004-06-10'

  # returns first positive contribution match (match on everything but contrib.date)
  result <- util$FindMatchingPostivieContrib(negContrib, dataSet)
  expect_equal(result, "1")

  dataSet[1, 'year'] <- '2005'
  result <- util$FindMatchingPostivieContrib(negContrib, dataSet)
  expect_equal(result, "4")

  # returns NA if there is no match
  dataSet[4, 'year'] <- '2005'
  result <- util$FindMatchingPostivieContrib(negContrib, dataSet)
  expect_equal(result, NA)
})