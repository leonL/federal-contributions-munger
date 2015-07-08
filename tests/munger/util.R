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