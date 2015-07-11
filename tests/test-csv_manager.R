source('../R/lib/csv_manager.R', chdir=TRUE)
source('config.R')

context("csv_manger utility functions")

dataSet <- util$ReadSrcCSV('contributions.csv', 'contributions')

test_that("ShardContributionsByPartyYear...", {
  # the expectation for this test is that it creates the correct folders and
  # files in data/test/output/contributions
  dataSet <- mutate(dataSet, contrib.year = year) %>% select(-year)
  util$ShardContributionsByPartyYear(dataSet)
})