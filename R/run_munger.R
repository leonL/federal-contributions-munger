source('lib/munger.R', chdir=TRUE)

logg$SummaryInfo("\n\nMunging initiated...\n")

# aggregate all source data into a single data set
dataSet <- util$InitContribsDataSet()

# cleanup and format values
dataSet <- within(dataSet, {
  donor.name <- munge$DonorNames(donor.name)
  postal_code <- munge$PostalCodes(postal_code)
  contrib.amount <- munge$ContribAmount(contrib.amount)
})

# filter out unusable rows from the data set
dataSet <- munge$FilterOutUnusableRows(dataSet)

# replace party specific riding names with normalized riding names
dataSet <- munge$NormalizeRidingNames(dataSet)

# add city, provice, contributor.riding, and geolocation columns based on postal code
dataSet <- munge$MergeWithPCodeConcordance(dataSet)

# order the data set cols and sort the data
dataSet <- util$FinalContribOrderAndSort(dataSet)

# save munged data to CSVs
if(k$saveContribsByPartyYearCSV) {
  source('lib/csv_manager.R')
  util$ShardContributionsByPartyYear(dataSet)
}
if(k$saveAllContribsToOneCSV) {
  util$SaveContributionsCSV(dataSet)
}