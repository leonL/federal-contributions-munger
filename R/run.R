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
dataSet <- merge(dataSet, util$GetRidingConcordSet(), all.x=TRUE)
ridingIds <- filter(dataSet, donee.riding_level) %>% select(target.riding_id)
validate$AllRidingsNormalized(ridingIds)

# add city, provice, contributor.riding, and geolocation columns based on postal code
dataSet <- munge$MergeWithPCodeConcordance(dataSet)