source('lib/munger.R', chdir=TRUE)

library(plyr, quietly=TRUE, warn.conflicts=FALSE)
library(dplyr, quietly=TRUE, warn.conflicts=FALSE)

logg$SummaryInfo("\n\nMunging algorithim initiated...\n")

# aggregate all source data into a single data set
dataSet <- util$AggreateSrcData()

# cleanup and format values
dataSet <- within(dataSet, {
  donor.name <- munge$DonorNames(donor.name)
  postal_code <- munge$PostalCodes(postal_code)
  contrib.amount <- munge$ContribAmount(contrib.amount)
})

# filter out unusable rows from the data set
dataSet <- munge$FilterOutUnusableRows(dataSet)

# merge normalized riding names into the data set
dataSet <- merge(dataSet, util$GetRidingConcordSet(), all.x=TRUE)
ridingIds <- filter(dataSet, donee.riding_level) %>% select(target.riding_id)
validate$AllRidingsNormalized(ridingIds)

# merge