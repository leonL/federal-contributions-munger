source('lib/munger.R', chdir=TRUE)

library(plyr, quietly=TRUE, warn.conflicts=FALSE)
library(dplyr, quietly=TRUE, warn.conflicts=FALSE)


rowCounts <- data.frame()
dataSet <- data.frame()
logg$SummaryInfo("\n\nMunging algorithim initiated...\n")

# aggregate all source data into a single data set
a_ply(k$AllPartyLabels, 1, function(partyLabels) {
  for(year in k$AllContribYears) {

    subset <- util$ReadAndFormatPartyYearSubset(partyLabels, year)

    rowCounts <<- rbind(rowCounts,
      data.frame(party_nickname=partyLabels['tag'], year=year, nrow=nrow(subset)))

    dataSet <<- rbind(dataSet, subset)
  }
})

# stop execuition if the compiled data set does not contain all the source csv rows
validate$AllRowsAccountedFor(nrow(dataSet), rowCounts$n)

loggin$SummaryInfo(
  "%s records sourced in all", util$FormatNum(nrow(dataSet)))

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

