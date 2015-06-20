source('lib/munger.R', chdir=TRUE)

library(plyr, quietly=TRUE, warn.conflicts=FALSE)
library(dplyr, quietly=TRUE, warn.conflicts=FALSE)

loggin$SummaryInfo("\n\nMunging algorithim initiated...\n")

rowCounts <- data.frame()
dataSet <- data.frame()

a_ply(k$AllPartyLabels, 1, function(partyLabels) {
  for(year in k$AllContribYears) {

    subset <- util$ReadAndFormatSubset(partyLabels, year)

    rowCounts <<- rbind(rowCounts,
      data.frame(party_nickname=partyLabels['tag'], year=year, nrow=nrow(subset)))

    dataSet <<- rbind(dataSet, subset)
  }
})

# stop execuition if the compiled data set does not contain all the source csv rows
validate$AllRowsAccountedFor(nrow(dataSet), rowCounts$n)

loggin$SummaryInfo(
  "%s records sourced in all", util$FormatNum(nrow(dataSet)))

dataSet <- munge$NameCol(dataSet) %>%
            munge$PostalCodeCol() %>%
              munge$FilterUnviableRows() %>%
                merge(util$GetRidingConcordSet(), all.x=TRUE)

# validate$AllRidingsNormalized()
