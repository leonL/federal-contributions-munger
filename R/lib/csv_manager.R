# load dependencies

library(dplyr, quietly=TRUE, warn.conflicts=FALSE)

# utility functions

if(!exists("util")) { util <- list() }
util <- within(util, {

  ShardContributionsByPartyYear <- function(dataSet) {
    allParties <- AllLevels(dataSet$party)
    allYears <- AllLevels(dataSet$contrib.year) %>% as.integer()
    rowCounts <- data.frame()

    for(partyTag in allParties) {
      for(year in allYears) {
        subset <- filter(dataSet, contrib.year==year, party==partyTag)
        if(nrow(subset) > 0) {
          SaveContributionsByPartyYearCSV(subset,
                      GetContributionsFileNamePrefix(partyTag), year, partyTag)
          rowCounts <- rbind(rowCounts,
            data.frame(party=partyTag, year=year, nrow=nrow(subset)))
        }
      }
    }
    # stop execuition if the compiled data set does not contain all the source csv rows
    validate$AllSubsetRowsAccountedFor(nrow(dataSet), rowCounts$n)
  }

  SaveContributionsByPartyYearCSV <- function(data, partyName, year, subfolder) {
    filename <- paste(partyName, '.', year, '.csv', sep='')
    subfolder <- paste('contributions', subfolder, sep='/')
    SaveCSV(data, filename, subfolder)
    logg$SummaryInfo("Saved %s rows to %s", util$FormatNum(nrow(data)), filename)
  }

  AllLevels <- function(vector) {
    as.factor(vector) %>% levels()
  }

})