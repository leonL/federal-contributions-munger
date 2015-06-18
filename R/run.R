source('lib/munger.R', chdir=TRUE)

library(dplyr, quietly=TRUE, warn.conflicts=FALSE)

log <- data.frame()
dataSet <- data.frame()

for(i in 1:nrow(k$AllParties)) {
  partyTag <- as.character(k$AllParties[i, 'tag'])
  partyName <- as.character(k$AllParties[i, 'name'])

  src <- paste(k$SourcePath, partyTag, sep = '/')
  srcFiles <- list.files(src)

  print(paste("Reading and munging data for", partyName))
  for(file in srcFiles) {
    print(file)

    currentYear <- strsplit(file, ".", fixed=TRUE)[[1]][2]
    test$ValidateYear(currentYear)

    csv <- read.csv(
      paste(src, file, sep = '/'), header=FALSE, as.is=TRUE, encoding="UTF-8"
    )

    csv <- munge$initializeColumns(csv) %>%
            munge$doneeCols(partyTag, partyName) %>%
            munge$dateCols(currentYear)

    log <- rbind(log,
      data.frame(party_nickname=partyTag, year=currentYear, nrow=nrow(csv)))

    dataSet <- rbind(dataSet, csv)
  }
}

test$AllRowsAccountedFor(subsetN = sum(log$n), dataSetN = nrow(dataSet))