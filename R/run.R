source('lib/munger.R', chdir=TRUE)

library(dplyr, quietly=TRUE, warn.conflicts=FALSE)

rowCounts <- data.frame()
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
    validate$Year(currentYear)

    csv <- read.csv(
      paste(src, file, sep = '/'), header=FALSE, as.is=TRUE, encoding="UTF-8"
    )

    csv <- munge$InitializeColumns(csv) %>%
            munge$DoneeCols(partyTag, partyName) %>%
              munge$DateCols(currentYear)

    rowCounts <- rbind(rowCounts,
      data.frame(party_nickname=partyTag, year=currentYear, nrow=nrow(csv)))

    dataSet <- rbind(dataSet, csv)
  }
}

# stop execuition if the compiled data set does not contain all the source csv rows
validate$AllRowsAccountedFor(nrow(dataSet), rowCounts$n)

dataSet <- munge$NameCol(dataSet) %>%
            munge$PostalCodeCol() %>%
              munge$FilterUnviableRows() %>%
                merge(util$GetRidingConcordSet(), all.x=TRUE)

# validate$AllRidingsNormalized()
