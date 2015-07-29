# load dependencies

library(plyr, quietly=TRUE, warn.conflicts=FALSE)
library(dplyr, quietly=TRUE, warn.conflicts=FALSE)

source('base/util.R')

# Constants & Config

if(!exists("k")) { k <- list() }
k <- within(k, {

  PartyTags <- c('Bloc', 'Conservative', 'Green', 'Liberal', 'NDP')

  PartyNames <- c("Bloc Québécois", "Conservative Party of Canada",
      "Green Party of Canada", "Liberal Party of Canada", "New Democratic Party")

  FileNamePrefix <- c('Bloc Québécois', 'Conservative Party', 'Green Party',
      'Liberal Party', 'New Democratic Party')

  PartyLabels <- data.frame(name=PartyNames, filePrefix=FileNamePrefix, row.names=PartyTags, stringsAsFactors=FALSE)

  AllContribYears <- as.character(c(2004:2014))

  ProviceLevels <-
    c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT", NA)

  AllDataFileName <- "all_contributions.csv"

  dataPath <- "../data"
})

# Inline validation

if(!exists("validate")) { validate <- list() }
validate <- within(validate, {

  Base <- function(clause, errorMsg) {
    if(clause) {
      return(TRUE)
    } else {
      logg$SummaryInlineValidationError(errorMsg)
      stop(errorMsg)
    }
  }

  IsNotNA <- function(vec, errogMsg) {
    Base(!any(is.na(vec)), errogMsg)
  }

  AllSubsetRowsAccountedFor <- function(setRowCount, sourceRowCounts) {
    subsetRowCount <- sum(sourceRowCounts)
    Base(setRowCount == subsetRowCount,
      errorMsgs$UnaccountedForSubsetRows(subsetRowCount, setRowCount))
  }
})

# Error Messages

if(!exists("errorMsgs")) { errorMsgs <- list() }
errorMsgs <- within(errorMsgs, {

  UnaccountedForSubsetRows <- function(subsetN, dataSetN) {
    paste("There's a discrepancy between the data set and total subset row counts.\n",
          "All the subset rows summed:", subsetN, "\n",
          "Data set row count:", dataSetN, "\n",
          "Difference:", abs(subsetN - dataSetN)
    )
  }
})

# Logging...

library(futile.logger, quietly=TRUE, warn.conflicts=FALSE)

if(!exists("logg")) { logg <- list() }
logg <- within(logg, {

  SummaryInfo <- function(msg, ...) {
    flog.info(msg, ..., name="data.summary")
    flog.info(msg, ...)
    return(NULL)
  }

  SummaryInlineValidationError <- function(msg, ...) {
    flog.fatal(msg, ..., name="data.summary")
    return(NULL)
  }

  SummaryFile <- function() {
    if(is.null(logg$summaryFile)) {
      logg$summaryFile <<- paste(util$OutputPath(), "munger.log", sep = '/')
    }
    return(logg$summaryFile)
  }
})

flog.appender(appender.file(logg$SummaryFile()), name="data.summary")

# Unit Test Helpers

if(!exists("test")) { test <- list() }
test <- within(test, {

  running <- FALSE

  Text <- function(txt) {
    if(test$running) { print(txt) }
  }
})
