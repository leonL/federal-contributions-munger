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

  AllContribYears <- as.character(c(2004:2015))

  ProviceLevels <-
    c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT", NA)

  AllDataFileName <- "all_contributions.csv"

  dataPath <- "../data"

  SourcePath <- function() {
    if(is.null(k$sourcePath)) {
      k$sourcePath <<- paste(k$dataPath, 'source', sep = '/')
    } else { test$Text('cache') }
    return(k$sourcePath)
  }

  OutputPath <- function() {
    if(is.null(k$outputPath)) {
      k$outputPath <<- paste(k$dataPath, 'output', sep = '/')
    } else { test$Text('cache') }
    return(k$outputPath)
  }

  ContribsSrcPath <- function() {
    if(is.null(k$contribsSrcPath)) {
      k$contribsSrcPath <<- paste(SourcePath(), 'contributions', sep = '/')
    } else { test$Text('cache') }
    return(k$contribsSrcPath)
  }
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
})

# Logging...

library(futile.logger, quietly=TRUE, warn.conflicts=FALSE)

if(!exists("logg")) { logg <- list() }
logg <- within(logg, {

  summaryFile <- paste(k$OutputPath(), "data_summary.log", sep = '/')

  SummaryInfo <- function(msg, ...) {
    flog.info(msg, ..., name="data.summary")
    return(NULL)
  }

  SummaryInlineValidationError <- function(msg, ...) {
    flog.fatal(msg, ..., name="data.summary")
    return(NULL)
  }
})

flog.appender(appender.file(logg$summaryFile), name="data.summary")

# Unit Test Helpers

if(!exists("test")) { test <- list() }
test <- within(test, {

  running <- FALSE

  Text <- function(txt) {
    if(test$running) { print(txt) }
  }
})
