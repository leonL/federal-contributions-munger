# load dependencies

library(plyr, quietly=TRUE, warn.conflicts=FALSE)
library(dplyr, quietly=TRUE, warn.conflicts=FALSE)

# Constants

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

  SourcePath <- "../data/source"
  ContribsSrcPath <- paste(SourcePath, "contributions", sep='/')

  OutputPath <- "../data/output"

  AllDataFileName <- "all_contributions.csv"
})

# Utility functions

if(!exists("util")) { util <- list() }
util <- within(util, {

  ReadSrcCSV <- function(filename, subfolder) {
    if(test$running) { subfolder <- paste(subfolder, 'fixtures', sep = "/")}
    file <- paste(k$SourcePath, subfolder, filename, sep = '/')
    print(paste("Reading", file, "..."))
    csv <- read.csv(file, as.is=TRUE, encoding="UTF-8")
    return(csv)
  }

  ReadPostalCodeSrcCSV <- function(filename) {
    subfolder <- 'postal_codes'
    ReadSrcCSV(filename, subfolder)
  }

  ReadRidingSrcCSV <- function(filename) {
    ReadSrcCSV(filename, "ridings")
  }

  SaveCSV <- function(data, filename=k$AllDataFileName) {
    file <- paste(k$OutputPath, filename, sep = '/')
    print(paste("Writing", file, "..."))
    write.csv(data, file=file, row.names=FALSE)
  }

  FormatNum <- function(n) {
    format(n, big.mark = ',')
  }

  GetOfficialPartyName <- function(partyTag) {
    k$PartyLabels[partyTag, 'name']
  }

  GetContributionsFileNamePrefix <- function(partyTag) {
    k$PartyLabels[partyTag, 'filePrefix']
  }

  TitleCase <- function(str) {
    gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", str, perl=TRUE)
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

  summaryFile <- paste(k$OutputPath, "data_summary.log", sep = '/')

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

  if(!exists("running")) { running <- FALSE }

  Text <- function(txt) {
    if(running) { print(txt) }
  }

})
