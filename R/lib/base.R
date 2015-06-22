# Constants

if(!exists("k")) { k <- list() }
k <- within(k, {

  PartyTags <- c('Bloc', 'Conservative', 'Green', 'Liberal', 'NDP')
  PartyNames <- c("Bloc Québécois", "Conservative Party of Canada",
      "Green Party of Canada", "Liberal Party of Canada", "New Democratic Party")
  FilePrefix <- c('Bloc Québécois', 'Conservative Party', 'Green Party', 'Liberal Party', 'New Democratic Party')
  AllPartyLabels <- data.frame(name=PartyNames, tag=PartyTags, filePrefix=FilePrefix)

  AllContribYears <- as.character(c(2004:2015))

  ProviceLevels <-
    c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT", NA)

  SourcePath <- "../data/source"
  OutputPath <- "../data/output"

  AllDataFileName <- "all_contributions.csv"
})

# Utility functions

if(!exists("util")) { util <- list() }
util <- within(util, {

  SaveCSV <- function(data, filename=k$AllDataFileName) {
    file <- paste(k$OutputPath, filename, sep = '/')
    print(paste("Writing", file, "..."))
    write.csv(data, file=file, row.names=FALSE)
  }

  ReadConcordanceCSV <- function(filename) {
    file <- paste(k$SourcePath, "concordances", filename, sep = '/')
    print(paste("Reading", file, "..."))
    csv <- read.csv(file, as.is=TRUE, encoding="UTF-8")
    return(csv)
  }

  FormatNum <- function(n) {
    format(n, big.mark = ',')
  }

})

# Inline validation

if(!exists("validate")) { validate <- list() }
validate <- within(validate, {

  Base <- function(clause, errorMsg) {
    if(clause) {
      return(TRUE)
    } else {
      stop(errorMsg)
    }
  }

  IsNotNA <- function(vec, errogMsg) {
    Base(!any(is.na(vec)), errogMsg)
  }
})

# Logging...

library(futile.logger, quietly=TRUE, warn.conflicts=FALSE)

if(!exists("loggin")) { loggin <- list() }
loggin <- within(loggin, {

  summaryFile <- paste(k$OutputPath, "data_summary.log", sep = '/')

  SummaryInfo <- function(msg, ...) {
    flog.info(msg, ..., name="data.summary")
    return(NULL)
  }

})

flog.appender(appender.file(loggin$summaryFile), name="data.summary")