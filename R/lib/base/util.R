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