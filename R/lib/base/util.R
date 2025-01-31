# Utility functions

if(!exists("util")) { util <- list() }
util <- within(util, {

  SourcePath <- function() {
    if(is.null(k$sourcePath)) {
      k$sourcePath <<- paste(k$dataPath, 'source', sep = '/')
    }
    return(k$sourcePath)
  }

  OutputPath <- function() {
    if(is.null(k$outputPath)) {
      k$outputPath <<- paste(k$dataPath, 'output', sep = '/')
    }
    return(k$outputPath)
  }

  ContribsSrcPath <- function() {
    if(is.null(k$contribsSrcPath)) {
      k$contribsSrcPath <<- paste(SourcePath(), 'contributions', sep = '/')
    }
    return(k$contribsSrcPath)
  }

  ReadSrcCSV <- function(filename, subfolder) {
    file <- paste(SourcePath(), subfolder, filename, sep = '/')
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

  SaveCSV <- function(data, filename, subfolder="") {
    path <- paste(OutputPath(), subfolder, sep = '/')
    dir.create(path, showWarnings=FALSE)
    filepath <- paste(path, filename, sep='/')
    flog.info("Writing %s", filepath)
    write.csv(data, file=filepath, row.names=FALSE)
  }

  SaveUnusableCSV <- function(data, filename) {
    SaveCSV(data, filename, 'unusable_rows')
  }

  SaveContributionsCSV <- function(data, filename=k$AllDataFileName) {
    SaveCSV(data, filename, 'contributions')
    logg$SummaryInfo("Saved %s rows to %s", util$FormatNum(nrow(data)), filename)
  }

  SaveUnknownPostalCodesCSV <- function(codes) {
    file <- paste(SourcePath(), 'postal_codes/unknown_postal_codes.csv', sep = '/')
    flog.info("Writing %s", file)
    codes <- as.data.frame(codes)
    names(codes) <- 'postal_code'
    write.csv(codes, file=file, row.names=FALSE)
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