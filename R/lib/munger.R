source('base.R')

library(stringr, quietly=TRUE, warn.conflicts=FALSE)
library(dplyr, quietly=TRUE, warn.conflicts=FALSE)

# Constants

if(!exists("k")) { k <- list() }
k <- within(k, {
  RawDataColNames <-
    c("party_riding", "id", "donor.name", "contrib.date",
      "contrib.amount", "city", "province", "postal_code")
  PostalCodeConcordanceColNames <-
    c("postal_code", "contributor.riding_id", "contributor.riding_name",
      "pcode.latitude", "pcode.longitude", "city", "province")
  RidingConcordanceColNames <-
    c("party_riding", "target.riding_name", "target.riding_id")
  PostalCodeRegex <-
    "^[ABCEGHJKLMNPRSTVXY]{1}[[:digit:]]{1}[ABCEGHJKLMNPRSTVWXYZ]{1}[[:digit:]]{1}[ABCEGHJKLMNPRSTVWXYZ]{1}[[:digit:]]{1}$"
})

# Munging functions

if(!exists("munge")) { munge <- list() }
munge <- within(munge, {

  InitializeRawDataCols <- function(dataSet) {
    colnames(dataSet) <- k$RawDataColNames
    dataSet <- select(dataSet, -id, -city, -province)
    return(dataSet)
  }

  DoneeCols <- function(dataSet, partyTag, partyName) {
    within(dataSet, {
      party_riding <- str_trim(sub("/.+", "", party_riding))

      # generate 'federal_contribution' column
      donee.riding_level <- (party_riding != partyName)

      # generate 'party' column
      party <- partyTag
    })
  }

  DateCols <- function(dataSet, currentYear) {
    dataSet <- within(dataSet, {
      contrib.date <- as.Date(str_trim(contrib.date), format="%b %d, %Y")
      contrib.month.day <- strftime(contrib.date, "%m-%d")
      contrib.year <- currentYear
    })
    dataSet <- select(dataSet, -contrib.date)
    return(dataSet)
  }

  DonorNames <- function(dNames) {
    dNames <- str_trim(dNames) %>% tolower() %>% util$TitleCase()
    dNames[dNames == ""] <- NA
    return(dNames)
  }

  PostalCodes <- function(pCodes) {
    # remove hyphens and whitespace
    toupper(pCodes) %>% gsub("(-|\\s)","", .)
  }

  ContribAmounts <- function(amounts) {
    # express in dollars (raw values are expressed in cents)
    amounts / 100
  }

  FilterOutUnusableRows <- function(dataSet) {
    dataSet <- munge$FilterOutInvalidPostalCodes(dataSet) %>%
                munge$FilterOutEstateContributions()
    return(dataSet)
  }

  # FilterOutInvalidAmounts <- function(dataSet, save.removeRows) {

  # }

  FilterOutInvalidPostalCodes <- function(dataSet, save.removedRows=TRUE) {
    flog.info("Filtering out invalid postal codes...")
    validCodes <- grepl(k$PostalCodeRegex, dataSet$postal_code)

    rowsWithInvalidPostal <- filter(dataSet, !validCodes)
    if(save.removedRows) {util$SaveCSV(rowsWithInvalidPostal, "unused_rows.invalid_pcodes.csv")}

    rowsWithValidPostal <- filter(dataSet, validCodes)
    return(rowsWithValidPostal)
  }

  FilterOutEstateContributions <- function(dataSet, save.removedRows=TRUE) {
    flog.info("Filtering out estate contributions...")
    isEstate <- grepl("estate", dataSet$donor.name, ignore.case = TRUE)

    estateContribs <- filter(dataSet, isEstate)
    if(save.removedRows) {util$SaveCSV(estateContribs, "unused_rows.estate_contribs.csv")}

    nonEstateContribs <- filter(dataSet, !isEstate)
    return(nonEstateContribs)
  }

})

# Utility functions

if(!exists("util")) { util <- list() }
util <- within(util, {

  AggreateSrcData <- function() {

    rowCounts <- data.frame()
    aggregateSet <- data.frame()

    a_ply(k$AllPartyLabels, 1, function(partyLabels) {
      for(year in k$AllContribYears) {

        subset <- util$ReadAndFormatPartyYearSubset(partyLabels, year)

        rowCounts <<- rbind(rowCounts,
          data.frame(party_nickname=partyLabels['tag'], year=year, nrow=nrow(subset)))

        aggregateSet <<- rbind(aggregateSet, subset)
      }
    })

    # stop execuition if the compiled data set does not contain all the source csv rows
    validate$AllRowsAccountedFor(nrow(aggregateSet), rowCounts$n)

    logg$SummaryInfo(
      "%s records sourced in all", util$FormatNum(nrow(aggregateSet)))

    return(aggregateSet)
  }

  TitleCase <- function(str) {
    gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", str, perl=TRUE)
  }

  GetRidingConcordSet <- function() {
    set <- util$ReadConcordanceCSV("patry_to_official_riding_name_concordance.csv")
    colnames(set) <- k$RidingConcordanceColNames
    return(set)
  }

  GetPostalConcordSet <- function() {
    set <- util$ReadConcordanceCSV("postal_code_riding_geo_concordance.csv")
    colnames(set) <- k$PostalCodeConcordanceColNames
    return(set)
  }

  ReadAndFormatPartyYearSubset <- function(partyIds, year) {
    partyTag <- as.character(partyIds[['tag']])
    partyName <- as.character(partyIds[['name']])
    filePrefix <- as.character(partyIds[['filePrefix']])

    flog.info("Reading and formating donations to %s %s", partyName, year)
    fileName <- paste(filePrefix, year, 'csv', sep = '.')
    src <- paste(k$SourcePath, partyTag, fileName, sep = '/')
    subset <- read.csv(src, header=FALSE, as.is=TRUE, encoding="UTF-8")

    logg$SummaryInfo(
      "%s records sourced from %s", util$FormatNum(nrow(subset)), fileName)


    # formatting....
    subset <- munge$InitializeRawDataCols(subset) %>%
                munge$DoneeCols(partyTag, partyName) %>%
                  munge$DateCols(year)

    return(subset)
  }

})

# Inline validation

if(!exists("validate")) { validate <- list() }
validate <- within(validate, {

  AllRowsAccountedFor <- function(setRowCount, sourceRowCounts) {
    subsetRowCount <- sum(sourceRowCounts)
    Base(setRowCount == subsetRowCount,
      errorMsgs$RowCount(subsetRowCount, setRowCount))
  }

  AllRidingsNormalized <- function(ridingIds) {
    nRowsNotNormalized <- length(which(is.na(ridingIds)))
    IsNotNA(ridingIds, errorMsgs$UnknownRidings(nRowsNotNormalized))
  }
})

# Error Messages

if(!exists("errorMsgs")) { errorMsgs <- list() }
errorMsgs <- within(errorMsgs, {

  RowCount <- function(subsetN, dataSetN) {
    paste("There's a discrepancy between the data set and subset row counts.\n",
          "All the subset rows summed:", subsetN, "\n",
          "Data set row count:", dataSetN, "\n",
          "Difference:", abs(subsetN - dataSetN)
    )
  }

  UnknownRidings <- function(nRowsNotNormalized) {
    paste("There was a problem normalizing the target riding names.\n",
          nRowsNotNormalized, "were not normalized."
    )
  }
})