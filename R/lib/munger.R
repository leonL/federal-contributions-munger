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

  InitializeColumns <- function(dataSet) {
    colnames(dataSet) <- k$RawDataColNames
    dataSet <- select(dataSet, -id, -city, -province)
    return(dataSet)
  }

  DoneeCols <- function(dataSet, partyTag, partyName) {
    within(dataSet, {
      party_riding <- str_trim(sub("/.+", "", party_riding))

      # generate 'federal_contribution' column
      donee.riding <- (party_riding != partyName)

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

  FilterUnviableRows <- function(dataSet) {
    dataSet <- munge$FilterEstateContributions(dataSet)
  }

  FilterEstateContributions <- function(dataSet, save=TRUE) {
    print("Filtering estate contributions...")
    estateBool <- grepl("estate", dataSet$donor.name, ignore.case = TRUE)

    estateContribs <- filter(dataSet, estateBool)
    if(save) {util$saveCsv(estateContribs, "estate_contributions.csv")}

    nonEstateContribs <- filter(dataSet, !estateBool)
    return(nonEstateContribs)
  }

})

# Inline validation

if(!exists("validate")) { validate <- list() }
validate <- within(validate, {

  Year <- function(year) {
    validate$Base(year %in% k$AllContribYears,
      paste("Invalid year:", year)
    )
  }

  AllRowsAccountedFor <- function(setRowCount, sourceRowCounts) {
    subsetRowCount <- sum(sourceRowCounts)
    validate$Base(setRowCount == subsetRowCount,
      errorMsgs$RowCount(subsetRowCount, setRowCount))
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
})