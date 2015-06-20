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

# Utility functions

if(!exists("util")) { util <- list() }
util <- within(util, {

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

  NameCol <- function(dataSet) {
    within(dataSet, {
      donor.name <- str_trim(donor.name) %>% tolower() %>% util$TitleCase()
      donor.name[donor.name == ""] <- NA
    })
  }

  PostalCodeCol <- function(dataSet) {
    within(dataSet, {
      # remove hyphens and whitespace
      postal_code <- toupper(postal_code) %>% gsub("(-|\\s)","", .)
      postal_code[postal_code == ""] <- NA
    })
  }

  FilterUnviableRows <- function(dataSet) {
    dataSet <- munge$FilterEstateContributions(dataSet)
  }

  FilterEstateContributions <- function(dataSet, saveCSV=TRUE) {
    print("Filtering estate contributions...")
    estateBool <- grepl("estate", dataSet$donor.name, ignore.case = TRUE)

    estateContribs <- filter(dataSet, estateBool)
    if(saveCSV) {util$SaveCSV(estateContribs, "estate_contributions.csv")}

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