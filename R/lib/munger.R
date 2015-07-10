source('base.R')
source('munger/munge.R')
source('munger/util.R')

library(stringr, quietly=TRUE, warn.conflicts=FALSE)
library(dplyr, quietly=TRUE, warn.conflicts=FALSE)

# Constants

if(!exists("k")) { k <- list() }
k <- within(k, {

  RawContributionsDataColNames <-
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

# Validations ... for validating the success of SENSITIVE data transformations
# in production so that badly corrupted data does not slip through (more general
# transformations are validated by unit tests)

if(!exists("validate")) { validate <- list() }
validate <- within(validate, {

  AllSubsetRowsAccountedFor <- function(setRowCount, sourceRowCounts) {
    subsetRowCount <- sum(sourceRowCounts)
    Base(setRowCount == subsetRowCount,
      errorMsgs$UnaccountedForSubsetRows(subsetRowCount, setRowCount))
  }

  AllRidingsNormalized <- function(ridingIds) {
    nRowsNotNormalized <- length(which(is.na(ridingIds)))
    IsNotNA(ridingIds, errorMsgs$UnknownRidings(nRowsNotNormalized))
  }

  AllPostalCodesMerged <- function(data, mergedData) {
    Base(nrow(data) == nrow(mergedData), "Postal Code Concordance merger dropped some rows!")
    IsNotNA(mergedData$contributor.riding_id, errorMsgs$UnknownPostalCodes())
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

  UnknownRidings <- function(nRowsNotNormalized) {
    paste("There was a problem normalizing the target riding names.\n",
          nRowsNotNormalized, "were not normalized."
    )
  }

  UnknownPostalCodes <- function() {
    paste("The Postal Code Concordance merge didn't resolve every postal code.",
          "Try running resolve_unknown_postal_code.rb")
  }
})

# Logging

if(!exists("logg")) { logg <- list() }
logg <- within(logg, {

  UnusableRows <- function(reason, nRows) {
    SummaryInfo("Filtered out %s unsuable rows: %s", util$FormatNum(nRows), reason)
  }

})