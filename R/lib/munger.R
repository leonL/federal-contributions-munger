source('base.R')

# Constants

if(!exists("k")) { k <- list() }
k <- within(k, {
  RawDataColNames <-
    c("party_riding", "id", "full_name", "contrib.date",
      "contrib.amount", "city", "province", "postal_code")
  PostalCodeConcordanceColNames <-
    c("postal_code", "contributor.riding_id", "contributor.riding_name",
      "pcode.latitude", "pcode.longitude", "city", "province")
  RidingConcordanceColNames <-
    c("party_riding", "target.riding_name", "target.riding_id")
  PostalCodeRegex <-
    "^[ABCEGHJKLMNPRSTVXY]{1}[[:digit:]]{1}[ABCEGHJKLMNPRSTVWXYZ]{1}[[:digit:]]{1}[ABCEGHJKLMNPRSTVWXYZ]{1}[[:digit:]]{1}$"
})

# Inline Tests

if(!exists("test")) { test <- list() }
test <- within(test, {

  ValidateYear <- function(year) {
    test$Base(year %in% k$AllContribYears,
      paste("Invalid year:", year)
    )
  }

  AllRowsAccountedFor <- function(subsetN, dataSetN) {
    test$Base(subsetN == dataSetN, errorMsgs$RowCount(subsetN, dataSetN))
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
