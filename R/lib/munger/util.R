# Utility functions

if(!exists("util")) { util <- list() }
util <- within(util, {

  InitContribsDataSet <- function() {

    rowCounts <- data.frame()
    aggregateSet <- data.frame()

    for(partyTag in k$PartyTags) {
      for(year in k$AllContribYears) {

        flog.info("Initializing contributions made to the %s in %s.", partyTag, year)

        subset <- util$GetContribsSubset(partyTag, year) %>%
                    FormatContributionsSubset(partyTag, year)

        rowCounts <- rbind(rowCounts,
          data.frame(party_nickname=partyTag, year=year, nrow=nrow(subset)))

        aggregateSet <- rbind(aggregateSet, subset)
      }
    }

    # stop execuition if the compiled data set does not contain all the source csv rows
    validate$AllSubsetRowsAccountedFor(nrow(aggregateSet), rowCounts$n)

    logg$SummaryInfo(
      "%s records sourced in all", util$FormatNum(nrow(aggregateSet)))

    return(aggregateSet)
  }

  FormatContributionsSubset <- function(subset, partyTag, year) {
    partyName <- GetOfficialPartyName(partyTag)
    subset <- munge$InitializeRawContribsDataCols(subset) %>%
                munge$DoneeCols(partyTag, partyName) %>%
                  munge$DateCols(year)
    return(subset)
  }

  GetContribsSubset <- function(partyTag, year) {
    fileNamePrefix <- GetContributionsFileNamePrefix(partyTag)

    fileName <- paste(fileNamePrefix, year, 'csv', sep = '.')
    src <- paste(k$ContribsSrcPath(), partyTag, fileName, sep = '/')

    subset <- read.csv(src, header=FALSE, as.is=TRUE, encoding="UTF-8")

    logg$SummaryInfo(
      "Read %s records from %s", util$FormatNum(nrow(subset)), fileName)

    return(subset)
  }

  GetRidingConcordSet <- function() {
    set <- util$ReadRidingSrcCSV("patry_to_official_riding_name_concordance.csv")
    colnames(set) <- k$RidingConcordanceColNames
    return(set)
  }

  GetFakePostalVector <- function() {
    set <- util$ReadPostalCodeSrcCSV("fake_postal_codes.csv")
    return(set$postal_code)
  }

  GetPostalConcordSet <- function() {
    if(!is.data.frame(util$postalCodeConcord)) {
      set <- util$ReadPostalCodeSrcCSV("postal_code_riding_geo_concordance.csv")
      colnames(set) <- k$PostalCodeConcordanceColNames
      util$postalCodeConcord <<- set
    } else { test$Text('cache') }
    return(util$postalCodeConcord)
  }

  GetDedupedPostalConcordSet <- function() {
    if(!is.data.frame(util$dedupedPostalCodeConcord)) {
      util$dedupedPostalCodeConcord <<-
        filter(GetPostalConcordSet(), !duplicated(postal_code))
    } else { test$Text('cache') }
    return(util$dedupedPostalCodeConcord)
  }
})