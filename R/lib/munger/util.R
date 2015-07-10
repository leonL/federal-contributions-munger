# Utility functions

if(!exists("util")) { util <- list() }
util <- within(util, {

  InitContribsDataSet <- function() {

    rowCounts <- data.frame()
    aggregateSet <- data.frame()

    for(partyTag in k$PartyTags) {
      for(year in k$AllContribYears) {

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
    src <- paste(ContribsSrcPath(), partyTag, fileName, sep = '/')

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

  MatchingPostivieContribIndices <- function(negativeContribs, dataSet) {
    matchContribSet <-
      if(!is.null(negativeContribs$contrib.date)) {
        select(negativeContribs, -contrib.date)
      } else {
        negativeContribs
      }

    matchContribSet$contrib.amount = abs(matchContribSet$contrib.amount)

    matchDataSet <- select(dataSet, -contrib.date) %>%
                      mutate(index = as.integer(row.names(.)))

    matches <- merge(matchDataSet, matchContribSet)
    matchesNoIndexCol <- select(matches, -index)
    uniqueMatches <- filter(matches, !duplicated(matchesNoIndexCol))

    indices <- as.vector(uniqueMatches$index)

    # any negativeContribs that seem like duplicates once the contrib.date
    # col has been removed need to be handled recursively because all their
    # positive matches will also be duplicates and so removed from the result
    if(anyDuplicated(matchContribSet)) {
      isDuplicate <- duplicated(matchContribSet)
      duplicateMatchContribs <- filter(matchContribSet, isDuplicate)
      dataSubSet <- dataSet[-indices, ]
      indices <- c(indices,
        util$MatchingPostivieContribIndices(duplicateMatchContribs, dataSubSet))
    }

    return(indices)
  }

  FinalContribOrderAndSort <- function(dataSet) {
    dataSet <- select(
      dataSet, party, target.riding_name, contrib.date, contrib.amount,
      donor.name, postal_code, contributor.riding_name, city, province,
      pcode.latitude, pcode.longitude, contrib.year, donee.riding_level,
      target.riding_id, contributor.riding_id
    ) %>% arrange(contrib.date)
    dataSet
  }
})