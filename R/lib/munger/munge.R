# Munging functions

if(!exists("munge")) { munge <- list() }
munge <- within(munge, {

  InitializeRawContribsDataCols <- function(dataSet) {
    colnames(dataSet) <- k$RawContributionsDataColNames
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
      correctedDate <- paste(currentYear, strftime(contrib.date, "%m-%d"), sep='-')
      contrib.date <- correctedDate
      contrib.year <- currentYear
    })
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
    dataSet <- FilterOutInvalidPostalCodes(dataSet) %>%
                FilterOutFakePostalCodes() %>%
                  FilterOutEstateContributions() %>%
                    FilterOutZeroValues() %>%
                      FilterOutNegativeContribs()
    return(dataSet)
  }

  FilterOutInvalidPostalCodes <- function(dataSet) {
    flog.info("Filtering out invalid postal codes...")
    validCodes <- grepl(k$PostalCodeRegex, dataSet$postal_code)

    rowsWithInvalidPostal <- filter(dataSet, !validCodes)
    util$SaveUnusableCSV(rowsWithInvalidPostal, "invalid_pcodes.csv")

    rowsWithValidPostal <- filter(dataSet, validCodes)
    return(rowsWithValidPostal)
  }

  FilterOutFakePostalCodes <- function(dataSet) {
    flog.info("Filtering out fake postal codes...")
    fakeCodes <- util$GetFakePostalVector()
    isFake <- dataSet$postal_code %in% fakeCodes

    rowsWithFakePostal <- filter(dataSet, isFake)
    util$SaveUnusableCSV(rowsWithFakePostal, "fake_pcodes.csv")

    rowsWithRealPostalCodes <- filter(dataSet, !(isFake))
    return(rowsWithRealPostalCodes)
  }

  FilterOutEstateContributions <- function(dataSet) {
    flog.info("Filtering out estate contributions...")
    isEstate <- grepl("estate", dataSet$donor.name, ignore.case = TRUE)

    estateContribs <- filter(dataSet, isEstate)
    util$SaveUnusableCSV(estateContribs, "estate_contribs.csv")

    nonEstateContribs <- filter(dataSet, !isEstate)
    return(nonEstateContribs)
  }

  FilterOutZeroValues <- function(dataSet) {
    flog.info("Filtering out contributions with an amount of $0...")
    isZero <- dataSet$contrib.amount == 0

    zeroContribs <- filter(dataSet, isZero)
    util$SaveUnusableCSV(zeroContribs, "zero_contribs.csv")

    nonZeroContribs <- filter(dataSet, !isZero)
    return(nonZeroContribs)
  }

  FilterOutNegativeContribs <- function(dataSet) {
    flog.info("Filtering out negative contributions and coresponding records...")
    negativeContribsIndex <- which(dataSet$contrib.amount < 0)
    negativeContribs <- dataSet[negativeContribsIndex, ]
    positiveMatchIndex <- util$MatchingPostivieContribIndices(negativeContribs, dataSet)
    filterIndex <- c(negativeContribsIndex, positiveMatchIndex) %>%
                      na.omit %>% as.integer

    filteredOutData <- dataSet[filterIndex, ]
    util$SaveUnusableCSV(filteredOutData, "negative_contribs.csv")

    filteredData <- dataSet[-filterIndex,]
    return(filteredData)
  }

  NormalizeRidingNames <- function(dataSet) {
    newSet <- merge(dataSet, util$GetRidingConcordSet(), all.x=TRUE)
    newSet <- select(newSet, -party_riding)
    ridingIds <- filter(newSet, donee.riding_level) %>% select(target.riding_id)
    validate$AllRidingsNormalized(ridingIds)
    return(newSet)
  }

  MergeWithPCodeConcordance <- function(dataSet) {
    flog.info(
      "Merging in postal code concordance cols where target and contributor ridings match..."
    )
    ridingSpecificMergeResult <- MergeWithPCodeConcordanceByRiding(dataSet)

    recordHasRidingSpecificConcord <-
      !is.na(ridingSpecificMergeResult$contributor.riding_name)

    ridingSpecificMergeSuccess <-
      filter(ridingSpecificMergeResult, recordHasRidingSpecificConcord)
    ridingSpecificMergeSuccess$contributor.riding_id <-
                                  ridingSpecificMergeSuccess$target.riding_id

    ridingSpecificMergeFailure <- filter(dataSet, !recordHasRidingSpecificConcord)
    flog.info(
      "Merging in postal code concordance cols for %s remaining records...",
       nrow(ridingSpecificMergeFailure)
    )
    dedupedMergeResut <-
      merge(ridingSpecificMergeFailure, util$GetDedupedPostalConcordSet())

    dataSetMerged <- rbind(ridingSpecificMergeSuccess, dedupedMergeResut)
    validate$AllPostalCodesMerged(dataSet, dataSetMerged)

    return(dataSetMerged)
  }

  MergeWithPCodeConcordanceByRiding <- function(dataSet) {
    merge(
      dataSet, util$GetPostalConcordSet(),
      by.x = c('postal_code', 'target.riding_id'),
      by.y = c('postal_code', 'contributor.riding_id'),
      all.x = TRUE
    )
  }

})