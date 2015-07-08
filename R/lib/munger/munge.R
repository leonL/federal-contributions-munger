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
    dataSet <- FilterOutInvalidPostalCodes(dataSet) %>%
                FilterOutFakePostalCodes() %>%
                  FilterOutEstateContributions()
    return(dataSet)
  }

  FilterOutInvalidPostalCodes <- function(dataSet, save.removedRows=TRUE) {
    flog.info("Filtering out invalid postal codes...")
    validCodes <- grepl(k$PostalCodeRegex, dataSet$postal_code)

    rowsWithInvalidPostal <- filter(dataSet, !validCodes)
    if(save.removedRows) {util$SaveCSV(rowsWithInvalidPostal, "unused_rows.invalid_pcodes.csv")}

    rowsWithValidPostal <- filter(dataSet, validCodes)
    return(rowsWithValidPostal)
  }

  FilterOutFakePostalCodes <- function(dataSet, save.removedRows=TRUE) {
    flog.info("Filtering out fake postal codes...")
    fakeCodes <- util$GetFakePostalVector()
    isFake <- dataSet$postal_code %in% fakeCodes

    rowsWithFakePostal <- filter(dataSet, isFake)
    if(save.removedRows) {util$SaveCSV(rowsWithFakePostal, "unused_rows.fake_pcodes.csv")}

    rowsWithRealPostalCodes <- filter(dataSet, !(isFake))
    return(rowsWithRealPostalCodes)
  }

  FilterOutEstateContributions <- function(dataSet, save.removedRows=TRUE) {
    flog.info("Filtering out estate contributions...")
    isEstate <- grepl("estate", dataSet$donor.name, ignore.case = TRUE)

    estateContribs <- filter(dataSet, isEstate)
    if(save.removedRows) {util$SaveCSV(estateContribs, "unused_rows.estate_contribs.csv")}

    nonEstateContribs <- filter(dataSet, !isEstate)
    return(nonEstateContribs)
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