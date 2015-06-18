# Constants

if(!exists("k")) { k <- list() }
k <- within(k, {
  PartyTags <- c('Bloc', 'Conservative', 'Green', 'Liberal', 'NDP')
  PartyNames <- c("Bloc Québécois", "Conservative Party of Canada",
      "Green Party of Canada", "Liberal Party of Canada", "New Democratic Party")
  AllParties <- data.frame(name=PartyNames, tag=PartyTags)

  AllContribYears <- as.character(c(2004:2015))

  ProviceLevels <-
    c("AB", "BC", "MB", "NB", "NL", "NS", "NT", "NU", "ON", "PE", "QC", "SK", "YT", NA)

  SourcePath <- "../data/source"
  OutputPath <- "../data/output"

  AllDataFileName <- "all_contributions.csv"
})

# Inline Teststest

if(!exists("test")) { test <- list() }
test <- within(test, {

  Base <- function(clause, errorMsg) {
    if(clause) {
      return(TRUE)
    } else {
      stop(errorMsg)
    }
  }
})