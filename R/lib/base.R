# Constants

if(!exists("k")) { k <- list() }
k <- within(k, {
  PartyNicknames <- c('Bloc', 'Conservative', 'Green', 'Liberal', 'NDP')
  PartyFullNames <- c("Bloc Québécois", "Conservative Party of Canada",
      "Green Party of Canada", "Liberal Party of Canada", "New Democratic Party")
  PartyNames <- data.frame(name=PartyFullNames, nick_name=PartyNicknames)

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