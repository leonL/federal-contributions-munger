source('lib/munger.R', chdir=TRUE)

dataSet <- data.frame()
log <- data.frame()

for(subfolder in k$PartyNicknames) {
  src <- paste(k$SourcePath, subfolder, sep = '/')
  srcFiles <- list.files(src)

  print(paste("Reading all files in", src))
  for(file in srcFiles) {
    print(file)

    currentYear <- strsplit(file, ".", fixed=TRUE)[[1]][2]
    test$ValidateYear(currentYear)

    csv <- read.csv(
      paste(src, file, sep = '/'), header=FALSE, as.is=TRUE, encoding="UTF-8"
    )

    # generate a 'party_name' column and a 'federal_contribution' boolean column
    # colnames(csv) <- raw_data_column_names
    # csv <- generate_federal_cols(csv, subfolder)
    # csv <- adjust_errant_dates(csv, currentYear)

    log <- rbind(log,
      data.frame(party_nickname=subfolder, year=currentYear, nrow=nrow(csv)))

    dataSet <- rbind(dataSet, csv)
  }
}

test$AllRowsAccountedFor(subsetN = sum(log$n), dataSetN = nrow(dataSet))