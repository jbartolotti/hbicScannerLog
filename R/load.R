

# scan directory for csv files with DATE_DATE_filename.csv format. Convert the dates to epochs and return all in a data frame.

LOAD.getReportDates <- function(repdir){
  # Get the contents of the report dir
  myfiles <- list.files(repdir, pattern = "\\.csv$")

  #If there are no report files, return an error message
  if(length(myfiles) == 0){return('none')}

  # Initialize an empty dataframe for report dates
  rep_dates <- data.frame()
  # Loop through each file
  for (ff in myfiles) {
    # Extract the startdate and enddate from the filename
    # The filename format is MMDDYYYY_MMDDYYYY_filename.csv
    file_split <- strsplit(ff, '_')[[1]]
    startdate <- file_split[1]
    enddate <- file_split[2]

    # Convert the startdate and enddate to epochs
    # The epoch is the number of seconds since 1970-01-01 00:00:00 UTC
    startepoch <- as.numeric(as.POSIXct(paste0(startdate, " 00:00:00"), format = "%m%d%Y %H:%M:%S", tz = "CST"))
    endepoch <- as.numeric(as.POSIXct(paste0(enddate, " 23:59:59"), format = "%m%d%Y %H:%M:%S", tz = "CST"))

    # Add a row to the ilab_dates dataframe with the filename, startepoch, and endepoch
    rep_dates <- rbind(rep_dates, data.frame(filename = ff, startepoch = startepoch, endepoch = endepoch))
  }
  return(rep_dates)

}


