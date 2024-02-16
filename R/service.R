
SERVICE.import_report <- function(filename){

  raw <- pdftools::pdf_data(filename)

  # go through all rows in the pdf's extracted text and combine words from the same text field together (i.e. ones separated by spaces)
  mylines = as.character()
  line = as.character()
  for(i in 1:dim(raw[[1]])[1]){

    # Append row to line. If there was already text on this line, insert a separating space.
    if(length(line)>0){mysep = ' '} else{mysep = ''}
    line = paste(line, raw[[1]]$text[i],sep = mysep)

    # If no space is present after this line, then insert it into mylines and start a fresh line
    if(!(raw[[1]]$space[i])){
      mylines = c(mylines,line)
      line = as.character()
    }

  }

  return(mylines)
}

SERVICE.import_report_asoneline <- function(filename){
  raw <- pdftools::pdf_data(filename)

  # go through all rows in the pdf's extracted text and combine words from the same text field together (i.e. ones separated by spaces)
  line = as.character()
  for(i in 1:dim(raw[[1]])[1]){

    # Append row to line. If there was already text on this line, insert a separating space.
    if(length(line)>0){mysep = ' '} else{mysep = ''}
    line = paste(line, raw[[1]]$text[i],sep = mysep)
  }

  return(line)

  }

folder <- '//kumc.edu/data/Research/Hoglund/Brooks_W/Skyra_QC/Siemens_Service_Reports'
folder <- '//kumc.edu/data/Research/Hoglund/Bartolotti_J/02_HBIC/qc/Siemens_Service_Reports'
folder <- 'Siemens_Service_Reports'
allpdfs <- dir(folder)
p <- allpdfs[1]

fields <- list(
  service_reason = list(
    field = 'Symptoms Reported / Reason for Service',
    datline = 1
  ),
  service_performed = list(
    field = 'Corrective Action / Service Performed',
    datline = 1
  ),
  datetime = list(
    field = 'SVC Arri Date/Time/Day',
    datline = 2
  ),
  cause_code = list(
    field = 'SVC Comp Date/Time/Day Effect Cause Code',
    datline = 7
    ),
  notes = list(
    field = 'Indicate system was properly functioning and include test & inspection data, if applicable',
    datline = 1,
    until = 'This is not an Invoice'
    )

  )

fields2 <- list(
  service_reason = list(
    field = 'Symptoms Reported / Reason for Service',
    until = 'Corrective Action / Service Performed'
  ),
  service_performed = list(
    field = 'Corrective Action / Service Performed',
    until = c('Repair Hours Breakdown', 'Indicate system was properly functioning')
  ),
  notes = list(
    field = 'Indicate system was properly functioning and include test & inspection data, if applicable',
    until = c('This is not an Invoice', 'Repair Hours Breakdown')
  )

)


alldat <- list()
index <- 0
for(p in allpdfs){
  spl <- strsplit(p,'~')[[1]]
  rawdate <- spl[1]
  date <- as.POSIXct(rawdate, format = 'Date-%Y-%m-%d-%H%M%S', timezone = 'America/Chicago')
  epoch <- as.numeric(date)
  id <- strsplit(spl[2],'[.]')[[1]][1]

  index <- index+1
  #pd <- SERVICE.import_report(file.path(folder,p))
  pdd <- SERVICE.import_report_asoneline(file.path(folder,p))

  alldat[[index]] <- data.frame(rawdate = rawdate, date = date, epoch = epoch, id = id)
  alldat[[index]][,names(fields2)] <- NA


#  for(f in names(fields)){
#    ff <- fields[[f]]
#
#    startline <- which(pd == ff$field) + ff$datline
#    if ('until' %in% names(ff)){
#      endline <- which(pd == ff$until) -1
#    } else{
#      endline <- startline
#    }
#    if(endline != startline){
#      val <- paste(pd[startline:endline],collapse = ' ')
#    } else{
#      val <- pd[[startline]]
#
#    }
#    alldat[[index]][1,f] <- val
#  }
  for(f in names(fields2)){
    ff <- fields2[[f]]
    start <- stringr::str_locate(pdd,ff$field)

    end <- 999999999
    for(uu in ff$until){
      ee <- stringr::str_locate(pdd,uu)
      end <- min(end,ee[1])
    }
    val <- trimws(substr(pdd,start[2]+1,end[1]-1))
    alldat[[index]][1,f] <- val
  }


}
dat <- do.call('rbind',alldat)
readr::write_delim(dat, 'service_reports.txt', delim = '\t')

