

REPORT.hourFormat <- function(hours){
  if(length(hours) == 0 | any(is.na(hours))){
    return('0h0m')
  } else{
    hr <- floor(hours)
    mn <- floor((hours - hr)*100)/100

    if(hr > 0){
      tt <- sprintf('%sh %sm',hr,floor(mn*60))
    } else {
      tt <- sprintf('%sm',floor(mn*60))
    }
    return(tt)
  }
}

REPORT.message <- function(xd){
  mes <- list()
  for(i in 1:dim(xd)[1]){
xx <- xd[i,]
xx$path_folder_archORpre <- xx$path_folder_PREARCH
xx$path_folder_archORpre[!is.na(xx$path_folder_ARCH)] <- xx$path_folder_ARCH[!is.na(xx$path_folder_ARCH)]

#deltaminutes <- round(as.numeric(xx$lastImageDatetime_posix - xx$studDatetime_posix))
timerange <- sprintf('TIME: %s - %s, %s', strftime(xx$serDatetime_posix, format = '%H:%M'),
                     strftime(xx$lastImageDatetime_posix, format = '%H:%M'),
                     REPORT.hourFormat(xx$durationMinutes/60))
desc <- sprintf('STUDY: %s, %s', xx$studDesc, xx$patientName)
dicoms <- system2('dicomcount',xx$path_folder_archORpre, stdout = TRUE)

maxchar <- max(nchar(desc),nchar(timerange))
timerange <- paste(timerange, paste(rep(' ', maxchar-nchar(timerange)),collapse=''))
desc <- paste(desc, paste(rep(' ', maxchar-nchar(desc)),collapse=''))

header <- paste(rep('#',maxchar+4),collapse = '')
mes[[i]] <- c(header,
              sprintf('# %s#',timerange),
              sprintf('# %s#',desc),
              header,
              '',
              dicoms,
              '',
              '----------------------------------------------',
              ''
)


  }

  return(mes)

}
