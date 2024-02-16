
PROCESS.compare.ilab.xnat <- function(ilab_data, xnat_data, time_threshold_minutes, report_path, filename_dates){



  ilab_data$xnat_closest_row <- 0
  ilab_data$xnat_closest_timedelta_minutes <- 0
  ilab_data$time_threshold_minutes <- time_threshold_minutes
  ilab_data$xnat_row <- NA

  for(i in 1:dim(ilab_data)[1]){
    lin <- ilab_data[i,]
    tdelt <- abs(lin$epoch - xnat_data$epochStud)
    ilab_data$xnat_closest_row[i] <- which(tdelt == min(tdelt, na.rm = TRUE))
    ilab_data$xnat_closest_timedelta_minutes[i] <- min(tdelt, na.rm = TRUE)/60

  }
  subthresh <- ilab_data$xnat_closest_timedelta_minutes <= ilab_data$time_threshold_minutes
  ilab_data$xnat_row[subthresh] <- ilab_data$xnat_closest_row[subthresh]

  ix_data <- merge(ilab_data, xnat_data, by = 'xnat_row', all.x = TRUE)

  ix_data$purchase_datetime_posix <- as.POSIXct(ix_data$epoch, origin = '1970-01-01')
  ix_data$purchase_date_time <- strftime(ix_data$purchase_datetime_posix, format = '%I:%M %p')
  ix_data$purchase_date_day <- strftime(ix_data$purchase_datetime_posix, format = '%Y-%m-%d')
  ix_data$purchase_date_day_posix <- as.POSIXct(ix_data$purchase_date_day)
  ix_data$purchase_date_dayweeknum <- strftime(ix_data$purchase_date_day_posix, format = '%u')

  ix_data$studDate_time_format <- strftime(as.POSIXct(ix_data$studDatetime,format = '%Y%m%d_%H%M%S'), format = '%I:%M %p')

  ix_data$study_id <- unlist(lapply(ix_data$patientComments, function(x){
    sid <- trimws(strsplit(x,':')[[1]][2])
    if(is.na(sid)){sid == 'NA'}
    return(sid)
  }))

  ix_data$time <- paste(ix_data$purchase_date_time, ix_data$studDate_time_format, sep = ' / ')

  ix_data <- ix_data[order(ix_data$epoch),]

  weekly_summary <- ix_data[c('purchase_date_dayweeknum','study_id','time','usage_type')]
  colnames(weekly_summary)[1] <- 'day'





  saveRDS(ix_data, file = file.path(report_path, sprintf('%s_%s_ilabxnat_merge.rds',filename_dates$start, filename_dates$end)))
  saveRDS(weekly_summary, file = file.path(report_path, sprintf('%s_%s_ilabxnat_merge_weeksummary.rds',filename_dates$start, filename_dates$end)))


  return(list(ix_data = ix_data,
              weekly_summary = weekly_summary))

}
