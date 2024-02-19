
#' @importFrom magrittr "%>%"

#' @export
initialize_hbicscanlog <- function(writedir = getwd()){
  # Create Study_Dicom_Filecounts.txt from the template file in the package.
  # Create the bash script that cron runs
  # Create a readme.txt file that tells you what to do with the files
  file.copy(
    system.file('extdata','README.txt', package = 'hbicScanLog'),
    file.path(writedir,'README.txt'))
  file.copy(
    system.file('extdata','HBIC_Scan_Reporter.sh', package = 'hbicScanLog'),
    file.path(writedir,'HBIC_Scan_Reporter.sh'))
  file.copy(
    system.file('extdata','Study_Dicom_Filecounts.txt', package = 'hbicScanLog'),
    file.path(writedir,'Study_Dicom_Filecounts.txt'))
}

#' This function is called by the HBIC_Scan_Reporter.sh script that is run as a cron job.
#' ilab path and filename are the location of the newly exported ilab report to process.
#' It will be scanned for scheduled scans since the last scanlog, and crossref with xnat.
#' config is a list with fields: admin_email and allowed_usage_type
#' @export
new_scanlog <- function(
  ilab_path,
  ilab_filename,
  xnat_paths,
  report_path,
  figure_path,
  config
){

  on.exit(UTILS.cleanup(ilab_path, config$admin_email),ilab_filename)

  ilab <- ILAB.process(ilab_path, ilab_filename, config$allowed_usage_types, config$usage_type_abbreviations, config$allowed_asset_types)

  xnat_data <- XNAT.process(xnat_paths, ilab$dates)


  saveRDS(ilab, file = file.path(report_path, sprintf('%s_%s_ilabdata.rds',ilab$filename_dates$start, ilab$filename_dates$end)))
  saveRDS(xnat_data$backup, file = file.path(report_path, sprintf('%s_%s_xnatdata_backup.rds',ilab$filename_dates$start, ilab$filename_dates$end)))
  saveRDS(xnat_data$arch, file = file.path(report_path, sprintf('%s_%s_xnatdata_arch.rds',ilab$filename_dates$start, ilab$filename_dates$end)))
  saveRDS(xnat_data$prearch, file = file.path(report_path, sprintf('%s_%s_xnatdata_prearch.rds',ilab$filename_dates$start, ilab$filename_dates$end)))
  saveRDS(xnat_data$all, file = file.path(report_path, sprintf('%s_%s_xnatdata_all.rds',ilab$filename_dates$start, ilab$filename_dates$end)))


  xnat_data$all$dicomcount_ARCH <- NA
  for(i in 1:dim(xnat_data$all)[1]){
    message(sprintf('%s of %s',i,dim(xnat_data$all)[1]))
    if(is.na(xnat_data$all$dicomcount_ARCH[i]) && !is.na(xnat_data$all$path_folder_ARCH)){
      xnat_data$all$dicomcount_ARCH[i] <- system2('dicomcount_quick', sprintf('%s',xnat_data$all$path_folder_ARCH[i]), stdout = TRUE)
    }
  }

  saveRDS(xnat_data$all, file = file.path(report_path, sprintf('%s_%s_xnatdata_all.rds',ilab$filename_dates$start, ilab$filename_dates$end)))


  #ix_list <- PROCESS.compare.ilab.xnat(ilab_data$data, xnat_data, config$time_threshold_minutes, report_path, ilab_data$filename_dates)

  figure_filenames <- FIGURES.weeklySummary(ilab$data, xnat_data, figure_path)
#  figure_filenames <- FIGURES.weeklySummary(ilab_data_all$data, xnat_data, figure_path)






  scanlog_report(ilab, xnat_data, file.path(figure_path, figure_filenames))

#  scanlog_report(ix_list$ix_data, ix_list$weekly_summary, report_path, ilab_data$filename_dates)


}

#' @export
xnat_yesterday <- function(xnat_paths, figure_path, email_list = '', admin_email = 'default'){
  yesterday <- Sys.Date()-1
  if(admin_email == 'default'){
    xnat_day(xnat_paths, yesterday, figure_path, email_list)
  } else {
    xnat_day(xnat_paths, yesterday, figure_path, email_list, admin_email)
  }
}

#' XNAT-only daily report. Day is date object like '1986-05-03'.
#' @export
xnat_day <- function(
  xnat_paths,
  day,
  figure_path,
  email_list = '',
  admin_email = 'jbartolotti2@kumc.edu'
){

  if(length(email_list) > 1){
    print_email_list <- sprintf('-c %s ',paste(email_list, collapse = ', '))

  } else {
      if(email_list != ''){
        print_email_list <- sprintf('-c %s', email_list)
      } else {
        print_email_list <- ''
      }
  }
  xnat_dates <- list(start = as.numeric(as.POSIXct(day)),
                     end = as.numeric(as.POSIXct(Sys.Date())))
  xnat_data <- XNAT.process(xnat_paths, xnat_dates)
  xnat_data_targetday <- xnat_data$all[xnat_data$all$date == day,]

  figname <- FIGURES.xnatDaySummary(xnat_data_targetday, figure_path)

  mes <- REPORT.message(xnat_data_targetday)

  mescat <- do.call('c',mes)

  report_title <- sprintf('XNAT Report: %s',strftime(day, format = '%A, %m-%d-%Y'))
  #try(sendmailR::sendmail(
#    'xnat_reporter@hbic-synapse2.kumc.edu',
 #   'jbartolotti2@kumc.edu',
  #  report_title,
   # msg = list(sendmailR::mime_part(c(report_title,'', mescat[1:4],'.')),
    #           sendmailR::mime_part(file.path(figure_path,figname), name = figname))
     #                    ),silent = TRUE)
body <- c(report_title,'', mescat)
system(paste0('echo "',paste(body,collapse='\n'),'" | mutt -e "my_hdr From: xnat_reporter@hbic-synapse2.kumc.edu" -e "set realname=XNAT_Reporter" -s "',report_title,'" ',admin_email,' ', print_email_list, '-a ',file.path(figure_path,figname)))
}


#' XNAT-only reporter
#' @export
new_xnatlog <- function(
  xnat_paths,
  xnat_dates,
  xnat_filename_dates,
  report_path,
  figure_path,
  config){

  xnat_data <- XNAT.process(xnat_paths, xnat_dates)
  saveRDS(xnat_data$backup, file = file.path(report_path, sprintf('%s_%s_xnatdata_backup.rds',xnat_filename_dates$start, xnat_filename_dates$end)))
  saveRDS(xnat_data$arch, file = file.path(report_path, sprintf('%s_%s_xnatdata_arch.rds',xnat_filename_dates$start, xnat_filename_dates$end)))
  saveRDS(xnat_data$prearch, file = file.path(report_path, sprintf('%s_%s_xnatdata_prearch.rds',xnat_filename_dates$start, xnat_filename_dates$end)))
  saveRDS(xnat_data$all, file = file.path(report_path, sprintf('%s_%s_xnatdata_all.rds',xnat_filename_dates$start, xnat_filename_dates$end)))

  FIGURES.xnatWeeklySummary(xnat_data$all, figure_path, xnat_dates$start)


  #/xnatdata/backup/{STUDY}/{20230101_HHMMSSmmm}/{patientName}or{patientID}/SCANS
  #the time is the time, I think, that the first dicom was received. Can substitute with paste0(studDatetime,'000') (i.e., blank milliseconds)
  #/xnatdata/arch/{STUDY}/arc001/{patientName}or{patientID}/SCANS

  XNAT.backupArchive(xnat_data$all, file.path(report_path,'backupArch.sh'), xnat_dates, xnat_filename_dates)
}

unassigned_size <- function(){

  pdir <- dir('/xnatdata/prearch')
  pdir <- pdir[grepl('^20',pdir)]
  bdir <- dir('/xnatdata/backup')
  inbackup <- pdir %in% bdir

  datmatch <- data.frame(folder = pdir, pcount = '', bcount = '', ismatch = FALSE, stringsAsFactors = FALSE)

  ismatch <- rep(FALSE, length(pdir))
  index <- 0
  for(p in pdir){
    index <- index+1
    message(sprintf('%s of %s',index, length(pdir)))
    datmatch$pcount[index] <- system2('dicomcount_quick',args = c(file.path('/xnatdata/prearch',p)), stdout = TRUE)
    datmatch$bcount[index] <- system2('dicomcount_quick',args = c(file.path('/xnatdata/backup',p)), stdout = TRUE)

  }

}




notafun <- function(){
  rmarkdown::render(system.file('extdata','report.Rmd', package = 'hbicScanLog'),
                    output_dir = getwd(),
                    output_file = 'test.html',
                    params = list(
                      ix_data = ix_data,
                      weekly_summary = weekly_summary
                      ))


  figure_filepaths <- figure_filenames
  rmarkdown::render(system.file('extdata','report.Rmd', package = 'hbicScanLog'),
                    output_dir = getwd(),
                    output_file = sprintf('%s_%s_HBIC_scanlog.html',ilab_data$filename_dates$start, ilab_data$filename_dates$end),
                    params = list(
                      ilab_data = ilab_data,
                      xnat_data = xnat_data,
                      figure_filepaths = figure_filepaths
                    ))

config$allowed_usage_types <- c(config$allowed_usage_types, "Research Scan", "Participant Bailed/Screen failure", "Cancelation after cut-off", "Technique Development (TD) Scan - No charge", "Research Scan2")




figure_filenames <- list()
figure_filenames[['2023-07-10']] <-  "ilab_xnat_schedule_20230710_20230714.png"
figure_filenames[['2023-07-03']] <- "ilab_xnat_schedule_20230703_20230707.png"
figure_filenames[['2023-07-17']] <- "ilab_xnat_schedule_20230717_20230721.png"
fdir <- 'C:/Users/j186b025/Documents/GitHub/jbartolotti/hbicScanLog'

rmarkdown::render(system.file('extdata','report.Rmd', package = 'hbicScanLog'),
                  output_dir = getwd(),
                  output_file = sprintf('%s_%s_HBIC_scanlog.html',ilab$filename_dates$start, ilab$filename_dates$end),
                  params = list(
                    ilab = ilab,
                    xnat_data = xnat_data,
                    figure_dir = fdir,
                    figure_filenames = figure_filenames
                  ))


  }

scanlog_report <- function(ilab_data, xnat_data, report_path, figure_filepaths){

  rmarkdown::render(system.file('extdata','report.Rmd', package = 'hbicScanLog'),
                    output_dir = report_path,
                    output_file = sprintf('%s_%s_HBIC_scanlog.html',ilab_data$filename_dates$start, ilab_data$filename_dates$end),
                    params = list(
                      ilab_data = ilab_data,
                      xnat_data = xnat_data,
                      figure_filepaths = figure_filepaths
                    ))


}


scanlog_report_old <- function(ix_data, weekly_summary, report_path, filename_dates){

  rmarkdown::render(system.file('extdata','report_old.Rmd', package = 'hbicScanLog'),
                    output_dir = report_path,
                    output_file = sprintf('%s_%s_HBIC_scanlog.html',filename_dates$start, filename_dates$end),
                    params = list(
                      ix_data = ix_data,
                      weekly_summary = weekly_summary
                      ))


  }


UTILS.cleanup <- function(ilabdir, admin_email, note){
  # delete the file PATH/.processing
  file.remove(file.path(ilabdir, '.processing'))

  if(note %in% c('noiLab','uptodate')){
    # No action needed. There were no ilabfiles in ilabdir,
    # or no ilab reports more recent than completed reports.


  } else if(note == 'ilabfile_unknown'){
    #send an email, the ilabfile returned was unknown. Not a character.
  } else{
    #ilab_filename
    # check for the completed report file.
    #   if it's not there, email an error
  }



}

#' Scan the ilab report directory and completed report directory for
#' DATE_DATE_filename.csv files. Return the filename of the most recent ilab file
#' with a daterange more recent than the completed reports. If we're up to date,
#' return 'none'
#'
#' @export
checkNewiLabReport <- function(ilabdir, reportdir){

  #Get all ilab report dates and time ranges
  ilab_dates <- LOAD.getReportDates(ilabdir)
  if(class(ilab_dates) == 'character' && ilab_dates == 'none'){
    return('noiLab')
  }
  # Find the maximum endepoch in ilab_dates
  ilab_max_endepoch <- max(ilab_dates$endepoch)
  ilab_latest_filename <- ilab_dates$filename[ilab_dates$endepoch == ilab_max_endepoch]


  # Get all finished report dates and time ranges
  report_dates <- LOAD.getReportDates(reportdir)
  if(class(report_dates) == 'character' && report_dates == 'none'){
    #If there are no completed reports, just return the most recent ilab report to process
    return(ilab_latest_filename)
  }
  # Find the maximum endepoch in report_dates
  report_max_endepoch <- max(report_dates$endepoch)

  # Determine whether the latest date range in ilabdir is more recent than any of the reports in reportdir
  # Compare the two maxima and return the ilab file or 'uptodate'
  if (ilab_max_endepoch > report_max_endepoch) {
    return(ilab_latest_filename)
  } else {
    return('uptodate')
  }

}

#####################

hbicscanlog.example <- function(){
  reportdir <- ilab_filename <- '03272023_03312023_charges_report_source_data_4637931040.csv'
  ilab_path <- file.path(reportdir,'iLab_Reports')
  ilab_filename <- 'ilabraw.csv'
  xnat_path <- '/xnatdata/backup'
  config <- list(
    admin_email = 'jbartolotti2@kumc.edu',
    allowed_usage_types = c('Full Research Study Scan',
                            'Clinical Trial NIH Rate 2',
                            'Cancelation - by Subject/Study Team - after cut-off',
                            "Cancelation - by Subject/Study Team - prior cut-off",
                            "Clinical Trial Scan",
                            "Cancelation - after cut-off_Clinical trial",
                            "DIAN-TU - NIH Rate",
                            "Clinical Trial Scan - NIH rate1",
                            "Pilot Study Scan",
                            "Full Study scan - FY19 rate",
                            "Subject No Show ",
                            "Other Clinical Scan")
    )

#  new_scanlog(ilab_path, ilab_filename, config) # EXCERPTED BELOW
    ilab_data <- ILAB.process(ilab_path, ilab_filename, config$allowed_usage_types)

    dates <- ilab_data$dates
   xnat_data <- XNAT.process(xnat_path, ilab_data$dates)
  # XNAT.process excerpted below

        # list all folders in xnat backup and their paths
        xnat_dat_raw <- XNAT.scanfolders(xnat_path)

        # convert to dates and whether it's in 'after the ilab start date'
        xnat_dat_raw <- XNAT.scanfolder_dates(xnat_dat_raw, dates)

        # trim to only entries more recent than the beginning of the ilab start date
        xnat_dat_all <- subset(xnat_dat_raw, ilab_range)
xnat_dat <- xnat_dat[1:10,] #for testing
      # add column for the first dicom of the first series
      all_firstdicom_fullpaths <- unlist(
        lapply(1:nrow(xnat_dat),
               function(i){ XNAT.get_firstdicom_fullpath(xnat_dat[i,])
               }))

      xnat_dat$firstdicom_fullpath <- all_firstdicom_fullpaths


      # return some time and description values from the dicom header

      dcmnames <- XNAT.dicominfo('names')

      for(i in 1:dim(xnat_dat)[1]){
        dcmdat <- XNAT.dicominfo(xnat_dat$firstdicom_fullpath[i])
        for(j in dcmnames){
          xnat_dat[i,j] <- dcmdat[j]
        }
      }

      return(xnat_dat)
}

