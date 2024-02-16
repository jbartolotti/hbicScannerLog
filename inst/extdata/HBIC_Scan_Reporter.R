library(hbicScanLog)

# Get the arguments passed to this script from the command line
args <- commandArgs(trailingOnly = TRUE)

# Assign the arguments to variables
basedir <- args[1]
ilabdir <- args[2]
xnatdir_backup <- args[3]
xnatdir_prearch <- args[4]
xnatdir_arch <- args[5]
reportdir <- args[6]
figuredir <- args[7]
admin_email <- args[8]
do_xnatlog <- FALSE

config <- load_config('default')
# Override default configuration as below:
#myconfig <- list(
#  setting = value,
#  setting2 = value2)
#config <- load_config(myconfig)

# Get the filename of the most recent ilab report that covers a time range
# not in any completed reports. If up to date, return 'uptodate'
ilabfile <- checkNewiLabReport(ilabdir, reportdir)

if(class(ilabfile) == 'character'){
  #ilabfile is a note. Exit.
  if(ilabfile %in% c('noiLab','uptodate')){
    UTILS.cleanup(ilabdir, admin_email, ilabfile)
  } else{

  #ilabfile is a filename. Start processing it.
    new_scanlog(ilabdir, ilabfile, list(backup = xnatdir_backup, prearch = xnatdir_prearch, arch = xnatdir_arch), reportdir, figuredir, config)

  }
} else {
  message(sprintf('ilabfile is not a character, it is a %s\nExiting.',class(ilabfile)))
  UTILS.cleanup(ilabdir, admin_email, 'ilabfile_unknown')

}

if(do_xnatlog){
  startdate <- as.POSIXct('2023-06-01')
  enddate <- as.POSIXct('2023-09-03')
xnat_dates <- list(start = as.numeric(startdate),
                   end = as.numeric(enddate))
xnat_filename_dates <- list(start = strftime(startdate, format = '%m%d%Y'),
                            end = strftime(enddate, format = '%m%d%Y'))
new_xnatlog(list(backup = xnatdir_backup, prearch = xnatdir_prearch, arch = xnatdir_arch),
            xnat_dates, xnat_filename_dates,
            file.path(reportdir,'xnat'), file.path(figure_path,'xnat'), config)

}
