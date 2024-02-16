
XNAT.all <- function(xnat_data){

  islist <- colnames(xnat_data$backup)[unlist(lapply(colnames(xnat_data$backup), function(x){class(xnat_data$backup[[x]])=='list'}))]
  for(col in islist){
    xnat_data$backup[[col]] <- unlist(xnat_data$backup[[col]])
  }
  islist <- colnames(xnat_data$prearch)[unlist(lapply(colnames(xnat_data$prearch), function(x){class(xnat_data$prearch[[x]])=='list'}))]
  for(col in islist){
    xnat_data$prearch[[col]] <- unlist(xnat_data$prearch[[col]])
  }
  islist <- colnames(xnat_data$arch)[unlist(lapply(colnames(xnat_data$arch), function(x){class(xnat_data$arch[[x]])=='list'}))]
  for(col in islist){
    xnat_data$arch[[col]] <- unlist(xnat_data$arch[[col]])
  }
  xnat_data$arch$studDate <- unlist(xnat_data$arch$studDate)

  samecol <- c("date",
               "ilab_range",
               "studDate",
               "serDate",
               "acqDate",
               "studTime",
               "serTime",
               "acqTime",
               "studDesc",
               "modality",
               "patientName",
               "patientID",
               "patientComments",
               "imageTime",
               "studDesc_short",
               "acqDatetime",
               "acqDatetime_posix",
               "epochAcq",
               "studDatetime",
               "studDatetime_posix",
               "epochStud",
               "serDatetime",
               "serDatetime_posix",
               "epochSer",
               "lastImageDatetime",
               "lastImageDatetime_posix",
               "epochLastImage",
               "durationMinutes",
               "hourday",
               "daynum")

  unq <- !(colnames(xnat_data$backup) %in% samecol)
  colnames(xnat_data$backup)[unq] <- paste0(colnames(xnat_data$backup)[unq],'_BACKUP')
  unq <- !(colnames(xnat_data$prearch) %in% samecol)
  colnames(xnat_data$prearch)[unq] <- paste0(colnames(xnat_data$prearch)[unq],'_PREARCH')
  unq <- !(colnames(xnat_data$arch) %in% samecol)
  colnames(xnat_data$arch)[unq] <- paste0(colnames(xnat_data$arch)[unq],'_ARCH')







  xnat_all <- merge(xnat_data$backup, xnat_data$prearch, by = samecol, all.x = TRUE, all.y = TRUE)
  xnat_all <- merge(xnat_all, xnat_data$arch, by = samecol, all.x = TRUE, all.y = TRUE)

return(xnat_all)
}


XNAT.backupArchive <- function(xd, writefile, xnat_dates, xnat_filename_dates){
  #/xnatdata/backup/{STUDY}/{20230101_HHMMSSmmm}/{patientName}or{patientID}/SCANS
  #the time is the time, I think, that the first dicom was received. Can substitute with paste0(studDatetime,'000') (i.e., blank milliseconds)
  #/xnatdata/arch/{STUDY}/arc001/{patientName}or{patientID}/SCANS

  fileConn<-file(writefile,'w+')
  writeLines(c('#!/bin/bash','','# Copy datasets that are in archive, but not in the backup, to the appropriate backup folder',sprintf('#From %s to %s',xnat_filename_dates$start,xnat_filename_dates$end),''),
             fileConn)

  xd <- subset(xd, epochSer > xnat_dates$start)

  xdb <- subset(xd, !is.na(path_folder_ARCH) & is.na(path_folder_BACKUP))

  for(i in 1:dim(xdb)[1]){
    line <- xdb[i,]

    archloc <- line$path_ARCH
    archloc <- gsub('/xnatdata/arch/','',archloc)
    archloc <- gsub('/arc001','',archloc)
    studyID <- gsub('/','',archloc)

    scantime <- paste0(line$studDatetime,'000')
    scanname <- line$patientName

    orig <- file.path(line$path_folder_ARCH,'SCANS')
    dest <- file.path('/xnatdata/backup',studyID,scantime,scanname)

    writeLines(c(sprintf('mkdir -p %s',dest),
                 sprintf('rsync -a --ignore-existing %s %s', orig, file.path(dest,'SCANS')),
                 sprintf('chmod -R g+rX %s', dest),
                 sprintf('echo "%s of %s complete"', i, dim(xdb)[1])),
               fileConn)
    }
  close(fileConn)

  }




XNAT.process <- function(xnat_paths, dates){
  xnat_data_backup <- XNAT.process.backup(xnat_paths$backup, dates)
  xnat_data_prearch <- XNAT.process.prearch(xnat_paths$prearch, dates)
  xnat_data_arch <- XNAT.process.arch(xnat_paths$arch, dates)
  xnat_data <- list(
    backup = xnat_data_backup,
    prearch = xnat_data_prearch,
    arch = xnat_data_arch)
  xnat_data$all <- XNAT.all(xnat_data)
  return(xnat_data)
}


XNAT.process.arch <- function(xnat_path_arch, dates){
  xnat_data_raw <- XNAT.scanfolders.arch(xnat_path_arch)
  xnat_data_raw$path_folder <- paste(xnat_data_raw$path, xnat_data_raw$folder_name, sep = '/')
  xnat_data_raw <- XNAT.scanfolder_dates.arch(xnat_data_raw, dates)
  # trim to only entries more recent than the beginning of the ilab start date
  xnat_data <- subset(xnat_data_raw, ilab_range)

  # add column for the first dicom of the first series
  all_firstdicom_fullpaths <- unlist(
    lapply(1:nrow(xnat_data),
           function(i){ XNAT.get_dicom_fullpath.arch(xnat_data[i,],'first')
           }))
  xnat_data$firstdicom_fullpath <- all_firstdicom_fullpaths
  # add column for the last dicom of the last series
  all_lastdicom_fullpaths <- unlist(
    lapply(1:nrow(xnat_data),
           function(i){ XNAT.get_dicom_fullpath.arch(xnat_data[i,],'last')
           }))
  xnat_data$lastdicom_fullpath <- all_lastdicom_fullpaths

  # Get dicom header info
  xnat_data <- XNAT.getdcm(xnat_data)
  # Filter out non-MR modality
  xnat_data <- subset(xnat_data, modality == 'MR')
  # abbreviate study description label
  xnat_data$studDesc_short <- XNAT.abbreviateStudDesc(xnat_data$studDesc)

  # Get datetime/information
  xnat_data <- XNAT.processDates(xnat_data)

  #xnat_data$xnat_row is for merging with ilab_data$xnat_row
  xnat_data$xnat_row <- 1:dim(xnat_data)[1]
  return(xnat_data)
}

XNAT.process.prearch <- function(xnat_path_prearch, dates){
  XNAT.process.backup(xnat_path_prearch, dates)
}

XNAT.process.backup <- function(xnat_path_backup, dates){
  # list all folders in xnat backup and their paths
  xnat_data_raw <- XNAT.scanfolders(xnat_path_backup)
  xnat_data_raw$path_folder <- paste(xnat_data_raw$path, xnat_data_raw$folder_name, sep = '/')

  # convert to dates and whether it's in after the ilab start date
  xnat_data_raw <- XNAT.scanfolder_dates(xnat_data_raw, dates)

  # trim to only entries more recent than the beginning of the ilab start date
  xnat_data <- subset(xnat_data_raw, ilab_range)

  # add column for the first dicom of the first series
  all_firstdicom_fullpaths <- unlist(
    lapply(1:nrow(xnat_data),
           function(i){ XNAT.get_dicom_fullpath(xnat_data[i,],'first')
             }))
  xnat_data$firstdicom_fullpath <- all_firstdicom_fullpaths
  # add column for the last dicom of the last series
  all_lastdicom_fullpaths <- unlist(
    lapply(1:nrow(xnat_data),
           function(i){ XNAT.get_dicom_fullpath(xnat_data[i,],'last')
           }))
  xnat_data$lastdicom_fullpath <- all_lastdicom_fullpaths

  # Get dicom header info
  xnat_data <- XNAT.getdcm(xnat_data)
  # Filter out non-MR modality
  xnat_data <- subset(xnat_data, modality == 'MR')
  # abbreviate study description label
  xnat_data$studDesc_short <- XNAT.abbreviateStudDesc(xnat_data$studDesc)

  # Get datetime/information
  xnat_data <- XNAT.processDates(xnat_data)

  #xnat_data$xnat_row is for merging with ilab_data$xnat_row
  xnat_data$xnat_row <- 1:dim(xnat_data)[1]


  return(xnat_data)
}

XNAT.abbreviateStudDesc <- function(studDesc){
  #for each word in study description, trim to five characters.
studDesc_short <- unlist(lapply(studDesc, function(x){
  splt <- strsplit(x, ' ')[[1]]
  return(paste(unlist(lapply(splt, function(xx){
    substr(xx,1,min(5,nchar(xx)))})), collapse = ' '))
}))
  #if the study description contained numbers, restore to the non-trimmed original
studDesc_short[grepl('[0-9]',studDesc)] <- studDesc[grepl('[0-9]',studDesc)]
  #trim final result to 18 characters
studDesc_short <- unlist(lapply(studDesc_short, function(x){substr(x,1,18)}))

return(studDesc_short)
}


XNAT.get_dicom_fullpath.arch <- function(row, dicomorder = 'first'){

  #Get the path to the SCANS directory
  #The easy path to the SCANS directory is path/folder/xxx/SCANS. If it's not there, do the recursive approach that takes a long time because it keeps searching subfolders of SCANS once it finds it.
  pf <- file.path(row$path, row$folder_name)

  if(any(dir(pf) == 'SCANS')){
    scansdir <- 'SCANS'
  } else{
    scansdir <- dir(path = pf, recursive = TRUE, pattern = 'SCANS', include.dirs = TRUE)
  }


  #this method invoking the system command is significantly slower
  #  scansdir <- system2("find", args = c(file.path(row$path, row$folder_name), "-type", "d", "-name", "'SCANS'", "-prune"), stdout = TRUE)

  # Find the lowest/highest number of the sessions within SCANS. Character directories get converted to NA, which is fine. We suppress the warning of 'na introduced by coercion' which we don't care about
  suppressWarnings(lowest_dir <- min(as.numeric(dir(file.path(pf,scansdir))),na.rm = TRUE))
  suppressWarnings(highest_dir <- max(as.numeric(dir(file.path(pf,scansdir))),na.rm = TRUE))

  # within that, look in DICOM and/or secondary and get the full path to the first/last dicom. If none are found, it will return NA

  if(dicomorder == 'first'){
    mypath <- file.path(row$path, row$folder_name, scansdir, lowest_dir, c('DICOM','secondary'))
    mylist <- dir(mypath, pattern = '\\.dcm$', full.names = TRUE)
    if(length(mylist) == 0){
      allfiles <- dir(mypath, full.names = TRUE)
      unwanted <- dir(mypath, pattern = "xml", full.names = TRUE)
      mylist <- base::setdiff(allfiles, unwanted)
    }
    dicom_fullpath <- mylist[1]
  } else if(dicomorder == 'last'){
    mypath <- file.path(row$path, row$folder_name, scansdir, highest_dir, c('DICOM','secondary'))
    mylist <- dir(mypath, pattern = '\\.dcm$', full.names = TRUE)
    if(length(mylist) == 0){
      allfiles <- dir(mypath, full.names = TRUE)
      unwanted <- dir(mypath, pattern = "xml", full.names = TRUE)
      mylist <- base::setdiff(allfiles, unwanted)
    }

#    mylist <- dir(file.path(row$path, row$folder_name, scansdir, highest_dir, c('DICOM','secondary')), pattern = '\\.dcm$', full.names = TRUE)
    numdcm <- length(mylist)
    dicom_fullpath <- mylist[grepl(sprintf('-%s-',numdcm),mylist)]
    if(length(dicom_fullpath) > 1){
      # in the rare case that the final dicom file number is *also*
      # the number of the run, explicitly match for both
      dicom_fullpath <- mylist[grepl(sprintf('-%s-%s-',highest_dir,numdcm),mylist)]
    }

    if(length(dicom_fullpath) == 0)
      dicom_fullpath <- mylist[length(mylist)]
  }
  if(length(dicom_fullpath) == 0 ){dicom_fullpath <- NA}
  return(dicom_fullpath)
}


XNAT.get_dicom_fullpath <- function(row, dicomorder = 'first'){

  #Get the path to the SCANS directory
  #The easy path to the SCANS directory is path/folder/xxx/SCANS. If it's not there, do the recursive approach that takes a long time because it keeps searching subfolders of SCANS once it finds it.
  pf <- file.path(row$path, row$folder_name)

  subdir <- dir(pf)
  isd <- file.info(file.path(pf,subdir))$isdir
  sspath <- file.path(pf,subdir[isd])
  if(any(dir(sspath) == 'SCANS')){
    scansdir <- file.path(subdir[isd],'SCANS')
  } else{
    scansdir <- dir(path = pf, recursive = TRUE, pattern = 'SCANS', include.dirs = TRUE)
  }
  # a few folders in backup are accidentally nested ..SCANS/SCANS/1, so account for that here
  if(any(dir(file.path(pf,scansdir)) == 'SCANS')){
    scansdir <- file.path(scansdir,'SCANS')
  }

  #this method invoking the system command is significantly slower
#  scansdir <- system2("find", args = c(file.path(row$path, row$folder_name), "-type", "d", "-name", "'SCANS'", "-prune"), stdout = TRUE)

  # Find the lowest/highest number of the sessions within SCANS. Character directories get converted to NA, which is fine. We suppress the warning of 'na introduced by coercion' which we don't care about
  suppressWarnings(lowest_dir <- min(as.numeric(dir(file.path(pf,scansdir))),na.rm = TRUE))
  suppressWarnings(highest_dir <- max(as.numeric(dir(file.path(pf,scansdir))),na.rm = TRUE))

  # within that, look in DICOM and/or secondary and get the full path to the first/last dicom. If none are found, it will return NA

  if(dicomorder == 'first'){
    mypath <- file.path(row$path, row$folder_name, scansdir, lowest_dir, c('DICOM','secondary'))
  mylist <- dir(mypath, pattern = '\\.dcm$', full.names = TRUE)
  if(length(mylist) == 0){
    allfiles <- dir(mypath, full.names = TRUE)
    unwanted <- dir(mypath, pattern = "xml", full.names = TRUE)
    mylist <- base::setdiff(allfiles, unwanted)
    }
    dicom_fullpath <- mylist[1]
  } else if(dicomorder == 'last'){
    mypath <- file.path(row$path, row$folder_name, scansdir, highest_dir, c('DICOM','secondary'))
    mylist <- dir(mypath, pattern = '\\.dcm$', full.names = TRUE)
    if(length(mylist) == 0){
      allfiles <- dir(mypath, full.names = TRUE)
      unwanted <- dir(mypath, pattern = "xml", full.names = TRUE)
      mylist <- base::setdiff(allfiles, unwanted)
    }
#  mylist <- dir(file.path(row$path, row$folder_name, scansdir, highest_dir, c('DICOM','secondary')), pattern = '\\.dcm$', full.names = TRUE)
  numdcm <- length(mylist)
  dicom_fullpath <- mylist[grepl(sprintf('-%s-',numdcm),mylist)]
  if(length(dicom_fullpath) > 1){
    # in the rare case that the final dicom file number is *also*
    # the number of the run, explicitly match for both
    dicom_fullpath <- mylist[grepl(sprintf('-%s-%s-',highest_dir,numdcm),mylist)]

  }

  if(length(dicom_fullpath) == 0)
    # if you didn't find one using the above search methods, just grab the final one in the list
    dicom_fullpath <- mylist[length(mylist)]
    if(length(dicom_fullpath) == 0)
    {
      # if you still didn't get one because there are none, return NA
      dicom_fullpath <- NA
    }
  }
  return(dicom_fullpath)
}


XNAT.dicomtime <- function(dicomfile){
  if(is.na(dicomfile)){
    return(NA)
  }
  afnitags <- '0008,0033'
  dcm <- NA
  tryCatch({dcm <- oro.dicom::readDICOM(dicomfile)}, error = function(e){message(sprintf('error %s in dicom %s using oro.dicom::readDICOM',e,dicomfile))})
  if(class(dcm) =='logical' && is.na(dcm)){

    #use afni to get the header info
    dcm <- tryCatch({system2('dicom_hinfo', args = c('-no_name', '-tag', afnitags ,dicomfile), stdout = TRUE)}, error = function(e){message(sprintf('error %s in dicom %s using AFNI dicom_hinfo',e,dicomfile))})
    if(length(dcm) == 0){
      return(NA)
    } else { #got the data from afni successfully
      message('Success! DICOM header read with AFNI dicom_hdr')
      imageTime <- dcm
    }

  } else{ #got the data from oro.dicom successfully
    hdr <- dcm$hdr[[1]]
    imageTime <- hdr$value[hdr$name == 'ContentTime']
  }
  if(length(imageTime) == 0){imageTime <- NA}
  return(imageTime)

}


XNAT.getdcm <- function(xnat_data){
  dcmnames <- XNAT.dicominfo('names')
  xnat_data[,dcmnames] <- NA #get the dicom values to be returned for colnames
  if(!('imageTime' %in% colnames(xnat_data))){
    xnat_data[,'imageTime'] <- NA
  }
  # return some time and description values from the dicom header
  for(i in 1:nrow(xnat_data)){
    #    dcm_dat <- tryCatch({
    #      XNAT.dicominfo(xnat_data$firstdicom_fullpath[i])},
    #      error = function(e){message(e) return(NA)})
    dcm_dat <- XNAT.dicominfo(xnat_data$firstdicom_fullpath[i])
    for(dd in dcmnames){
      xnat_data[[dd]][i] <- dcm_dat[dd]
    }
    xnat_data$imageTime[i] <- XNAT.dicomtime(xnat_data$lastdicom_fullpath[i])
  }
  return(xnat_data)
}

XNAT.processDates <- function(xnat_data){

  #combine acquisition date and time, trim off milliseconds
  xnat_data$acqDatetime <- paste(xnat_data$acqDate,xnat_data$acqTime, sep = '_')
  xnat_data$acqDatetime <- unlist(lapply(xnat_data$acqDatetime, function(x){strsplit(x,'[.]')[[1]][1]}))
  xnat_data$acqDatetime_posix <- as.POSIXct(xnat_data$acqDatetime, format = '%Y%m%d_%H%M%S')
  xnat_data$epochAcq <- as.numeric(xnat_data$acqDatetime_posix)

  xnat_data$studDatetime <- paste(xnat_data$studDate,xnat_data$studTime, sep = '_')
  xnat_data$studDatetime <- unlist(lapply(xnat_data$studDatetime, function(x){strsplit(x,'[.]')[[1]][1]}))
  xnat_data$studDatetime_posix <- as.POSIXct(xnat_data$studDatetime, format = '%Y%m%d_%H%M%S')
  xnat_data$epochStud <- as.numeric(xnat_data$studDatetime_posix)

  xnat_data$serDatetime <- paste(xnat_data$serDate,xnat_data$serTime, sep = '_')
  xnat_data$serDatetime <- unlist(lapply(xnat_data$serDatetime, function(x){strsplit(x,'[.]')[[1]][1]}))
  xnat_data$serDatetime_posix <- as.POSIXct(xnat_data$serDatetime, format = '%Y%m%d_%H%M%S')
  xnat_data$epochSer <- as.numeric(xnat_data$serDatetime_posix)

  xnat_data$lastImageDatetime <- paste(xnat_data$serDate,xnat_data$imageTime, sep = '_')
  xnat_data$lastImageDatetime[is.na(xnat_data$imageTime)] <- NA
  xnat_data$lastImageDatetime <- unlist(lapply(xnat_data$lastImageDatetime, function(x){strsplit(x,'[.]')[[1]][1]}))
  xnat_data$lastImageDatetime_posix <- as.POSIXct(xnat_data$lastImageDatetime, format = '%Y%m%d_%H%M%S')
  xnat_data$epochLastImage <- as.numeric(xnat_data$lastImageDatetime_posix)

  xnat_data$durationMinutes <- (xnat_data$epochLastImage - xnat_data$epochSer)/60


  xnat_data$hourday <- strftime(xnat_data$studDatetime_posix, format = '%H')
  xnat_data$date <- strftime(xnat_data$studDatetime_posix, format = '%Y-%m-%d')
  xnat_data$daynum <-as.numeric(strftime(xnat_data$studDatetime_posix, format = '%u')) #M-Su, 1-7
  xnat_data$minuteday <- strftime(xnat_data$studDatetime_posix, format = '%M')
  xnat_data$hourminute <- as.numeric(xnat_data$hourday) + as.numeric(xnat_data$minuteday)/60


  return(xnat_data)
}


XNAT.dicominfo <- function(dicomfile){
  emptydat <- list(studDate = NA,
                   serDate = NA,
                   acqDate = NA,
                   studTime = NA,
                   serTime = NA,
                   acqTime = NA,
                   studDesc = NA,
                   modality = NA,
                   patientName = NA,
                   patientID = NA,
                   patientComments = NA)
  if(is.na(dicomfile)){
    return(emptydat)
  }

  if(dicomfile=='names'){
    return(c('studDate','serDate','acqDate','studTime','serTime','acqTime','studDesc','modality','patientName','patientID','patientComments'))
  }
  afnitags <- c('0008,0020','0008,0021','0008,0022','0008,0030','0008,0031','0008,0032', '0008,1030','0008,0060','0010,0010','0010,0020', '0010,4000')


  dcm <- NA
   tryCatch({dcm <- oro.dicom::readDICOM(dicomfile)}, error = function(e){message(sprintf('error %s in dicom %s using oro.dicom::readDICOM',e,dicomfile))})
  if(class(dcm) =='logical' && is.na(dcm)){

    #use afni to get the header info
    dcm <- tryCatch({system2('dicom_hinfo', args = c('-no_name', '-tag', afnitags ,dicomfile), stdout = TRUE)}, error = function(e){message(sprintf('error %s in dicom %s using AFNI dicom_hinfo',e,dicomfile))})
    if(length(dcm) == 0){
      return(emptydat)
    } else { #got the data from afni successfully
      message('Success! DICOM header read with AFNI dicom_hdr')
      dcm <- strsplit(dcm, ' ')[[1]]
      studDate <- dcm[1]
      serDate <- dcm[2]
      acqDate <- dcm[3]
      studTime <- dcm[4]
      serTime <- dcm[5]
      acqTime <- dcm[6]
      studDesc <- dcm[7]
      modality <- dcm[8]
      patientName <- dcm[9]
      patientID <- dcm[10]
      patientComments <- paste(dcm[11:length(dcm)], collapse = ' ')
    }

  } else{ #got the data from oro.dicom successfully
    hdr <- dcm$hdr[[1]]

    studDate <- hdr$value[hdr$name == 'StudyDate']
    serDate <- hdr$value[hdr$name == 'SeriesDate']
    acqDate <- hdr$value[hdr$name == 'AcquisitionDate']

    studTime <- hdr$value[hdr$name == 'StudyTime']
    serTime <- hdr$value[hdr$name == 'SeriesTime']
    acqTime <- hdr$value[hdr$name == 'AcquisitionTime']

    studDesc <- hdr$value[hdr$name == 'StudyDescription']
    modality <- hdr$value[hdr$name == 'Modality']

    patientName <- hdr$value[hdr$name == 'PatientsName']
    patientID <- hdr$value[hdr$name == 'PatientID']
    patientComments <- hdr$value[hdr$name == 'PatientComments']
  }
    return(list = c(
    studDate = studDate,
    serDate = serDate,
    acqDate = acqDate,
    studTime = studTime,
    serTime = serTime,
    acqTime = acqTime,
    studDesc = studDesc,
    modality = modality,
    patientName = patientName,
    patientID = patientID,
    patientComments = patientComments
    ))

}


XNAT.scanfolders.arch <- function(xnat_path){
  project_folders <- fs::dir_ls(xnat_path, type = 'directory')

  xlist <- list()
  index <- 0
  for(project in project_folders){
    tryCatch({
      dd <- dir(project)
    if( any(grepl('arc', dd)) ){
      for(oned in dd){
        index <- index + 1

        subproject <- file.path(project,oned)
        scanlist <- fs::dir_ls(subproject, type = 'directory')
        if(length(scanlist) > 0){
        xlist[[index]] <- data.frame(folder_name = fs::path_file(scanlist),
                                   path = subproject,
                                   stringsAsFactors = FALSE)
        }
      }
    } else {
      index <- index + 1
      scanlist <- fs::dir_ls(project, type = 'directory')
      if(length(scanlist) > 0){
      xlist[[index]] <- data.frame(folder_name = fs::path_file(scanlist),
                                   path = project,
                                   stringsAsFactors = FALSE)
      }

    }
    }, error = function(e){
      message(paste("Skipping", project, "due to error:", e$message))

    })
  }
  df <- data.table::rbindlist(xlist)
  return(df)

}

# Scans xnat's backup folder and returns all folders of format YYYYMMDD_HHMMSSSSS.
# If they are nested within a project folder, the project is included in a path column
XNAT.scanfolders <- function(xnat_path) {
  # Get the subfolders of the input folder
  scan_folders <- fs::dir_ls(xnat_path, type = "directory")

  # Initialize an empty list to store folder paths
  xlist <- list()
  index <- 0
  # Loop through each scan
  for (scan in scan_folders) {
    index <- index + 1
    # Use a tryCatch block to handle permission denied errors
    tryCatch({
      # Get the basename of the scan
      basename <- fs::path_file(scan)

      # Check if the basename is a datetime format (YYYYMMDD_HHMMSSSSS)
      if (grepl("^\\d{8}_\\d{9}$", basename)) {
        # If yes, add a row to the dataframe with the folder_name and path
        xlist[[index]] <- data.frame(folder_name = basename, path = xnat_path, stringsAsFactors = FALSE)
      } else {
        # If no, assume it is a project ID and recursively call the function on it
        xlist[[index]] <- XNAT.scanfolders(scan)
      }
    }, error = function(e) {
      # If an error occurs, print a message and continue with the next scan
      message(paste("Skipping", scan, "due to error:", e$message))
    })
  }
  df <- data.table::rbindlist(xlist)

  # Return the dataframe
  return(df)
}



XNAT.scanfolder_dates.arch <- function(df, dates){
  modtimes <- unlist(lapply(df$path_folder, function(x){
    file.info(x)$mtime
  }))
  df$moddate <- as.POSIXct(modtimes, origin = '1970-01-01')
  df$modepoch <- modtimes
  df$epochs_after_ilabstart <- modtimes - dates$start

  df$ilab_range <- df$epochs_after_ilabstart >=0

  return(df)

}


#converts foldername to date, and applies a logical whether it is later than the report's start date (unbounded recency, because the scan could be uploaded at a later date than the scantime)

XNAT.scanfolder_dates <- function(df, dates) {
  datestart <- 0
  dateend <- 0
  # If date is passed as an epoch already, no need to convert
  if(is.numeric(dates$start)){
    datestart <- dates$start
    } else {
    datestart <- as.numeric(lubridate::mdy(dates$start))
    }
  if(is.numeric(dates$end)){
    dateend <- dates$end
  } else {
    dateend <- as.numeric(lubridate::mdy(dates$end))
  }

  # Create a new column that is a date formatted version of folder_name
  df$date <- lubridate::ymd_hms(substr(df$folder_name, 1, 15), tz = 'America/Chicago')

  # Create an age column that says how many days old that date value is relative to today
  #df$days_after_ilabstart <- as.integer(difftime(lubridate::date(df$date), datestart, units = "days"))

  df$epochs_after_ilabstart <- as.numeric(df$date) - datestart

  df$ilab_range <- df$epochs_after_ilabstart >=0
  # NOTE: need to get xnat dates that are > start, and < (end+1day). Because empty time days are at midnight.

  return(df)
}

