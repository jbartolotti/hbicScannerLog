

ILAB.process <- function(ilab_path, ilab_filename, allowed_usage_types, usage_type_abbreviations, allowed_asset_types){

  ilab<- list()

  # import the ilab file
  ilab_raw <- ILAB.load(file.path(ilab_path, ilab_filename))

  # rename columns, filter out the non-scans
  ret <- ILAB.filter(ilab_raw, allowed_usage_types, usage_type_abbreviations, allowed_asset_types)
  ilab$data <- ret$ilab_filter
  ilab$ignored_usage_types <- ret$ignored_usage_types
  ilab$ignored_asset_types <- ret$ignored_asset_types

  # Pull out the date range, start and end, of the iLab report
  ilab$data <- ILAB.processDates(ilab$data)
  ret <- ILAB.getDate(ilab_filename)
  ilab$filename_dates <- list(start = ret$start, end = ret$end)
  ilab$dates <- list(start = min(ilab$data$epoch), end = max(ilab$data$epoch))

  ilab$data$study_id_short <- ILAB.AbbreviateStudyID(ilab$data$study_id)
  ilab$data$daynum <- as.numeric(ilab$data$daynum)
  ilab$data$minuteday <- as.numeric(strftime(ilab$data$purchase_date_posix, format = '%M'))
  ilab$data$hourminute <- as.numeric(ilab$data$hourday) + as.numeric(ilab$data$minuteday)/60


  return(ilab)
}

ILAB.getDate <- function(ilab_filename){
  spl <- strsplit(ilab_filename, '_')[[1]]
  dates <- list(
    start = spl[1],
    end = spl[2]
  )
  return(dates)
}


# Load the specified ilab report file
ILAB.load <- function(ilab_path_filename){
  ilab_raw <- read.csv(ilab_path_filename)
  return(ilab_raw)
}

# Abbreviate study IDs. Modify later to allow custom study_id abberv defined in config
ILAB.AbbreviateStudyID <- function(study_ids){
  study_id_short <- unlist(lapply(study_ids, function(x){
    # apply abbreviation rules to units separated by '_'
    splt <- strsplit(x, '_')[[1]]
    # Return numeric units as-is. For the rest, truncate to first three characters.
    shortened <- unlist(lapply(splt, function(x){
      if(grepl('[0-9]',x)){
        ret <- x
      } else {
        ret <- substring(x,1,3)
      }
      return(ret)

    }))
    return(paste(shortened, collapse = '_'))
  }))
return(study_id_short)
}

ILAB.processDates <- function(ilab_data){

  # Try to convert purchase date to posix.
  # If date was in non-standard time and couldn't be parsed,
  # try the excel datetime format.
  # If that doesn't work either, return NA
  ilab_data$purchase_date_posix <- tryCatch({as.POSIXct(ilab_data$purchase_date)},
           error = function(e){
             tryCatch({ilab_data$purchase_date_posix <- as.POSIXct(ilab_data$purchase_date, format = '%m/%d/%Y %H:%M')},
                      error = NA)
             })

  ilab_data$epoch <- as.numeric(ilab_data$purchase_date_posix)
  ilab_data$hourday <- strftime(ilab_data$purchase_date_posix, format = '%H')
  ilab_data$date <- strftime(ilab_data$purchase_date_posix, format = '%Y-%m-%d')
  ilab_data$daynum <- strftime(ilab_data$purchase_date_posix, format = '%u') #M-Su, 1-7

  return(ilab_data)
}


# reformat the raw ilab file.
# input a list of which usage_type fields to use. Return the filtered data,
# and a list of which usage_types were excluded. The excluded list gets printed at the end of the report, so that you can see if a usage type was mistyped etc., and needs to be added to the list.

ILAB.filter <- function(ilab_raw, allowed_usage_types, usage_type_abbreviations, allowed_asset_types){

  # rename columns to be nicer
  ilab_trim <- ilab_raw[,c('Study.ID','Study.Title','Notes','Purchase.Date','Usage.Type','Asset.ID','Charge.Name','Quantity')]
  colnames(ilab_trim) <- c('study_id','study_title','notes','purchase_date','usage_type','asset_type','charge_name','quantity')
  ilab_trim$usage_type <- trimws(ilab_trim$usage_type)

  # filter asset types, and save the names of the ones that are ignored
  ilab_trim_asset_types <- unique(ilab_trim$asset_type)
  ignored_assets <- ilab_trim_asset_types[! ilab_trim_asset_types %in% allowed_asset_types]


  # filter usage types, and save the names of the ones that are ignored
  ilab_trim_usage_types <- unique(ilab_trim$usage_type)
  ignored_types <- ilab_trim_usage_types[! ilab_trim_usage_types %in% allowed_usage_types]

  # remove usage types that aren't scanner related
  ilab_filter <- ilab_trim[ilab_trim$usage_type %in% allowed_usage_types & ilab_trim$asset_type %in% allowed_asset_types,]
  # Apply abbreviations to usage types for use on figures
  ilab_filter$usage_type_short <- unlist(usage_type_abbreviations[ilab_filter$usage_type])




  returndat <- list(
    ilab_filter = ilab_filter,
    ignored_usage_types = ignored_types,
    ignored_asset_types = ignored_assets
    )
  return(returndat)
}
