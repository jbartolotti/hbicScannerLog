

#' load configurations.
#' If a list, first load defaults, then overwrite with anything in the supplied config.
#' If filename, first load defaults then overwrite with anything in the txt file.
#' If 'default' then just load defaults.
#' @export
load_config <- function(input_config){
  if(class(input_config) == 'list' ){
    default_config <- CONFIG.defaultConfig()
    config <- modifyList(default_config, input_config)
    config <- CONFIG.evalConfig(config)
  } else if(class(input_config) == 'character'){

    if(input_config == 'default'){
      config <- CONFIG.defaultConfig()
    } else{
      #default_config <- LOAD.defaultConfig()
      # file_config <- LOAD.fileConfig(input_config) LOAD VALUES FROM TEXT FILE AND PUT IN A LIST
      # config <- modifyList(default_config, file_config)
      # config <- LOAD.calcConfig(config)

      stop('loading config fields from a text file not currently supported. Please pass a list containing just the fields you want to overwrite to config instead of a filename.')
    }
  }
  return(config)
}


CONFIG.defaultConfig <- function(){
  config <- list()
  config$allowed_usage_types <- c(
    'QC',
    'Full Research Study Scan',
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
    "Other Clinical Scan",
    "Research Scan",
    "Participant Bailed/Screen failure",
    "Cancelation after cut-off",
    "Technique Development (TD) Scan - No charge",
    "Research Scan2",
    "Unusable data - No Charge",
    "Protocol Harmonization Scan",
    "Participant No-show",
    "Quality Control Scan"



    )
  config$allowed_asset_types <- c(
    269500) #3T scanner
  config$time_threshold_minutes <- 30

  config$usage_type_abbreviations <- list()
  config$usage_type_abbreviations[['QC']] <-'QC'
  config$usage_type_abbreviations[['Full Research Study Scan']] <-'RSRCH'
  config$usage_type_abbreviations[['Clinical Trial NIH Rate 2']] <- 'CLIN'
  config$usage_type_abbreviations[['Cancelation - by Subject/Study Team - after cut-off']] <- 'CNCL'
  config$usage_type_abbreviations[["Cancelation - by Subject/Study Team - prior cut-off"]] <- 'CNCL'
  config$usage_type_abbreviations[["Clinical Trial Scan"]] <- 'CLIN'
  config$usage_type_abbreviations[["Cancelation - after cut-off_Clinical trial"]] <- 'CNCL'
  config$usage_type_abbreviations[["DIAN-TU - NIH Rate"]] <- 'DIAN-TU'
  config$usage_type_abbreviations[["Clinical Trial Scan - NIH rate1"]] <- 'CLIN'
  config$usage_type_abbreviations[["Pilot Study Scan"]] <- 'PILOT'
  config$usage_type_abbreviations[["Full Study scan - FY19 rate"]] <- 'RSRCH'
  config$usage_type_abbreviations[["Subject No Show "]] <- 'NOSHOW'
  config$usage_type_abbreviations[["Other Clinical Scan"]] <- 'CLIN'
  config$usage_type_abbreviations[["Research Scan"]] <- 'RSRCH'
  config$usage_type_abbreviations[["Participant Bailed/Screen failure"]] <- 'ABORT'
  config$usage_type_abbreviations[["Cancelation after cut-off"]] <- 'CNCL'
  config$usage_type_abbreviations[["Technique Development (TD) Scan - No charge"]] <- 'TD'
  config$usage_type_abbreviations[["Research Scan2"]] <- 'RSRCH'
  config$usage_type_abbreviations[["Unusable data - No Charge"]] <- 'BAD DATA'
  config$usage_type_abbreviations[["Protocol Harmonization Scan"]] <- 'PROTOCOL'
  config$usage_type_abbreviations[["Participant No-show"]] <- 'NO SHOW'
  config$usage_type_abbreviations[["Quality Control Scan"]] <- 'QC'


  return(config)
}


CONFIG.evalConfig <- function(input_config, config = NA){
  if(class(config) != 'list' && is.na(config)){
    config <- input_config
  }
  for(f in names(input_config)){
    if(class(input_config[[f]]) == 'list'){
      input_config[[f]] <- CONFIG.evalConfig(input_config[[f]], config)
    } else{
      if(grepl('EVAL:', input_config[[f]])){
        cmd <- trimws(strsplit(input_config[[f]],'EVAL:')[[1]][2])
        input_config[[f]] <- eval(parse(text = cmd))
        config[[f]] <- input_config[[f]]
      }
    }

  }
  return(input_config)
}
