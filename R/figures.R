

FIGURES.weeklySummary <- function(ilab_data, xnat_data, figdir){
  figure_filenames <- list()
 # ilab_data$daynum <- as.numeric(ilab_data$daynum)
  xnat_data$backup$daynum <- as.numeric(xnat_data$backup$daynum)
  xnat_data$prearch$daynum <- as.numeric(xnat_data$prearch$daynum)
  xnat_data$arch$daynum <- as.numeric(xnat_data$arch$daynum)

  hourlines <- list(start = 7, end = 19,
                    color = '#444444')
  hourlines$labels <- c(paste(hourlines$start:12, ':00 AM', sep = ''),
                        paste(1:(hourlines$end-12), ':00 PM', sep = ''))
  ilab_color <- list(bg = '#FFFCB6', text = '#736F25',
                     usage_type = 'black',
                     bad_bg = '#FAB5AA', bad_outline = '#F83B1E')
  xnat_color <- list(
    backup = list(bg = '#A4F4FA', text = '#0000AA'),
    prearch = list(bg = '#C8FFE7', text = 'black'),
    arch = list(bg = '#A4FAA5', text = '#00AA00')
    )
  daysofweek  <-c('Mon','Tues','Wed','Thur','Fri','Sat','Sun')
  redscans <- c('BAD DATA','CNCL','ABORT','NO SHOW')

  #get all of the mondays in the ilab_data
  monday_list <- unique(ilab_data$date[ilab_data$daynum == 1])

#  ilab_data$minuteday <- strftime(ilab_data$purchase_date_posix, format = '%M')
#  ilab_data$hourminute <- as.numeric(ilab_data$hourday) + as.numeric(ilab_data$minuteday)/60
  xnat_data$backup$minuteday <- strftime(xnat_data$backup$serDatetime_posix, format = '%M')
  xnat_data$backup$hourminute <- as.numeric(xnat_data$backup$hourday) + as.numeric(xnat_data$backup$minuteday)/60
  xnat_data$prearch$minuteday <- strftime(xnat_data$prearch$serDatetime_posix, format = '%M')
  xnat_data$prearch$hourminute <- as.numeric(xnat_data$prearch$hourday) + as.numeric(xnat_data$prearch$minuteday)/60
  xnat_data$arch$minuteday <- strftime(xnat_data$arch$serDatetime_posix, format = '%M')
  xnat_data$arch$hourminute <- as.numeric(xnat_data$arch$hourday) + as.numeric(xnat_data$arch$minuteday)/60

  #make a week figure for each collection of monday - sunday.
  for(monday in monday_list){
    epoch_start <- as.numeric(as.POSIXct(monday))
    epoch_end <- epoch_start + (7*24*60*60)
    ilab_thisweek <- subset(ilab_data, epoch >= epoch_start & epoch < epoch_end)
    xnat_backup_thisweek <- subset(xnat_data$backup, epochSer >= epoch_start & epochSer < epoch_end)
    xnat_prearch_thisweek <- subset(xnat_data$prearch, epochSer >= epoch_start & epochSer < epoch_end)
    xnat_arch_thisweek <- subset(xnat_data$arch, epochSer >= epoch_start & epochSer < epoch_end)

    eachdate <- as.POSIXct(epoch_start+c(0:6)*(24*60*60), origin = '1970-01-01')
    if(any(
      ilab_thisweek$epoch > epoch_start+5*(24*60*60) ||
      xnat_backup_thisweek$epochSer > epoch_start+5*(24*60*60) ||
      xnat_prearch_thisweek$epochSer > epoch_start+5*(24*60*60) ||
      xnat_arch_thisweek$epochSer > epoch_start+5*(24*60*60)
    )){
      lastdaynum <- 7
    } else{
      lastdaynum <- 5
    }
    daylabels <- paste(daysofweek[1:lastdaynum], strftime(eachdate[1:lastdaynum], format = '%m/%d'))
    mytitle <- paste(strftime(eachdate[1], format = '%Y, %B %d'),
                     strftime(eachdate[lastdaynum], format = '%B %d'), sep = ' - ')
    myfilename <- sprintf('ilab_xnat_schedule_%s_%s.png',strftime(eachdate[1], format = '%Y%m%d'), strftime(eachdate[lastdaynum], format = '%Y%m%d'))
    figure_filenames[[monday]] <- myfilename

    ggplot2::ggplot(ilab_thisweek, ggplot2::aes(x = daynum-.2, y =  hourminute)) +
      ggplot2::theme_bw() +
      ggplot2::geom_hline(yintercept = seq(from = hourlines$start, to = hourlines$end, by = 1),
                 color = hourlines$color) +
      # ilab boxes
      ggplot2::geom_rect(ggplot2::aes(ymin = hourminute, ymax = hourminute+quantity,
                    xmin = daynum-.4, xmax = daynum),
                fill = ilab_color$bg, color = 'black') +
      ggplot2::geom_rect(data = subset(ilab_thisweek, usage_type_short %in% redscans),
                         ggplot2::aes(ymin = hourminute, ymax = hourminute+quantity,
                                      xmin = daynum-.4, xmax = daynum),
                         fill = ilab_color$bad_bg, color = ilab_color$bad_outline) +
      #xnat boxes
      ggplot2::geom_rect(data = xnat_backup_thisweek,
                ggplot2::aes(ymin = hourminute, ymax = hourminute+durationMinutes/60,
                    xmin = daynum, xmax = daynum+.15),
                fill = xnat_color$backup$bg, color = 'black') +
      ggplot2::geom_rect(data = xnat_prearch_thisweek,
               ggplot2::aes(ymin = hourminute, ymax = hourminute+durationMinutes/60,
                    xmin = daynum+.15, xmax = daynum+.25),
                   fill = xnat_color$prearch$bg, color = 'black') +
      ggplot2::geom_rect(data = xnat_arch_thisweek,
                ggplot2::aes(ymin = hourminute, ymax = hourminute+durationMinutes/60,
                    xmin = daynum+.25, xmax = daynum+.4),
                fill = xnat_color$arch$bg, color = 'black') +
      #ilab start points
      ggplot2::geom_point() +
      #xnat start points
      ggplot2::geom_point(data = xnat_backup_thisweek,
                 ggplot2::aes(x = daynum+.2, y = hourminute)) +
      ggplot2::geom_point(data = xnat_arch_thisweek,
                 ggplot2::aes(x = daynum+.2, y = hourminute)) +
      #ilabtext
      ggplot2::geom_text(ggplot2::aes(x = daynum-.2, y = hourminute+.2, label = study_id_short),
                color = ilab_color$text) +
      ggplot2::geom_text(ggplot2::aes(x = daynum-.2, y = hourminute+.5, label = usage_type_short),
                color = ilab_color$usage_type) +
      #xnat text
      ggplot2::geom_text(data = xnat_backup_thisweek,
                ggplot2::aes(x = daynum+.2, y = hourminute+.2, label = studDesc_short),
                color = xnat_color$backup$text) +
      ggplot2::geom_text(data = xnat_arch_thisweek,
                ggplot2::aes(x = daynum+.2, y = hourminute+.4, label = studDesc_short),
                color = xnat_color$arch$text) +
      # hour and day axes
      ggplot2::scale_y_continuous(breaks = hourlines$start:hourlines$end, trans = 'reverse',
                         labels =  hourlines$labels) +
      ggplot2::scale_x_continuous(breaks = 1:lastdaynum, position = 'top',
                         labels = daylabels) +
      ggplot2::labs(x = '', y = '', title = mytitle )

    ggplot2::ggsave(file.path(figdir, myfilename), width = 4+lastdaynum*2, height = 8)
  }
  #make a tall figure for each day
  for(thisday in unique(ilab_data$date)){
    epoch_start <- as.numeric(as.POSIXct(thisday))
    epoch_end <- epoch_start + (24*60*60)
    ilab_thisday <- subset(ilab_data, epoch >= epoch_start & epoch < epoch_end)
    xnat_backup_thisday <- subset(xnat_data$backup, epochSer >= epoch_start & epochSer < epoch_end)
    xnat_prearch_thisday <- subset(xnat_data$prearch, epochSer >= epoch_start & epochSer < epoch_end)
    xnat_arch_thisday <- subset(xnat_data$arch, epochSer >= epoch_start & epochSer < epoch_end)

    posx <- as.POSIXct(epoch_start, origin = '1970-01-01')

    daylabel <- paste(daysofweek[as.numeric(ilab_thisday$daynum[1])], strftime(posx, format = '%m/%d'))
    myfilename <- sprintf('ilab_xnat_schedule_%s.png',strftime(posx, format = '%Y%m%d'))


    ggplot2::ggplot(ilab_thisday, ggplot2::aes(x = daynum-.2, y =  hourminute)) +
      ggplot2::theme_bw() +
      ggplot2::geom_hline(yintercept = seq(from = hourlines$start, to = hourlines$end, by = 1),
                          color = hourlines$color) +
      # ilab boxes
      ggplot2::geom_rect(ggplot2::aes(ymin = hourminute, ymax = hourminute+quantity,
                                      xmin = daynum-.4, xmax = daynum),
                         fill = ilab_color$bg, color = 'black') +
      ggplot2::geom_rect(data = subset(ilab_thisday, usage_type_short %in% redscans),
                         ggplot2::aes(ymin = hourminute, ymax = hourminute+quantity,
                                      xmin = daynum-.4, xmax = daynum),
                         fill = ilab_color$bad_bg, color = ilab_color$bad_outline) +

      #xnat boxes
      ggplot2::geom_rect(data = xnat_backup_thisday,
                         ggplot2::aes(ymin = hourminute, ymax = hourminute+durationMinutes/60,
                                      xmin = daynum, xmax = daynum+.15),
                         fill = xnat_color$backup$bg, color = 'black') +
      ggplot2::geom_rect(data = xnat_prearch_thisday,
                         ggplot2::aes(ymin = hourminute, ymax = hourminute+durationMinutes/60,
                                      xmin = daynum+.15, xmax = daynum+.25),
                         fill = xnat_color$prearch$bg, color = 'black') +
      ggplot2::geom_rect(data = xnat_arch_thisday,
                         ggplot2::aes(ymin = hourminute, ymax = hourminute+durationMinutes/60,
                                      xmin = daynum+.25, xmax = daynum+.4),
                         fill = xnat_color$arch$bg, color = 'black') +
      #ilab start points
      ggplot2::geom_point() +
      #xnat start points
      ggplot2::geom_point(data = xnat_backup_thisday,
                          ggplot2::aes(x = daynum+.2, y = hourminute)) +
      ggplot2::geom_point(data = xnat_arch_thisday,
                          ggplot2::aes(x = daynum+.2, y = hourminute)) +
      #ilabtext
      ggplot2::geom_text(ggplot2::aes(x = daynum-.2, y = hourminute+.2, label = study_id_short),
                         color = ilab_color$text) +
      ggplot2::geom_text(ggplot2::aes(x = daynum-.2, y = hourminute+.5, label = usage_type_short),
                         color = ilab_color$usage_type) +
      #xnat text
      ggplot2::geom_text(data = xnat_backup_thisday,
                         ggplot2::aes(x = daynum+.2, y = hourminute+.2, label = studDesc_short),
                         color = xnat_color$backup$text) +
      ggplot2::geom_text(data = xnat_arch_thisday,
                         ggplot2::aes(x = daynum+.2, y = hourminute+.4, label = studDesc_short),
                         color = xnat_color$arch$text) +
      # hour and day axes
      ggplot2::scale_y_continuous(breaks = hourlines$start:hourlines$end, trans = 'reverse',
                                  labels =  hourlines$labels) +
      ggplot2::scale_x_continuous(breaks = 1, position = 'top',
                                  labels = daylabel) +
      ggplot2::labs(x = NULL, y = NULL )
    ggplot2::ggsave(file.path(figdir, myfilename), width = 4, height = 12)

  }


  return(figure_filenames)

}







FIGURES.xnatWeeklySummary <- function(xd, figdir,startepoch){
  xd <- subset(xd, epochSer > startepoch)
  figure_filenames <- list()

  hourlines <- list(start = 7, end = 19,
                    color = '#444444')
  hourlines$labels <- c(paste(hourlines$start:12, ':00 AM', sep = ''),
                        paste(1:(hourlines$end-12), ':00 PM', sep = ''))
  xnat_color <- list(
    backup = list(bg = '#A4F4FA', text = '#0000AA'),
    prearch = list(bg = '#C8FFE7', text = 'black'),
    arch = list(bg = '#A4FAA5', text = '#00AA00')
  )
  daysofweek  <-c('Mon','Tues','Wed','Thur','Fri','Sat','Sun')

  #get all of the mondays (or other earliest day) in the ilab_data
  monday_list <- unique(xd$date[xd$daynum == min(xd$daynum)])


  #make a week figure for each collection of monday - sunday.
  for(monday in monday_list){
    epoch_start <- as.numeric(as.POSIXct(monday))
    epoch_end <- epoch_start + (7*24*60*60)
    xd_thisweek <- subset(xd, epochSer >=epoch_start & epochSer < epoch_end)

    eachdate <- as.POSIXct(epoch_start+c(0:6)*(24*60*60), origin = '1970-01-01')
    if(any(
      xd_thisweek$epochSer > epoch_start+5*(24*60*60)
    )){
      lastdaynum <- 7
    } else{
      lastdaynum <- 5
    }
    daylabels <- paste(daysofweek[1:lastdaynum], strftime(eachdate[1:lastdaynum], format = '%m/%d'))
    mytitle <- paste(strftime(eachdate[1], format = '%Y, %B %d'),
                     strftime(eachdate[lastdaynum], format = '%B %d'), sep = ' - ')
    myfilename <- sprintf('xnat_schedule_%s_%s.png',strftime(eachdate[1], format = '%Y%m%d'), strftime(eachdate[lastdaynum], format = '%Y%m%d'))
    figure_filenames[[monday]] <- myfilename

    ggplot2::ggplot(xd_thisweek, ggplot2::aes(x = daynum)) +
      ggplot2::theme_bw() +
      ggplot2::geom_hline(yintercept = seq(from = hourlines$start, to = hourlines$end, by = 1),
                          color = hourlines$color) +
      ggplot2::geom_vline(xintercept = seq(from = 1.5, to = lastdaynum-.5, by = 1),
                          color = hourlines$color) +

      #xnat boxes
      ggplot2::geom_rect(data = subset(xd_thisweek, !is.na(path_folder_BACKUP)),
                         ggplot2::aes(ymin = hourminute_BACKUP, ymax = hourminute_BACKUP+durationMinutes/60,
                                      xmin = daynum-.45, xmax = daynum-.15),
                         fill = xnat_color$backup$bg, color = 'black') +
      ggplot2::geom_rect(data = subset(xd_thisweek, !is.na(path_folder_PREARCH)),
                         ggplot2::aes(ymin = hourminute_PREARCH, ymax = hourminute_PREARCH+durationMinutes/60,
                                      xmin = daynum-.15, xmax = daynum+.15),
                         fill = xnat_color$prearch$bg, color = 'black') +
      ggplot2::geom_rect(data = subset(xd_thisweek, !is.na(path_folder_ARCH)),
                         ggplot2::aes(ymin = hourminute_ARCH, ymax = hourminute_ARCH+durationMinutes/60,
                                      xmin = daynum+.15, xmax = daynum+.45),
                         fill = xnat_color$arch$bg, color = 'black') +
      ggplot2::geom_rect(data = subset(xd_thisweek, !is.na(path_folder_ARCH) & is.na(path_folder_BACKUP)),
                         ggplot2::aes(ymin = hourminute_ARCH, ymax = hourminute_ARCH+durationMinutes/60,
                                      xmin = daynum+.15, xmax = daynum+.45),
                         fill = xnat_color$arch$bg, color = 'red') +

      #xnat start points
      ggplot2::geom_point(data = subset(xd_thisweek, !is.na(path_folder_BACKUP)),
                          ggplot2::aes(x = daynum-.3, y = hourminute_BACKUP)) +
      ggplot2::geom_point(data = subset(xd_thisweek, !is.na(path_folder_ARCH)),
                          ggplot2::aes(x = daynum+.3, y = hourminute_ARCH)) +
      #xnat text
      ggplot2::geom_text(data = subset(xd_thisweek, !is.na(path_folder_BACKUP)),
                         ggplot2::aes(x = daynum, y = hourminute_BACKUP+.2, label = studDesc_short),
                         color = 'black') +
      ggplot2::geom_text(data = subset(xd_thisweek, !is.na(path_folder_ARCH)),
                         ggplot2::aes(x = daynum, y = hourminute_ARCH+.2, label = studDesc_short),
                         color = 'black') +
      ggplot2::geom_text(data = subset(xd_thisweek, !is.na(path_folder_BACKUP)),
                         ggplot2::aes(x = daynum, y = hourminute_BACKUP+.4, label = patientName),
                         color = 'black') +
      ggplot2::geom_text(data = subset(xd_thisweek, !is.na(path_folder_ARCH)),
                         ggplot2::aes(x = daynum, y = hourminute_ARCH+.4, label = patientName),
                         color = 'black') +
      # hour and day axes
      ggplot2::scale_y_continuous(breaks = hourlines$start:hourlines$end, trans = 'reverse',
                                  labels =  hourlines$labels) +
      ggplot2::scale_x_continuous(breaks = 1:lastdaynum, position = 'top',
                                  labels = daylabels) +
      ggplot2::labs(x = '', y = '', title = mytitle )

    ggplot2::ggsave(file.path(figdir, myfilename), width = 6+lastdaynum*2, height = 12)
  }


  return(figure_filenames)

}




FIGURES.xnatDaySummary <- function(xd, figdir){
  figure_filenames <- list()
  day <- unique(xd$date)
  daynum=unique(xd$daynum)


  hourlines <- list(start = 7, end = 19,
                    color = '#444444')
  hourlines$labels <- c(paste(hourlines$start:12, ':00 AM', sep = ''),
                        paste(1:(hourlines$end-12), ':00 PM', sep = ''))
  xnat_color <- list(
    backup = list(bg = '#A4F4FA', text = '#0000AA'),
    prearch = list(bg = '#C8FFE7', text = 'black'),
    arch = list(bg = '#A4FAA5', text = '#00AA00')
  )
  daysofweek  <-c('Mon','Tues','Wed','Thur','Fri','Sat','Sun')

  thisdayofweek <- daysofweek[unique(xd$daynum)]


    daylabels <- paste(daysofweek[unique(xd$daynum)], strftime(day, format = '%m/%d'))
    mytitle <- strftime( day, format = '%A %B %d, %Y')

    myfilename <- sprintf('xnat_daily_%s.png',strftime(day, format = '%Y%m%d'))
    figure_filenames <- myfilename

    textshift <- .75

    ggplot2::ggplot(xd, ggplot2::aes(x = daynum)) +
      ggplot2::theme_bw() +
      ggplot2::geom_hline(yintercept = seq(from = hourlines$start, to = hourlines$end, by = 1),
                          color = hourlines$color) +

      #xnat boxes
      ggplot2::geom_rect(data = subset(xd, !is.na(path_folder_BACKUP)),
                         ggplot2::aes(ymin = hourminute_BACKUP, ymax = hourminute_BACKUP+durationMinutes/60,
                                      xmin = daynum-.45, xmax = daynum-.15),
                         fill = xnat_color$backup$bg, color = 'black') +
      ggplot2::geom_rect(data = subset(xd, !is.na(path_folder_PREARCH)),
                         ggplot2::aes(ymin = hourminute_PREARCH, ymax = hourminute_PREARCH+durationMinutes/60,
                                      xmin = daynum-.15, xmax = daynum+.15),
                         fill = xnat_color$prearch$bg, color = 'black') +
      ggplot2::geom_rect(data = subset(xd, !is.na(path_folder_ARCH)),
                         ggplot2::aes(ymin = hourminute_ARCH, ymax = hourminute_ARCH+durationMinutes/60,
                                      xmin = daynum+.15, xmax = daynum+.45),
                         fill = xnat_color$arch$bg, color = 'black') +
      ggplot2::geom_rect(data = subset(xd, !is.na(path_folder_ARCH) & is.na(path_folder_BACKUP)),
                         ggplot2::aes(ymin = hourminute_ARCH, ymax = hourminute_ARCH+durationMinutes/60,
                                      xmin = daynum+.15, xmax = daynum+.45),
                         fill = xnat_color$arch$bg, color = 'red') +

      #xnat start points
      ggplot2::geom_point(data = subset(xd, !is.na(path_folder_BACKUP)),
                          ggplot2::aes(x = daynum-.3, y = hourminute_BACKUP)) +
      ggplot2::geom_point(data = subset(xd, !is.na(path_folder_ARCH)),
                          ggplot2::aes(x = daynum+.3, y = hourminute_ARCH)) +
      #xnat text
      ggplot2::geom_text(data = subset(xd, !is.na(path_folder_BACKUP)),
                         ggplot2::aes(x = daynum+textshift, y = hourminute_BACKUP+.2, label = studDesc_short),
                         color = 'black') +
      ggplot2::geom_text(data = subset(xd, !is.na(path_folder_ARCH)),
                         ggplot2::aes(x = daynum+textshift, y = hourminute_ARCH+.2, label = studDesc_short),
                         color = 'black') +
      ggplot2::geom_text(data = subset(xd, !is.na(path_folder_BACKUP)),
                         ggplot2::aes(x = daynum+textshift, y = hourminute_BACKUP+.4, label = patientName),
                         color = 'black') +
      ggplot2::geom_text(data = subset(xd, !is.na(path_folder_ARCH)),
                         ggplot2::aes(x = daynum+textshift, y = hourminute_ARCH+.4, label = patientName),
                         color = 'black') +
      # hour and day axes
      ggplot2::scale_y_continuous(breaks = hourlines$start:hourlines$end, trans = 'reverse',
                                  labels =  hourlines$labels) +
      ggplot2::scale_x_continuous(breaks = c(daynum-.3,daynum,daynum+.3), position = 'top',
                                  labels = c('Backup','Prearch','Archive')) +
      ggplot2::labs(x = '', y = '', title = mytitle ) +
      ggplot2::coord_cartesian(xlim = c(daynum-.5, daynum+textshift+.5))

    ggplot2::ggsave(file.path(figdir, myfilename), width = 5, height = 6)


  return(figure_filenames)

}




notafun2 <- function(){

library(ggplot2)

xnat_data_arch <- readRDS(file.path('//kumc.edu/data/Research/Hoglund/Bartolotti_J/3T_Scan_Logs/Reports/08132023_08202023_xnatdata_arch.rds'))


mydays <- unique(ilab_data$date)

#ggplot(ilab_data, aes(x = daynum, y =  hourday)) + theme_bw() +
#  geom_point() +
#  geom_point(aes(y = hourday + quantity), color = 'red') +

#ggplot(ilab_data, aes(x = daynum, y =  hourday)) + theme_bw() +
#  geom_rect(aes(ymin = hourday, ymax = hourday+quantity, xmin = daynum-.4, xmax = daynum), fill = 'red') +
#  geom_point() +
#  scale_y_reverse() +
#  geom_hline(yintercept = seq(from=7, to = 19, by = 1))

xnat_data <- subset(xnat_data, modality == 'MR')

xnat_data$daynum <- NA
xnat_data$daynum[xnat_data$serDate == '20230814'] <- 1
xnat_data$daynum[xnat_data$serDate == '20230815'] <- 2
xnat_data$daynum[xnat_data$serDate == '20230816'] <- 3
xnat_data$daynum[xnat_data$serDate == '20230817'] <- 4
xnat_data$daynum[xnat_data$serDate == '20230818'] <- 5

xnat_data$hourday <- (xnat_data$epochSer/(60*60)-5) %% (24)
xnat_data$durationMinutes <- as.numeric(xnat_data$durationMinutes)

xnat_data$studDesc_short <- unlist(lapply(xnat_data$studDesc, function(x){
splt <- strsplit(x, ' ')[[1]]
return(paste(unlist(lapply(splt, function(xx){
  substr(xx,1,min(5,nchar(xx)))})), collapse = ' '))
  }))
xnat_data$studDesc_short[grepl('[0-9]',xnat_data$studDesc)] <- xnat_data$studDesc[grepl('[0-9]',xnat_data$studDesc)]
xnat_data$studDesc_short <- unlist(lapply(xnat_data$studDesc_short, function(x){substr(x,1,18)}))



xnat_data_arch <- subset(xnat_data_arch, modality == 'MR')
xnat_data_arch$daynum <- NA
xnat_data_arch$daynum[xnat_data_arch$serDate == '20230814'] <- 1
xnat_data_arch$daynum[xnat_data_arch$serDate == '20230815'] <- 2
xnat_data_arch$daynum[xnat_data_arch$serDate == '20230816'] <- 3
xnat_data_arch$daynum[xnat_data_arch$serDate == '20230817'] <- 4
xnat_data_arch$daynum[xnat_data_arch$serDate == '20230818'] <- 5

xnat_data_arch$hourday <- (xnat_data_arch$epochSer/(60*60)-5) %% (24)
xnat_data_arch$durationMinutes <- as.numeric(xnat_data_arch$durationMinutes)

xnat_data_arch$studDesc_short <- unlist(lapply(xnat_data_arch$studDesc, function(x){
  splt <- strsplit(x, ' ')[[1]]
  return(paste(unlist(lapply(splt, function(xx){
    substr(xx,1,min(5,nchar(xx)))})), collapse = ' '))
}))
xnat_data_arch$studDesc_short[grepl('[0-9]',xnat_data_arch$studDesc)] <- xnat_data_arch$studDesc[grepl('[0-9]',xnat_data_arch$studDesc)]
xnat_data_arch$studDesc_short <- unlist(lapply(xnat_data_arch$studDesc_short, function(x){substr(x,1,18)}))






ggplot(ilab_data, aes(x = daynum-.2, y =  hourday)) + theme_bw() +
  geom_rect(aes(ymin = hourday, ymax = hourday+quantity, xmin = daynum-.4, xmax = daynum), fill = '#FFAAAA') +
  geom_point() +
  geom_hline(yintercept = seq(from=7, to = 19, by = 1)) +
  geom_rect(data = xnat_data, aes(ymin = hourday, ymax = hourday+durationMinutes/60, xmin = daynum, xmax = daynum+.4), fill = '#AAAAFF') +
  geom_point(data = xnat_data, aes(x = daynum+.2, y = hourday)) +
  geom_text(aes(x = daynum-.2, y = hourday+.2, label = study_id_short), color = '#AA0000') +
  geom_text(data = xnat_data, aes(x = daynum+.2, y = hourday+.2, label = studDesc_short), color = '#0000AA') +
  scale_y_continuous(breaks = 7:19, trans = 'reverse', labels = c(7:12,1:7))
ggsave('sampleweek.png', width = 14, height = 8)


}


