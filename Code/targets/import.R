csv_read_in <- function(csv_dir){
  detections <- list.files(csv_dir, full.names = T, pattern = '*.csv')
  
  if(length(detections) == 0){
    data.table::data.table()
  }else{
    # Read files into the elements of a list
    detections <- lapply(detections, fread, fill = T,
                         # rename columns
                         col.names = function(x) tolower(gsub('and|UTC|[) (\\.]', '', x)),
                         na.strings = c(NA, ''))
    
    # Bind list together into a data.table
    detections <- rbindlist(detections)
    
    detections
  }
}



make_station_key <- function(receiver_deployment){
  matos <- read_excel(receiver_deployment,
                      sheet = 2, skip = 3)
  
  setDT(matos)
  setnames(matos, function(x) tolower(gsub(' .*', '', x)))
  matos <- matos[!is.na(otn_array)]
  
  matos[, deploy_date_time := as.POSIXct(deploy_date_time, tz = 'UTC',
                                         format = '%Y-%m-%dT%H:%M:%S')]
  matos[, recover_date_time := as.POSIXct(recover_date_time, tz = 'UTC',
                                          format = '%Y-%m-%dT%H:%M:%S')]
  
  matos[!is.na(deploy_date_time),
        .(stationname = station_no,
          receiver = paste(ins_model_no, ins_serial_no, sep = '-'),
          internal_transmitter = transmitter,
          deploy_date_time, deploy_lat, deploy_long,
          recover_date_time)]
}



clean_detections <- function(csv_data, station_key){
  
  # prep for data.table::foverlaps
  # copy data as to not change the original file in memory
  station_copy <- copy(station_key)
  station_copy <- station_copy[!(is.na(recover_date_time) |
                                 grepl('CPOD', receiver))]
  setkey(station_copy, receiver, deploy_date_time, recover_date_time)
  
  # copy data as to not change the original file in memory
  detections <- copy(csv_data)
  
  # Since the files were read in by a loop, there may be redundant detections
  detections <- unique(detections, by = c('datetime', 'receiver', 'transmitter'))
  detections[, dummy_date := datetime]
  
  setkey(detections, receiver, datetime, dummy_date)
  
  # conduct data.table::foverlaps
  detections <- foverlaps(detections,
                          station_copy,
                          nomatch = 0)
  
  detections[, ':='(latitude = fifelse(is.na(latitude), deploy_lat, latitude),
                    longitude = fifelse(is.na(longitude), deploy_long, longitude))]
  
  
  detections[, ':='(sync = fifelse(transmitter %in%
                                     unique(station_copy$internal_transmitter),
                                   TRUE, FALSE),
                    self = fifelse(transmitter == internal_transmitter,
                                   TRUE, FALSE))]
  
  # Clean up redundant columns
  detections[, .(datetime, stationname, receiver,
                 transmitter, sensorvalue, sensorunit,
                 longitude, latitude, sync, self)]
}



act_fix <- function(act){
  # library(readxl); library(data.table)
  
  # Read in file
  act <- read_excel(act,
                    na = c('', 'NA', '`', '-'),
                    guess_max = 6000)
  
  # Convert to data.table
  setDT(act)
  
  
  # Repair names
  ## Convert to lower case
  setnames(act, tolower)
  
  ## Remove dashes
  setnames(act, function(.) gsub('-', '', .))
  
  ## Convert Roman numerals
  setnames(act, function(.) gsub(' I$', '1', .))
  setnames(act, function(.) gsub(' II$', '2', .))
  
  ## Convert spaces, periods, and slashes to empty
  setnames(act, function(.) gsub('[ \\./]', '', .))
  
  act <- act[, !c('idstandard', 'idsensori', 'idsensorii')]
  
  setnames(act, 'tagidcodestandard', 'transmitter')
  
  act[, releasedate := as.Date(releasedate)]
  
  # Write to CSV
  # fwrite(act, 'act-matos/active_transmitters_2021108.csv')
  act
}



tag_matching <- function(detections, act, tag_lists){
  # library(readxl); library(data.table)
  
  # Pull out great white shark detections
  gws <- tar_read(tag_lists,
                  branches = grep('WS.*List', tar_read(tag_lists)))
  
  gws <- setDT(
    read_excel(gws)
  )
  
  setnames(gws, function(x) tolower(gsub('and|UTC|[) (\\.]', '', x)))
  setnames(gws, 'datetagged', 'releasedate')
  
  gws[, ':='(primaryresearcher = 'Greg Skomal',
             commonname = 'white shark',
             releasedate = as.Date(releasedate))]
  
  gws_matched <- detections[sync  == FALSE] |> 
    DT(gws, on = 'transmitter', nomatch = 0)
  
  
  
  
  # Pull out ACT detections
  unmatched <- detections[sync  == FALSE] |> 
    DT(!gws, on = 'transmitter')
  
  act_matched <- unmatched[act, on = 'transmitter',
                           nomatch = 0]
  
  
  # Pull out detections matched by Innovasea
  unmatched <- unmatched[!act_matched, on = 'transmitter']
  
  innovasea <- tar_read(tag_lists,
                        branches = grep('Innovasea',
                                        tar_read(tag_lists)))
  innovasea <- copy(fread(innovasea, na.strings = c('', NA)))
  setnames(innovasea,
           c('taggingdate', 'datahandler', 'notes'),
           c('releasedate', 'collaborators', 'comments'))
  innovasea[, ':='(tl_m = tl / 100,
                   tl = NULL,
                   releasedate = as.Date(releasedate))]
  
  innovasea_matched <- unmatched[innovasea, on = 'transmitter',
                                 nomatch = 0]
  
  unmatched <- unmatched[!innovasea, on = 'transmitter']
  
  
  # Check for previously-matched detections via MATOS/OTN
  matos <- tar_read(tag_lists,
                    branches = grep('qualified',
                                    tar_read(tag_lists)))
  
  matos <- lapply(matos, fread, na.strings = '')
  matos <- rbindlist(matos)
  
  setnames(matos,
           c('fieldnumber', 'trackercode'),
           c('transmitter', 'network'))
  matos <- unique(matos, by = 'transmitter')
  
  matos[, primaryresearcher := fifelse(is.na(tag_contact_poc),
                                       tag_contact_pi,
                                       tag_contact_poc)]
  matos[, primaryresearcher := gsub(' \\(.*', '', primaryresearcher)]
  
  matos <- matos[, .(network, transmitter, primaryresearcher)]
  
  matos_matched <- unmatched[matos, on = 'transmitter',
                             nomatch = 0]
  
  unmatched <- unmatched[!matos, on = 'transmitter']
  
  # Bring it all together
  all_matched <- rbind(gws_matched, act_matched,
                       innovasea_matched, matos_matched, fill = T)
  all_matched[, commonname := tolower(commonname)]
  all_matched[, commonname :=
                fifelse(commonname %in% c('sturgeon', 'not identified'),
                        'atlantic sturgeon', commonname)]
  
  list(
    matched = all_matched,
    unmatched = unmatched
  )
  
}
