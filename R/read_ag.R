#' Function to read in ActiGraph accelerometer data.
#' Filepath provided should be to an excel .csv format file
#' Accelerometer data should be exported with steps, lux, and inclinometer data. Otherwise, may result in improper column naming
#'
#' @param filepath Character string of complete filepath for a single ActiGraph accelerometer excel file. May be raw or count-based data
#' @param ENMO_calibrate  Binary indicator (T/F) of whether ENMO calibrated acceleration data should be computed. Uses g.calibrate function from GGIR (broken with recent GGIR updates)
#' @param device_serial_calibrate Binary indicator (T/F) of whether to use device-specific correction factor coefficients when there is insufficient non-movement periods in the data file for auto-calibration
#' @param calibration_file Data frame containing device serial numbers and device-specific correction factor coefficients
#' @param parse_timestamp Binary indicator (T/F) of whether the Timestamp should be separated into Date and Time columns. For larger files, this extends the read and return time.
#'
#'
read_ag = function(filepath, ENMO_calibrate = T, device_serial_calibrate = T, calibration_file, parse_timestamp = F){

  check_data = fread(filepath,header = F,skip = 10, nrows = 1)[1,]
  if(is.numeric(check_data[1,])){
    file_data = fread(filepath,header = F, skip = 10, stringsAsFactors = F)
  } else {
    file_data = fread(filepath,header = T, skip = 10, stringsAsFactors = F)
  }

  ag_header = read.csv(filepath, header = F,stringsAsFactors = F, nrows = 10)
  device_serial = str_split(ag_header[2,],'Number: ')[[1]][2]
  start = str_split(ag_header[3,],'Time ')[[1]][2]
  date = str_split(ag_header[4,],'Date ')[[1]][2]
  frequency = as.numeric(str_split(str_split(ag_header[1,],'at ')[[1]][3],' Hz')[[1]][1])

  epoch_full = str_split(ag_header[5,],' 00:')[[1]][2]
  epoch_temp = str_split(epoch_full,':')
  epoch = (as.numeric(epoch_temp[[1]][1])*60) + (as.numeric(epoch_temp[[1]][2]))

  file_length = nrow(file_data)
  date_time_start = mdy_hms(paste(date,start, sep = ' '))

  if(epoch < 1){
    if(any(colnames(file_data) == 'Timestamp')){
      file_data = file_data %>% dplyr::select(-Timestamp)
    }

    # For Raw data
    file_data = file_data %>% mutate(`Accelerometer X` = as.numeric(`Accelerometer X`),
                                     `Accelerometer Y` = as.numeric(`Accelerometer Y`),
                                     `Accelerometer Z` = as.numeric(`Accelerometer Z`))

    file_data = mutate(file_data,
                       VM = sqrt(`Accelerometer X`^2 + `Accelerometer Y`^2 + `Accelerometer Z`^2),
                       VMcorrG = abs(sqrt(`Accelerometer X`^2 + `Accelerometer Y`^2 + `Accelerometer Z`^2)-1))

    colnames(file_data) = c('AxisX','AxisY','AxisZ','VM','VMcorrG')

    if(ENMO_calibrate == T){
      if(nrow(file_data) > 12*60*60*frequency){
        C = g.calibrate(filepath,use.temp = F, printsummary=F)
      } else {
        C = list(offset = c(0,0,0),
                 scale = c(1,1,1))
      }

      if(device_serial_calibrate == T){
        if(C$offset[1] == 0 & C$scale[1] == 1){

          device_serial_index = str_which(calibration_file$Serial,device_serial)

          if(length(device_serial_index) == 0){

          } else {
            C$offset[1] = calibration_file$Offset_X[device_serial_index]
            C$offset[2] = calibration_file$Offset_Y[device_serial_index]
            C$offset[3] = calibration_file$Offset_Z[device_serial_index]

            C$scale[1] = calibration_file$Scale_X[device_serial_index]
            C$scale[2] = calibration_file$Scale_Y[device_serial_index]
            C$scale[3] = calibration_file$Scale_Z[device_serial_index]

          }
        }
      }

      file_data = mutate(file_data,
                         calX = AxisX*C$scale[1] + C$offset[1],
                         calY = AxisY*C$scale[2] + C$offset[2],
                         calZ = AxisZ*C$scale[3] + C$offset[3],
                         ENMO = (sqrt(calX^2 + calY^2 + calZ^2)-1)*1000) # convert to milli-gravitational units

      file_data = mutate(file_data, ENMO = ifelse(ENMO < 0,0,ENMO))

      colnames(file_data) = c('AxisX','AxisY','AxisZ', 'VM','VMcorrG','calX','calY','calZ','ENMO')
    }

    Timestamp = seq(from = date_time_start,to = (date_time_start + (file_length/frequency)), by = 1/frequency)[1:nrow(file_data)]

  } else {
    # If Count data does not have column names when being read in
    if(is.numeric(check_data[1,])){
      colnames(file_data) <- c('Axis1', 'Axis2','Axis3', 'Steps', 'Lux','Inclinometer Off','Inclinometer Standing','Inclinometer Sitting','Inclinometer Lying')[1:ncol(file_data)] # hardcoded column names may result in issues
    }

    frequency = epoch

    file_data = mutate(file_data, VM = sqrt(Axis1^2 + Axis2^2 + Axis3^2))

    Timestamp = seq(from = date_time_start,to = (date_time_start + file_length*epoch), by = epoch)[1:nrow(file_data)]

  }



  if(parse_timestamp == T){
    new_data = cbind(filename = basename(filepath),
                      Timestamp = Timestamp,
                      Date = lubridate::date(Timestamp),
                      Time = format(Timestamp, format = "%H:%M:%S"),
                      file_data)
    colnames(new_data) = c('Filename','Timestamp','Date','Time',colnames(file_data))

  } else {
    file_data = cbind(filename = basename(filepath),
                      Timestamp = Timestamp,
                      file_data)

  }

  return(file_data)
}
