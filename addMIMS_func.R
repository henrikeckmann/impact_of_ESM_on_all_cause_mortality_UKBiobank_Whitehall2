library(GGIR)
library(MIMSunit)
library(dplyr)

# external function to loop through files and keep track of errors
addMIMS <- function(
    datadir, outputdir, dynrange, desiredtz, configtz, overwrite=FALSE
  ){
  
  # Convert paths from Windows specific slashed to generic slashes
  outputdir = gsub(pattern = "\\\\", replacement = "/", x = outputdir)
  datadir = gsub(pattern = "\\\\", replacement = "/", x = datadir)
  
  # Tilde-expansion
  outputdir = path.expand(outputdir)
  datadir = path.expand(datadir)
  print(paste("OUTPUTDIR PATH AFTER EXPANSION =", outputdir))
  print(paste("DATADIR PATH AFTER EXPANSION =", datadir))
  
  # handle paths with/without "/" at the end, and generate metadatadir
  datadir = paste(unlist(strsplit(datadir, "/")), collapse = "/")
  outputdir = paste(unlist(strsplit(outputdir, "/")), collapse = "/")
  
  outputfoldername = unlist(
    strsplit(datadir, "/")
  )[length(unlist(strsplit(datadir, "/")))]
  
  metadatadir = paste(
    outputdir, "/output_", outputfoldername, "/meta/basic", sep = ""
  )
  
  # read in list of meta files in metadatadir
  filenames = list.files(
    path=metadatadir, 
    pattern=NULL, 
    all.files=FALSE, 
    full.names=FALSE,
  )
  
  file_n = 1 # counter
  
  #create output sink
  con_out_file <- file(
    paste0(sub("meta/basic", "", metadatadir), "console_output.txt"),
    open = "a"
    )
  sink(con_out_file, append = TRUE, type = "output", split = TRUE)
  print("")
  print("")
  print(Sys.time())
  print("STARTING NEW RUN")
  print(paste("OUTPUTDIR PATH AFTER EXPANSION =", outputdir))
  print(paste("DATADIR PATH AFTER EXPANSION =", datadir))
  print(paste("METADATDIR PATH =", metadatadir))
  
  if (file.exists(
    paste(sub("meta/basic", "", metadatadir), "addMIMS_errors.csv")
    ) 
  ){
    errors_df <- read.csv(
      paste(sub("meta/basic", "", metadatadir), "addMIMS_errors.csv")
    )
    errors <- c(errors_df$x, "FLAG, NEW RUN")
    rm(errors_df)
  } else {
    errors <- c() # vector to keep track of files that throw an error
  }
  err_length_start <- length(errors)
  
  if (file.exists(
    paste0(sub("meta/basic", "", metadatadir), "overwrites_skipped.csv")
    ) 
  ){
    overwrites_df <- read.csv(
      paste0(sub("meta/basic", "", metadatadir), "overwrites_skipped.csv")
    )
    overwrites_skipped <- c(overwrites_df$x, "FLAG, NEW RUN")
    rm(overwrites_df)
  } else {
    overwrites_skipped <- c() # vector to keep track of files that are skipped due to overwrite check 
  }
  overwrites_length_start <- length(overwrites_skipped)
  
  # loop over metafiles
  for (filename in filenames){
    
    print(
      paste(
        format(Sys.time(), '%H:%M:%S'),
        "running file", file_n, "out of", length(filenames)
      )
    )
    
    print(filename)
    
    # try running internal addMIMS_internal function
    # and catch any errors
    err <- tryCatch(
      {
        addMIMS_internal(
          datadir = datadir,
          metadatadir = metadatadir,
          dynrange = dynrange,
          filename = filename,
          desiredtz = desiredtz,
          configtz = configtz,
          overwrite = overwrite
        )
      }, error = function(e){
        print("ERROR, SKIPPING FILE")
        return(TRUE)
      }
    )
    
    # if file throws an error, add filename to errors vector
    if (err == TRUE){
      errors <- c(errors, filename)
    } else if (err == "OVERWRITE"){
      overwrites_skipped <- c(overwrites_skipped, filename)
    } 
    file_n = file_n + 1 # increment counter
  }
  
  err_length <- length(errors) - err_length_start # count of errors, for final print statement
  
  # if no files have errored, add a line to vector saying no errors
  if (err_length == 0){
    errors <- c(errors, "NO addMIMS_func ERRORS")
  }
  
  # print .csv with files that errored
  write.csv(
    errors, paste(sub("meta/basic", "", metadatadir), "addMIMS_errors.csv")
  )
  
  overwrites_length <- length(overwrites_skipped) - overwrites_length_start # count of errors, for final print statement
  
  # if no files have errored, add a line to vector saying no errors
  if (overwrites_length == 0){
    overwrites_skipped <- c(overwrites_skipped, "NO FILES SKIPPED DUE TO OVERWRITE CHECK")
  }
  
  # print .csv with files that were skipped because they were already written 
  write.csv(
    overwrites_skipped, paste0(sub("meta/basic", "", metadatadir), "overwrites_skipped.csv")
  )
  
  print(format(Sys.time(), '%H:%M:%S'))
  print(paste(
    "COMPLETED ALL",
    length(filenames), 
    "FILES IN METAFILE DIRECTORY WITH",
    err_length,
    "ERRORS."
    )
  )
  
  rm(err_length); rm(err_length_start)
  rm(overwrites_length); rm(overwrites_length_start)
  
  # close output sink 
  sink()
  close(con_out_file)
}

# internal function that loads each metafile and adds MIMS vector
addMIMS_internal <- function(
    datadir, metadatadir, dynrange, filename, desiredtz, configtz, overwrite
  ){
  
  # load metafile
  print(paste(format(Sys.time(), '%H:%M:%S'), "load metafile"))
  load(paste(metadatadir, "/", filename, sep = ""))
  
  if (M$filecorrupt == TRUE){
    print(paste(filename, "CORRUPT"))
    return(TRUE)
  }
  if (M$filetooshort == TRUE){
    print(paste(filename, "FILE TOO SHORT"))
    return(TRUE)
  }
  if (overwrite == FALSE){
    if ("LFENMO" %in% colnames(M$metashort)){
      print(paste(filename, "ALREADY CONTAINS 'LFENMO' VECTOR. SKIPPING."))
      return("OVERWRITE")
    }
  }
  
  # rawfile name from metafile name
  rawfile <- sub(".RData", "", filename)
  rawfile <- sub("meta_", "", rawfile)
  rawfile <- paste(datadir, "/", rawfile, sep = "")
  
  # prepare necessary parameters for g.readaccfile
  ws <- M$windowsizes[3]
  ws2 <- M$windowsizes[2]
  filequality <- data.frame(
    filetooshort = M$filetooshort, filecorrupt = M$filecorrupt,
    filedoesnotholdday = FALSE, NFilePagesSkipped = M$NFilePagesSkipped
  )
  
  hvars = g.extractheadervars(I)
  deviceSerialNumber = hvars$deviceSerialNumber
  rm(hvars); gc()
  
  # get now-wear, clip, and blocksize parameters (thresholds)
  ncb_params = get_nw_clip_block_params(
    chunksize = 1,
    dynrange = NULL,
    monc = I$monc, dformat = I$dformc,
    deviceSerialNumber = deviceSerialNumber,
    rmc.noise = c(),
    sf = I$sf,
    rmc.dynamic_range = c()
  )
  
  blocksize = ncb_params$blocksize * 15
  rm(ncb_params); gc()
  
  # read raw acceleration
  print(paste(format(Sys.time(), '%H:%M:%S'), "accread"))
  header <- NULL
  PreviousLastValue = c(0, 0, 1)
  PreviousLastTime = NULL
  PreviousEndPage = c()
  i = 1
  
  accread = g.readaccfile(
    filename = rawfile, blocksize = blocksize, blocknumber = i,
    filequality = filequality, ws = ws, PreviousEndPage = PreviousEndPage,
    inspectfileobject = I, PreviousLastValue = PreviousLastValue,
    PreviousLastTime = PreviousLastTime, header = header
  )
  
  rm(filequality); rm(header); rm(i); rm(PreviousEndPage);
  rm(PreviousLastTime); rm(PreviousLastValue); gc()
  
  # CALIBRATE
  print(paste(format(Sys.time(), '%H:%M:%S'), "apply calibration"))
  temperaturecolumn = which(colnames(accread$P$data) == "temperature") 
  
  yy = as.matrix(cbind(
    as.numeric(accread$P$data[,temperaturecolumn]),
    as.numeric(accread$P$data[,temperaturecolumn]),
    as.numeric(accread$P$data[,temperaturecolumn]))
    )
  
  accread$P$data[,2:4] = scale(
    as.matrix(accread$P$data[,2:4]), center = -C$offset, scale = 1/C$scale
    ) + scale(
      yy, center = rep(C$meantempcal,3), scale = 1/C$tempoffset
      )
  
  rm(yy); gc()
  
  # align data start time
  SWMT <- get_starttime_weekday_truncdata(
    I$monc, I$dformc, accread$P$data, header, desiredtz = desiredtz,
    I$sf, rawfile,  ws2, configtz = configtz
  )
  
  starttime <- SWMT$starttime
  data <- SWMT$data
  
  rm(accread); rm(SWMT); gc()
  
  # fix timevector format for MIMSunit
  starttime_num = as.numeric(starttime)
  endtime_num = starttime_num + nrow(data) / I$sf 
  endtime = as.POSIXct(endtime_num)
  time = seq(from = starttime, to = endtime, length.out = nrow(data)+1)
  time = time[-length(time)]
  data$HEADER_TIME_STAMP = time
  rm(time); rm(starttime); rm(starttime_num); rm(endtime); rm(endtime_num); gc()
  
  # prepare data format for MIMS function
  data <- select(data, HEADER_TIME_STAMP, x, y, z)
  data <- rename(data, X = x, Y = y, Z = z)
  
  # split data into 3 chunks
  data1_rows <- 1:(nrow(data)/3 - (nrow(data)/3)%%(I$sf*10))
  data2_rows <- (length(data1_rows)+1):(length(data1_rows)*2)
  
  data1 <- data[data1_rows,]
  data2 <- data[data2_rows,]
  data3 <- tail(data, (nrow(data) - length(data1_rows) - length(data2_rows)))
  rm(data); rm(data1_rows); rm(data2_rows); gc()
  epoch = paste(M$windowsizes[1], "sec")
  
  # generate MIMS
  print(paste(format(Sys.time(), '%H:%M:%S'), "generate mims"))
  mims <- genMIMS(
    data1,
    before_df = NULL,
    after_df = head(data2, I$sf * 100),
    dynrange = dynrange,
    epoch = epoch
  )
  
  mims2 <- genMIMS(
    data2,
    before_df = tail(data1, I$sf * 100),
    after_df = head(data3, I$sf * 100),
    dynrange = dynrange,
    epoch = epoch
  )
  
  mims <- c(mims, mims2)
  rm(mims2); rm(data1); gc()
  
  mims3 <- genMIMS(
    data3,
    before_df = tail(data2, I$sf * 100),
    after_df = NULL,
    dynrange = dynrange,
    epoch = epoch
  )
  mims <- c(mims, mims3)
  rm(mims3); rm(data2); rm(data3); gc()

  # truncating if mims vector longer than metashort as GGIR will cut off ends of files
  mims <- mims[1:nrow(M$metashort)]
  
  # calling MIMS "LFENMO" to get GGIR part 2 to do IG
  # as it won't do it to embedded functions
  M$metashort$LFENMO <- mims 
  
  # save metafile and clean up before next loop
  print(paste(format(Sys.time(), '%H:%M:%S'), "saving"))
  save(M, I, C, filename_dir, filefoldername, tail_expansion_log,
       file = paste(metadatadir, "/", filename, sep = ""))
  rm(M); rm(I); rm(C); rm(mims); rm(blocksize);
  rm(deviceSerialNumber); rm(epoch); rm(filefoldername); rm(filename_dir);
  rm(rawfile); rm(tail_expansion_log); rm(temperaturecolumn);
  rm(ws); rm(ws2)
  gc()
  print(paste(format(Sys.time(), '%H:%M:%S'), "done"))
  return(FALSE)
}

# internal wrapper function for mims_unit function from MIMSunit package
# simply discards dataframe only keeping MIMS vector, rounded to 4 decimals
genMIMS <- function(data, before_df, after_df, dynrange, epoch){
  mims_df <- mims_unit(
    data,
    before_df = before_df,
    after_df = after_df,
    dynamic_range = dynrange, 
    epoch = epoch,
    #use_snapshot_to_check = TRUE,
  )
  mims <- round(mims_df$MIMS_UNIT, 4)
  return(mims)
}
