if (!isGeneric('upload2Solo')) {
  setGeneric('upload2Solo', function(x, ...)
    standardGeneric('upload2Solo'))
}
#' upload mission file to solo
#'
#' @description  upload2Solo provides a crude interface to upload the Solo mission file to the 3dr SOLO
#'
#' @param connection a valid connection string to the Solo default is "udp:10.1.1.166:14550"
#' @param prearm controls the prearm status of the Solo prearm check 0=Disabled,1=Enabled,-3=Skip Baro,-5=Skip Compass,-9=Skip GPS,-17=Skip INS,-33=Skip Params/Rangefinder,-65=Skip RC,127=Skip Voltage 
#' @param missionFile mission file to upload
#' 
#'
#'
#'@note #' @note for using the solo stuff you need to install: sudo pip install pymavlink; sudo pip install dronekit-sitl; sudo pip install dronekit; sudo apt-get install sshpass
#'
#' @examples
#' 
#' 
#' 
#' 
#' upload2Solo("export_1001_solo.waypoints")
#' 
#' @export upload2Solo
#'               

upload2Solo <- function(missionFile=NULL,connection="udp:10.1.1.166:14550",prearm="-9"){
  
  
  command ='python'
  
  script <- paste(system.file(package="uavRst"), "python/io_solo_mission.py", sep="/")
  #script='~/proj/drone/scripte/io_solo_mission.py'
  
  option1<-'--connect'
  connection<-connection
  option2<-'--prearm'
  prearm<-prearm
  option3<-'--mission'
  missionFile<-missionFile
  
  args = c(option1, connection,option2,prearm,option3,missionFile)
  
  # Add path to script as first arg
  allArgs = c(script, args)
  
  output = system2(command, args=allArgs, stdout=TRUE)
  
  print(paste("Solo returns:", output,"\n"))
}



if (!isGeneric('soloLog')) {
  setGeneric('soloLog', function(x, ...)
    standardGeneric('soloLog'))
}
#' Download , reorganize and export the telemetry (tlog) files from 3DR Solo (and Pixhawk) 
#'
#' @description  Wraps the mavtogpx.py converter as provided by the dronkit library. It downloads and/ or converts the 3DR Solo logfiles. Otionally you may import the geometries and data as sp objects in R
#'
#' @param logFiles pattern of which kind of logs should be downloaded for telemetry it is "solo.t*" which means all log files...
#' @param logDir (existing) destination path to which the logs should be downloaded to 
#' @param downloadOnly default = FALSE, set to TRUE  if you ONLY want to download the log files from the solo controller to the logDir
#' @param netWarn if true warns and waits before starting a connection to the controller to connect to the solo wifi
#' @param organize renames the log and gpx files according to their timeperiod
#' 
#' @param makeSP generates SP objects from the gpx files
#' 
#' @note for using the solo stuff you need to install: \cr 
#' sudo pip install pymavlink  \cr 
#' sudo pip install dronekit-sitl \cr 
#' sudo pip install dronekit \cr 
#' sudo apt-get install sshpass \cr 
#' 
#' @examples
#' 
#' ## download current telemetry log file from controller and convert it to gpx
#' soloLog(logDir="~/tmp/solo",logFiles = "solo.tlog")
#' 
#' ## download all available telemetry logfiles from the controller
#' soloLog(logDir="~/tmp/solo")
#' 
#' ## download ALL logfiles from the controller
#' soloLog(logDir="~/tmp/solo", logFiles = "*")
#' 
#' @export soloLog
#'               

soloLog <- function(logFiles="solo.t*",
                    logDir="~/soloLog", 
                    downloadOnly=FALSE,
                    netWarn=TRUE,
                    organize=TRUE,
                    makeSP = FALSE){
  
  logDir<- path.expand(logDir)
  command <-"mavtogpx.py"
  option1<-paste0(logDir,"/",logFiles)
  
  if (!file.exists(file.path(logDir))) {
    dir.create(file.path(logDir), recursive = TRUE)
  }
  
  
  invisible(readline(prompt="Press [enter] to continue\n The controller shutdown after a while - check connection\n"))
  cat("downloading and converting will take a while without prompting anything...\n be patient in the end you will know.\n")
  log<-system( paste0("sshpass -p 'TjSDBkAu'  scp 'root@10.1.1.1:/log/",logFiles,"' ",logDir,"/. " ),wait=TRUE)
  if (log == 0) {
    f <- list.files(logDir, pattern=extension(logFiles))
    cat(f," downloaded...\n")
    cat("Download from solo controllor seems to be ok\n")
  } else {
    cat('FATAL', "### can not find/read input file")        
    stop("### could not read any log data\n")
  }  
  if (downloadOnly){
    cat("All logs downloaded...")
    exit
  } 
  
  test<-system2("mavtogpx.py","-h",stdout = TRUE)
  if (grep("usage: mavtogpx.py",test)){
    cat("pymavlink seems to be installed\n")
    cat("converting log files to to gpx...\n")
    test<-system2(command, option1,stdout = TRUE)
    cat(test)
  } else {
    stop("No pymavlink lib. Try: sudo pip install pymavlink\n")
  }
  
  
  if (organize) {
    cat("rename files...\n")
    f <- list.files(logDir, pattern="gpx")
    
    i=1
    flights <- list()
    for (flight in f) {
      f <- h_read_gpx(path.expand(paste0(logDir,"/",flight)))
      flights[[i]]<-f
      firstTime<-as.character(flights[[i]]$track_points@data$time)[1]
      lastTime<-as.character(flights[[i]]$track_points@data$time)[length(flights[[i]]$track_points@data$time)]
      la1<-gsub(x = lastTime, pattern = "\\/",replacement = "")
      la2<-gsub(x = la1, pattern = "\\ ",replacement = "_")
      la3<-substr(gsub(x = la2, pattern = "\\:",replacement = "-"),10,17)
      
      fi1<-gsub(x = firstTime, pattern = "\\/",replacement = "")
      fi2<-gsub(x = fi1, pattern = "\\ ",replacement = "_")
      fi3<-substr(gsub(x = fi2, pattern = "\\:",replacement = "-"),1,17)
      logName<- paste0(logDir,"/",fi3,"_",la3,"_solo.tlog")
      gpxName<- paste0(logDir,"/",fi3,"_",la3,"_solo.gpx")      
      fNgpx <- paste0(logDir,"/",list.files(logDir, pattern="gpx"))
      fNlog <- paste0(logDir,"/",list.files(logDir, pattern="tlog",include.dirs = FALSE))
      if (!file.exists(logName)) file.rename(fNlog[i],logName)
      else {cat("you'd already converted ",logName,"\n")}
      if (!file.exists(gpxName)) file.rename(fNgpx[i],gpxName)
      else {cat("you'd already converted ",gpxName,"\n")}
      
      i=i+1
    }
    cat("All logfiles stored and coverted ...\n")
    if (makeSP) {
      cat("export as sp objects ...\n")
      return(flights)}
  }
}
