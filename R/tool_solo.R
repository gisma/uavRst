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
#' @author
#' Chris Reudenbach
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



if (!isGeneric('solo2gpx')) {
  setGeneric('solo2gpx', function(x, ...)
    standardGeneric('solo2gpx'))
}
#' download tlog files from solo and convert them to gpx files
#'
#' @description  solo2gpx is interfacing the mavtogpx.py converter
#'
#' @param logFiles pattern of which kind of logs should be downloaded for telemetry it is "solo.t*"
#' @param logDir path to the folder where the logs should be downloaded to
#' @param download boolean if you want to download log files from the solo controller 
#' @param netWarn if true warns and waits before starting a connection to the controller to connect to the solo wifi
#' @param import import of the selecected as sp objects
#' 
#' @author
#' Chris Reudenbach
#' @note for using the solo stuff you need to install: sudo pip install pymavlink; sudo pip install dronekit-sitl; sudo pip install dronekit; sudo apt-get install sshpass
#' @examples
#' 
#' ## download current telemetry log file from controller and convert it to gpx
#' solo2gpx(logDir="~/tmp/solo",logFiles = "solo.tlog")
#' 
#' ## download all available telemetry logfiles from the controller
#' solo2gpx(logDir="~/tmp/solo")
#' 
#' ## download ALL logfiles from the controller
#' solo2gpx(logDir="~/tmp/solo", logFiles = "*")
#' 
#' @export solo2gpx
#'               

solo2gpx <- function(logFiles="solo.t*",logDir="soloLog", download=TRUE,netWarn=TRUE,import=FALSE){
  command <-"mavtogpx.py"
  option1<-paste0(logDir,"/",logFiles)
  
  if (!file.exists(file.path(logDir))) {
    dir.create(file.path(logDir), recursive = TRUE)
  }
  
  if (download){
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
  if (import) {
    cat("read gpx to a nested list of sp objects\n")
    f <- list.files(logDir, pattern="gpx")
    i=1
    flights <- list()
    for (flight in f) {
    f <- h_read_gpx(path.expand(paste0(logDir,"/",flight)))
    flights[[i]]<-f
    i=i+1
    }
    return(flights)
  }
}
