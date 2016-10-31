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
      f <- list.files(logDir, pattern=logFiles)
      cat("Download from solo controllor seems to be ok")
    } else {
      cat('FATAL', "### can not find/read input file")        
      stop("### could not read any log data")
    }  
    
  }
  
  test<-system2("mavtogpx.py","-h",stdout = TRUE)
  if (grep("usage: mavtogpx.py",test)){
    cat("pymavlink seems to be installed\n")
    test<-system2(command, option1,stdout = TRUE)
    cat(test)
    } else {
    stop("No pymavlink lib. Try: sudo pip install pymavlink\n")
  }
  
  
  #flight.gpx <- rgdal::readOGR(dsn = "/home/creu/tmp/solo/solo.tlog.gpx", layer="tracks")
  

  
 
}