if (!isGeneric('getSoloLog')) {
  setGeneric('getSoloLog', function(x, ...)
    standardGeneric('getSoloLog'))
}
#' download tlog files from solo and convert them to gpx files
#'
#' @description  getSoloLog is interfacing the mavtogpx.py converter
#'
#' @param option
#' 
#' @author
#' Chris Reudenbach
#' @note for using the solo stuff you need to install: sudo pip install pymavlink; sudo pip install dronekit-sitl; sudo pip install dronekit; sudo apt-get install sshpass
#' @examples
#' getSoloLog(logDir="~tmp/solo/")
#' 
#' @export getSoloLog
#'               

getSoloLog <- function(logFiles="solo.t*",logDir="soloLog"){
  
  if (!file.exists(file.path(logDir))) {
    dir.create(file.path(logDir), recursive = TRUE)
  }
  command <-'mavtogpx.py'
  option1<-paste0(logDir,"/",logFiles)
  
  log<-system( paste0("sshpass -p 'TjSDBkAu'  scp 'root@10.1.1.1:/log/",logFiles,"' ",logDir,"/. " ))
  log<-system(paste(command, option1))
  
  #flight.gpx <- rgdal::readOGR(dsn = "/home/creu/tmp/solo/solo.tlog.gpx", layer="tracks")
}