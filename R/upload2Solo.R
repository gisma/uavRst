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
#' @examples
#' 
#' 
#' 
#' upload2Solo("export_1001_solo.waypoints")
#' 
#' @export upload2Solo
#'               

upload2Solo <- function(missionFile=NULL,connection="udp:10.1.1.166:14550",prearm="-9"){
  
  
  command ='python'
  
  script <- paste(system.file(package="robubu"), "python/io_solo_mission.py", sep="/")
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