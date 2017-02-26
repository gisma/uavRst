if (!isGeneric('t3p')) {
  setGeneric('t3p', function(x, ...)
    standardGeneric('t3p'))
}
#' t3p take tree top pictures
#'
#' @description  t3p generates a flight track chaining up point objects with respect to a heterogenous Surface and known obstacles for taking top down pictures.
#'
#' @note basic idea is to fly in a serie to object positions with respect to the surface model for taking high resolution pics
#' @param projectDir path to the main folder where several projects can be hosted
#' It will overwrite the DEM based estimation if any other value than -9999
#' @param demFn  filname of the corresponding DEM data file
#' @param missionName base string for mission filenames
#' @param followSurface  \code{boolean}  TRUE performs an altitude correction 
#' of the missions flight altitude using additional DEM data. 
#' If no DEM data is provided and \code{followSurface} is TRUE, 
#' SRTM data will be downloaded and used
#' Further explanation at \link{seealso}
#' @param presetFlightTask (DJI only) strongly recommended to use "remote" 
#'        \cr
#'  Options are: 
#' \code{"simple_ortho"} takes one picture/waypoint, 
#' \code{"multi_ortho"} takes 4 picture at a waypoint, two vertically down and two in forward and backward viewing direction and an angele of -60deg,
#' \code{"simple_pano"} takes a 360 deg panorama picture and 
#' \code{"remote"} which assumes that the camera is controlled by the remote control (RC)
#' @param flightAltitude set the default flight altitude of the mission. It is 
#'   assumed that the UAV is started at the highest point of the surveyArea 
#'   otherwise you have to defined the position of launching.
#' By default it is set to (\code{= 0.0}). If set to \code{-99} it will be 
#' calculated from the swath width of the pictures. NOTE This makes only sense for 
#' \code{followingTerrain = TRUE} to smooth curves.
#' For \code{flightPlanMode = "waypoint"} camera actions are DISABLED during curve flights.
#' @param rotationdir (DJI only) camera control parameter set the UAV basic turn direction to right (0) or left (1)
#' @param gimbalmode (DJI only) camera control parameter
#' \code{0} deactivates the gimbal control
#' \code{1} activates the gimbale for focussing POIs
#' \code{2} activates the gimbale for focus and interpolate a field of view in an angel of \code{gimbalpitchangle}
#' @param gimbalpitchangle (DJI only) vertical angle of camera  \code{+30 deg..-90 deg} 
#' @param actiontype (DJI only) individual actionype settings of the camera c(1,1,...)
#' @param actionparam (DJI only) corresponding parameter for the above individual actiontype c(0,0,...)
#' @param maxSpeed  cruising speed
#' @param followSurfaceRes horizontal step distance for analysing the DEM altitudes
#' @param windCondition 1= calm 2= light air 1-5km/h, 3= light breeze 6-11km/h, 4=gentle breeze 12-19km/h 5= moderate breeze 20-28km/h
#' @param rcRange range of estimated range of remote control 
#' @param uavType type of uav. currently "djip3" and "solo" are supported
#' @param maxFl maximum duration of a flight in minutes

#' @author
#' Chris Reudenbach
#'
#' @examples
#' t3<-t3p(projectDir ="/home/creu/uav/bayerwald",
#' missionName = "filzmoosTree",
#' missionTrackList="~/uav/bayerwald/Selected_trees_Filz.txt",
#' demFn = "~/uav/grossfilz/grosserfilz.tif",
#' windCondition = 2,
#' uavType = "djip3",
#' followSurfaceRes=5,
#' launchPos = c(13.409114897133804,48.92039612988935))
#' 
#' mapview(t3$wp,zcol = "altitude",lwd=1,cex=5)+mapview(t3$lp,color="red",cex=5)
#' 
#' @export t3p 
#'               

t3p<- function(projectDir="~",
               missionName="autoflightcontrol",
               missionTrackList=NULL,
               launchPos=NULL,
               demFn=NULL,
               flightAltitude=75,
               climbDist=7.5,
               aboveTreeAlt=15,
               presetFlightTask="remote",
               maxSpeed=25.0,
               followSurfaceRes=5,
               altFilter=1.0,
               maxFL=10,
               windCondition=1,
               rcRange=-9999,
               launchAltitude=-9999,
               uavType="djip3") {
  
  
  # assign flight mission name 
  mission<-paste(missionName, sep=.Platform$file.sep)
  
  workingDir<-missionName
  # create directories if needed
  if(!file.exists(file.path(projectDir, workingDir))){dir.create(file.path(projectDir, workingDir),recursive = TRUE)}
  if(!file.exists(file.path(projectDir, workingDir,"tmp"))){  dir.create(file.path(projectDir, workingDir,"/tmp"),recursive = TRUE)}
  if(!file.exists(file.path(projectDir, workingDir,"control"))) { dir.create(file.path(projectDir, workingDir,"control"),recursive = TRUE)}
  if(!file.exists(file.path(projectDir,"data"))){dir.create(file.path(projectDir,"data"),recursive = TRUE)}
  # setting R environ temp folder to the current working directory
  Sys.setenv(TMPDIR=file.path(projectDir, workingDir,"tmp"))
  
  # set R working directory
  setwd(file.path(projectDir, workingDir,"tmp"))
  
  Sys.chmod(list.dirs("../.."), "777")
  
  # create log file
  logger <- log4r::create.logger(logfile = paste0(file.path(projectDir, workingDir,"control/"),strsplit(basename(mission), "\\.")[[1]][1],'.log'))
  log4r::level(logger) <- "INFO"
  log4r::levellog(logger, 'INFO',"                                                           ")
  log4r::levellog(logger, 'INFO',"                                                           ")
  log4r::levellog(logger, 'INFO',"--------------------- START RUN ---------------------------")
  log4r::levellog(logger, 'INFO',paste("Working folder: ",file.path(projectDir, workingDir)))
  
  
  
  # create misson filename
  csvFn<- paste(file.path(projectDir, workingDir,"control"), paste0(mission,".csv"), sep=.Platform$file.sep)
  
  # import flight area if provided by an external vector file
  file.copy(overwrite = TRUE, from = missionTrackList, to = file.path(projectDir,"data"))
  test<-try(flightList<-robubu:::readTreeTrack(missionTrackList))
  if (class(test)!="try-error"){
    treeList<-flightList
  }
  else{
    log4r::levellog(logger, 'FATAL', "### can not find/read flight list")        
    stop("### could not read flight list")
  }
  test<-try(robubu:::readLaunchPos(launchPos))
  if (class(test)!="try-error"){
    launchPos<-test
  }
  else{
    log4r::levellog(logger, 'FATAL', "### can not find/read launchPosition")        
    stop("### could not read launchPosition")
  }
  #
  p<-list()
  p$launchLat<-launchPos@coords[2]
  p$launchLon<-launchPos@coords[1]
  p$missionName<-missionName
  p$missionTrackList<-missionTrackList
  p$demFn<-demFn
  p$flightAltitude<-flightAltitude
  p$presetFlightTask<-presetFlightTask
  p$maxSpeed<-maxSpeed
  p$followSurfaceRes<-followSurfaceRes
  p$maxFL=maxFL
  p$windCondition<-windCondition
  p$rcRange<-rcRange
  p$uavType<-uavType
  p$curvesize<-0
  p$rotationdir<-0
  p$gimbalmode<-0
  p$gimbalpitchangle<--90
  p$launchAltitude<-launchAltitude
  p$aboveTreeAlt<-aboveTreeAlt
  p$altFilter<-altFilter
  p$projectDir<-projectDir
  p$climbDist<-climbDist
  p$task<- robubu:::getPresetTask("treetop")
  
  fullTreeList<-robubu:::makeFlightPath(treeList,p,uavType,task,demFn,logger)
}
