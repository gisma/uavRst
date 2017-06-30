if (!isGeneric('makeFP')) {
  setGeneric('makeFP', function(x, ...)
    standardGeneric('makeFP'))
}
#' make flight plans (makeFP) is a tool to generate autonomous flight plans for
#' an optimal picture retrieval with respect to DSM/DEM and orthophoto
#' calculation.
#'
#' @description The basic idea is to provide an easy to use workflow for
#'   controlling rtf UAVs from planning and flying autonomous surveys to
#'   derivation and post classification of the data. makeFP (Make UAV Remote
#'   Controlled Survey) creates either intermediate flight control files for the
#'   DJI phantom x UAVs or ready to upload control files for the 3DR Solo. The
#'   dji control files are designed for using with the proprietary litchi flight
#'   control app exchange format, while the 3DR Solo files are using the MAVLINK
#'   common message set, that is used by the PixHawk flight controller family.
#'   Both are implemented very rudimentary.\cr\cr DJI:\cr The reason using DJI
#'   is their absolute straightforward usage. Everybody can fly with a DJI but
#'   the price is a hermetically closed system. Only the  litchi app provides
#'   additionally to a cloud based mission planer an offline/standalone
#'   interface to upload a CSV formated way point file for autonomous flights to
#'   the Phantom.\cr\cr PixHawk/3DR Solo:\cr The open uav community is focused
#'   on the PixHawk autopilot unit and the Mission Planner software. It is well
#'   documented and several APIs are provided. Nevertheless a terrain following
#'   autonomous flight planning tool is not available. makeFP creates static
#'   implementation of the MAV format that is ready to be uploaded directly on
#'   the Pixhawk controller using the upload2Solo function.\cr\cr
#' @section Warning: Take care! There are still a lot of construction zones
#'   around. This script is far beyond to be in a mature state. Please control
#'   and backup all controls again while planning and performing autonomous
#'   flight plans and missions. You will have a lot of chances to make a small
#'   mistake what may yield in a damage of your uav or even worse in involving
#'   people, animals or non-cash assets. Check your risk use parachute systems
#'   and even if it is running like a charm keep alert!

#'
#'
#'
#' @section Basic Introduction:
#' \subsection{Survey Area}{
#'
#'   To define a flight area you have to provide either 4 Points (or 3 lines).
#'   You may take more complex vectors like a multi point polygon,
#'   but only the first 4 coordinates x1, x2, x3 and x4 (for the launching position)
#'   are used in exactly this order.
#'   If you take a rectangle the 4th corner coordinate will be the launching point!
#'   \cr\cr
#'   The concept is looking like the following sketch.
#'  \preformatted{
#'   x2------x3           x2-------x1
#'   | a                 /
#'   |                  /
#'   |   x4            / x4
#'   |  /             / /
#'   x1/             x3/
#'   }
#'   This coordinates the length of the line and the angle are used to calculate extend and parallels
#'   of the flight plan according to the flight altitude, overlap etc. Note the flight direction depends on
#'   the order of the points. If the \code{flightPlanMode} is equal \code{tracks}.
#'   \cr\cr
#'   The result look like this.
#'
#'  \preformatted{
#'
#'   #--#  #-->             #-----#
#'   |  |  |               /    
#'   |  |  |              #-----#
#'   |  |  |                   /
#'   #  #--#         <--#-----#

#'   }
#'  If \code{flightPlanMode} is equal \code{waypoints} the result is an equal spatial distribution of way points:
#'  \preformatted{
#'
#'   #--#  #-->             #--#--#
#'   |  |  |               /    
#'   #  #  #              #--#--#
#'   |  |  |                   /
#'   #  #--#         <--#--#--#
#'   }
#'
#'
#'   \code{waypoints} is optimal for autonomous flights under calm conditions in complex terrain
#'   because the camera takes a picture at every way point\cr
#'   \code{track} is optimal for relatively plain areas and automatically triggered picture capturing
#'   Note: Automatically picture capturing in a time interval works only within the range of the remote control.
#'   because the the uav needs a trigger signal for taking pictures.
#'   }
#'   \subsection{Terrain Following flight plan}{

#'
#'   The \code{followSurface} switch is used to adapt the fixed flight altitude into a terrain following flight altitude.\cr
#'   ----------------------------------------------------------------------------------------------------------\cr
#'   NOTE: You have to be aware that the DJI uav is calibrating the altitude at the launch position in the field!
#'   So you need either a correct coordinate altitude or a high resolution DEM to get a good! estimation of the launch position and altitude.
#'   You must choose a clearly defined and reliable launching position both in the map and the field. If you fail I made the experience that the aircraft
#'   probably will hit the terrain...\cr
#'   ----------------------------------------------------------------------------------------------------------\cr\cr
#'
#'  How it works. Let us assume a defined flight altitude of 50 m.
#'  According to the launching point altitude the uav will act like the following sketch shows:
#'
#' \preformatted{
#'
#'   ............... x_(uav)_x ........... uav started at 30 m altitude results in
#'                                            a "real" flight altitude of 30m + 50m => 80m
#'
#'
#'                   ___60m____
#'                  |          |
#'          30m _x__|          |
#'         ____|               |___
#'     ___|                        |____
#
#'
#'
#'                  ___60m____
#'       ..........|          |............ uav started at 0 m altitude results in
#'              ___|          |___          "real" flight altitude of 50m above 0m
#'         ____|                  |
#'     ___|                       |__x__ 0m
#
#'   }
#'  To avoid negative impacts from the P3 auto calibration, the launch altitude is used to
#'  correct the flight altitude according to: \cr
#'  maximumAltitude_of_surveyArea + altitude_of_launchposition\cr
#'  So the adapted flight altitude is always seen as the flight altitude above the highest terrain altitude:
#'   \preformatted{
#'
#'  ...................................... real altitude of uav 110 m
#'
#'
#'                  ___60m____
#'                 |          |
#'          30m _x_|          |___
#'         ____|                  |
#'     ___|                       |______
#
#'   }
#'  To get a fixed scale flight the launch altitude is used to correct the flight altitude according to   maximumAltitude of surveyArea + altitude of launch position. With the setting of terrainfollowing = true tis is calculated for each way point.  . So the adapted flight altitude looks like:
#'   \preformatted{
#'
#'                  ..........
#'                 |          |
#'             ....|          |....
#'        ....|     ___60m____    |
#'   ....|         |          |   |....... real altitude of uav 50m
#'          30m _x_|          |___
#'         ____|                  |
#'     ___|                       |___x___ 0m
#
#'   }
#'   }

#' @param projectDir path to the main folder where several locations can be hosted
#' @param locationName path to the location folder where all tasks of this plot are hosted
#' @param surveyArea  you may provide either the coordinates by
#' c(lon1,lat1,lon2,lat2,lon3,lat3,launchLat,launchLon) or
#' an OGR compatible file (preferably geoJSON or KML) with
#' at least 4 coordinates that describe the flight area.
#' The fourth coordinate is the launch position.
#'  You will find further explanation under the \link{seealso}.
#' @param launchAltitude absolute altitude of launching position.
#' It will overwrite the DEM based estimation if any other value than -9999
#' @param demFn  filename of the corresponding DEM data file
#' @param taskName base string for taskName filenames
#' @param followSurface  \code{boolean}  TRUE performs an altitude correction
#' of the missions flight altitude using additional DEM data.
#' If no DEM data is provided and \code{followSurface} is TRUE,
#' SRTM data will be downloaded and used
#' Further explanation at \link{seealso}
#' @param altFilter if \code{followingTerrain} is equal \code{TRUE} then
#' \code{altFilter} is the threshold value of accepted altitude difference (m) between two way points.
#'  If this value is not exceeded the way point is omitted due to the fact that only 99 way points per mission are allowed.
#'  If this value is not exceeded the way point is omitted due to the fact that only 99 way points per mission are allowed.
#' @param horizonFilter integer filter size of the rolling filter kernel for the flight track. Must be multiplied by the \code{followSurfaceRes} to get the spatial extent
#' @param flightPlanMode type of flight plan. Available are: \code{"waypoints"},
#'   \code{"track"}, \code{"manual"}.
#' @param presetFlightTask (DJI only) strongly recommended to use "remote"
#'        \cr
#'  Options are:
#' \code{"simple_ortho"} takes one picture/way point,
#' \code{"multi_ortho"} takes 4 picture at a waypoint, two vertically down and two in forward and backward viewing direction and an Angele of -60deg,
#' \code{"simple_pano"} takes a 360 deg panorama picture and
#' \code{"remote"} which assumes that the camera is controlled by the remote control (RC)
#' @param flightAltitude set the default flight altitude of the mission. It is
#'   assumed that the UAV is started at the highest point of the surveyArea
#'   otherwise you have to defined the position of launching.
#' @param overlap overlapping of the pictures in percent (1.0 = 100)
#' @param djiBasic c(0,0,0,-90)
#' \cr curvesize (DJI only) controls the curve angle of the uav passing way points.
#' By default it is set to (\code{= 0.0}).
#' \cr rotationdir (DJI only) camera control parameter set the UAV basic turn direction to right (0) or left (1)
#' \cr gimbalmode (DJI only) camera control parameter
#' \code{0} deactivates the gimbal control
#' \code{1} activates the gimbal for focusing POIs
#' \code{2} activates the gimbal for focus and interpolate a field of view in an angel of \code{gimbalpitchangle}
#' \cr gimbalpitchangle (DJI only) vertical angle of camera  \code{+30 deg..-90 deg}
#' \cr actiontype (DJI only) individual actionype settings of the camera c(1,1,...)
#' \cr actionparam (DJI only) corresponding parameter for the above individual actiontype c(0,0,...)
#' \code{uavViewDir} viewing direction of camera default is \code{0}
#' @param maxSpeed  cruising speed
#' @param heatMap switch for calculating the overlapping factor on a raster map
#' @param picFootprint switch for calculating the footprint at all way points
#' @param followSurfaceRes horizontal step distance for analyzing the DEM altitudes
#' @param picRate fastest stable interval (s) for shooting pictures
#' @param windCondition 1= calm 2= light air 1-5km/h, 3= light breeze 6-11km/h, 4=gentle breeze 12-19km/h 5= moderate breeze 20-28km/h

#' @param maxFlightTime user defined estimation of the lipo lifetime (20 min default)
#' @param rcRange range of estimated range of remote control
#' @param uavType type of uav. currently "djip3" and "solo" are supported
#' @param dA if TRUE the real extent of the used DEM is returned helpful for low altitudes flight planning
#' @param cameraType depending on uav system for dji the dji4k is default for solo you can choose GP3_7MP GP3_11MP and MAPIR2
#'
#' @note
#' To use the script you need to install quite a lot of R-packages and at least the binary GDAL tools as well as SAGA GIS and GRASS GIS according to your system needs. Please find more information at NASA EarthData project: \href{http://giswerk.org/doku.php?id=rs:micrors:uavrs:intro}{uav based Remote Sensing at giswerk.org}).
#'
#'
#'
#' @examples
#'
#'\dontrun{
#' # Please keep in mind that there is a bunch of interdependent parameter settings.
#'
#' # The following spatial data sets are returned
#'
#' # lp      the planned launching position of the uav.
#' # wp      waypoints inclusive all information
#' # oDEM    the original (input) digital surface model (DSM)
#' # rDEM    the resampled (used) DSM
#' # fp      optimized footprints of the camera
#' # fA      flight area with at least 2 overlaps
#' # rcA     area covered by the RC according to the range and line of sight
#' # hm      a heatmap abundance of pictures/pixel (VERY SLOW, only if heatMap = TRUE)
#'
#'
#' # load example DEM data
#' data(mrbiko) # to use the example data it's easier to write same in tif format
#' writeRaster(mrbiko,"~/dem.tif")
#'
#' #
#'
#'
#' ## (2) simple flight, 50 meters above ground
#' ##     assuming a flat topography,
#' ##     generating a heatmap to estimate overlapping
#'
#' fp<-makeFP(surveyArea = c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.8055,8.734),
#'               demFn = "~/dem.tif",
#'               heatMap = TRUE)
#'
#' ## view results
#' mapview(fp$lp,color="red",cex=5)+
#' mapview(fp$wp,zcol = "altitude",lwd=1,cex=4)+
#' mapview(fp$oDEM,col=terrain.colors(256))+
#' mapview(fp$fA,color="red", alpha.regions = 0.1,lwd=1.0)+
#' mapview(fp$hm)+
#' fp$demA
#'
#'
#' ## (2) typical real case scenario
#' ##     a flight altitude BELOW 50 m is extreme
#' ##     U have to use a high resulution DSM
#' ##     (here simulated with a standard DEM)
#'
#' fp<-makeFP(projectDir ="~/uav/test",
#'            locationName = "DEPA01",
#'            surveyArea=c(50.80801,8.72993,50.80590,8.731153,50.80553,8.73472,50.80709,8.734),
#'            followSurface = TRUE,
#'            flightAltitude = 30,
#'            demFn = "~/dem.tif",
#'            windCondition = 3,
#'            uavType = "djip3",
#'            followSurfaceRes = 5,
#'            altFilter = .75)
#'
#'
#' ## (3) use of external vector data to define the surveyArea...
#' ##     digitize flight area using leafDraw()
#' ##     save vectors as JS "json" or "kml" files
#' ##     provide full filename+upper extensions!
#'
#' leafDraw(preset="uav")
#'
#' ## assuming resulting file is named "uav.json"
#' ## use it for planning
#'
#' fp<-makeFP(projectDir="~/uav/test",
#'                    locationName = "DEPA01",
#'                    surveyArea="~/uav.json",
#'                    followSurface = TRUE,
#'                    followSurfaceRes = 1,
#'                    flightAltitude = 50,
#'                    overlap = 0.8,
#'                    demFn = "~/mrbiko.tif",
#'                    altFilter = 3.5,
#'                    maxSpeed = 20,
#'                    uavType = "djip3",
#'                    dA = TRUE,
#'                    windCondition = 3)
#'
#' ## view results
#'mapview::mapview(fp2$wp,zcol = "altitude",cex=4, lwd=0.5)+
#'mapview(fp2$lp,color = "red", lwd=1,cex=4)+
#'mapview::mapview(fp2$fA,color="blue", alpha.regions = 0.1,lwd=0.5)+
#'mapview(fp2$oDEM,col=terrain.colors(256))+
#'mapview::mapview(fp2$demA,color="black", alpha.regions = 0.1,lwd=0.5)

#'}


#' @export makeFP
#'


makeFP <- function(projectDir = "~",
                   locationName = "dummylocation",
                   surveyArea = NULL,
                   flightAltitude = 100,
                   launchAltitude = NULL,
                   followSurface = FALSE,
                   followSurfaceRes = NULL,
                   demFn = NULL,
                   altFilter = 1.0,
                   horizonFilter = 30,
                   terrainSmooth= FALSE,
                   flightPlanMode = "track",
                   presetFlightTask = "remote",
                   overlap = 0.8,
                   maxSpeed = 20.0,
                   maxFlightTime = 10,
                   picRate = 2,
                   windCondition = 0,
                   uavType = "solo",
                   cameraType = "MAPIR2",
                   cmd=16,
                   uavViewDir = 0,
                   djiBasic = c(0, 0, 0,-90, 0),
                   dA = FALSE,
                   heatMap = FALSE,
                   picFootprint = FALSE,
                   rcRange = NULL,
                   copy = FALSE)
{
  ###  setup environ and params
  cat("setup environ and params...\n")
  # assign flight mission name
  #workingDir <- format(Sys.time(), "%Y_%m_%d_%H-%M") 
  workingDir <- format(Sys.time(), "%Y_%m_%d") 
  taskName <-paste(paste0(locationName, "_",
                          flightAltitude,"m_",
                          uavType,"_", 
                          cameraType,"_", 
                          tools::file_path_sans_ext(basename(surveyArea)),"_", 
                          format(Sys.time(), "%Y_%m_%d_%H-%M")),
                   sep = .Platform$file.sep)
  
  
  
  # create directories if needed
  if (!file.exists(file.path(projectDir, locationName, workingDir))) {
    dir.create(file.path(projectDir, locationName, workingDir), recursive = TRUE)
  }
  if (!file.exists(file.path(projectDir, locationName, workingDir, "run"))) {
    dir.create(file.path(projectDir, locationName,workingDir, "/run"), recursive = TRUE)
  }
  if (!file.exists(file.path(projectDir, locationName,workingDir, "control"))) {
    dir.create(file.path(projectDir, locationName,workingDir, "control"), recursive = TRUE)
  }
  if (!file.exists(file.path(projectDir, locationName,workingDir, "log"))) {
    dir.create(file.path(projectDir, locationName,workingDir, "log"), recursive = TRUE)
  }
  if (!file.exists(file.path(projectDir,locationName, "data"))) {
    dir.create(file.path(projectDir,locationName, "data"), recursive = TRUE)
  }
  if (!is.null(surveyArea)) {
    file.copy(surveyArea, paste0(file.path(projectDir,locationName, "data")))
    surveyArea <- paste0(file.path(projectDir,locationName, "data"), "/", basename(surveyArea))
      
  }
  
  if (!is.null(demFn) & copy ) {
    file.copy(demFn, paste0(file.path(projectDir,locationName, "/data"), "/", basename(demFn)))
    demFn <- paste0(file.path(projectDir,locationName, "/data"), "/", basename(demFn))
    
  }
  # setting R environ temp folder to the current working directory
  Sys.setenv(TMPDIR = file.path(projectDir, locationName, workingDir, "run"))
  
  # set R working directory
  setwd(file.path(projectDir,locationName, workingDir, "run"))
  
  # set common read write permissions
  #Sys.chmod(list.dirs("../.."), "777")
  
  # create log file
  logger <- log4r::create.logger(logfile = paste0(file.path(projectDir, locationName, workingDir, "log/"),strsplit(basename(taskName), "\\.")[[1]][1],'.log'))
  log4r::level(logger) <- "INFO"
  log4r::levellog(logger, 'INFO', "                                                           ")
  log4r::levellog(logger,'INFO',"--------------------- START RUN ---------------------------")
  log4r::levellog(logger, 'INFO', paste("Working folder: ", file.path(projectDir, locationName, workingDir)))
  
  # generate misson control filename
  csvFn <-paste(file.path(projectDir, locationName, workingDir, "control"),paste0(taskName, ".csv"),sep = .Platform$file.sep)
  
  # get survey area
  surveyArea <- calcSurveyArea(surveyArea, projectDir, logger)
  
  # need picfootprint for calculating the heatmap
  if (heatMap) {
    picFootprint = TRUE
  }
  
  # uav depending parameter setting
  if (uavType == "djip3") {
    cameraType<-"dji4k"
    factor <- 1.71
    flightParams = c(flightPlanMode = flightPlanMode,
                     launchAltitude = launchAltitude,
                     flightAltitude = flightAltitude,
                     presetFlightTask = presetFlightTask,
                     overlap = overlap,
                     curvesize = djiBasic[1],        # curvesize
                     rotationdir = djiBasic[2],      # rotationdir
                     gimbalmode = djiBasic[3],       # gimbalmode
                     gimbalpitchangle = djiBasic[4], # gimbalpitchangle
                     uavViewDir = uavViewDir,
                     followSurfaceRes = followSurfaceRes)
    
    #calc & assign overlapping factor as a function of flightAltitude
    fliAltRatio <- 1 - overlap
    
    # FOV*agl*(1-overlap)
    uavOptimumSpeed <- ceiling(factor * flightAltitude * fliAltRatio)
    
  }
  else if (uavType == "solo") {
    if (cameraType == "MAPIR2") {
      factor <- 1.55
    } else if (cameraType == "GP3_7MP") {
      factor <- 2.3
    } else if (cameraType == "GP3_11MP") {
      factor <- 2.3
    }
    
    
    flightParams = c(flightPlanMode = flightPlanMode,
                     launchAltitude = launchAltitude,
                     flightAltitude = flightAltitude,
                     presetFlightTask = presetFlightTask,
                     overlap = overlap,
                     uavViewDir = uavViewDir,
                     followSurfaceRes = followSurfaceRes)
    
    #calc & assign overlapping factor as a function of flightAltitude
    fliAltRatio <- 1 - overlap
    
    # FOV*agl*(1-overlap)
    uavOptimumSpeed <- ceiling(factor * flightAltitude * fliAltRatio)
  }
  
  # adapt default flight params to runtime request
  p <- makeFlightParam(surveyArea, flightParams, followSurface)
  
  # assign flightmode
  mode <- as.character(p$flightPlanMode)
  
  # assign flight Altitude
  flightAltitude <- as.numeric(flightParams["flightAltitude"])
  
  
  
  # calc distance beteen two pictures using a camera dependent multiplicator
  trackDistance <- calcTrackDistance(fliAltRatio, flightAltitude, factor)
  totalTrackdistance <- trackDistance
  # to keep it simple we tacke picture as squares
  crossDistance <- trackDistance
  
  # calculate survey area
  # create an sp polygon object of the mission area
  taskArea <- taskarea(p, csvFn)
  # reproject it to UTM
  taskAreaUTM <- spTransform(taskArea, CRS(paste("+proj=utm +zone=",long2UTMzone(p$lon1)," ellps=WGS84",sep = '')))
  # calculate area
  surveyAreaUTM <- rgeos::gArea(taskAreaUTM)
  
  # calculate heading from launch position to mission start position
  launch2startHeading <- geosphere::bearing(c(p$launchLon, p$launchLat),c(p$lon1, p$lat1),a = 6378137,f = 1 / 298.257223563)
  
  # calculate and assign  heading base flight track W-E
  updir <- geosphere::bearing(c(p$lon1, p$lat1),c(p$lon2, p$lat2),a = 6378137,f = 1 / 298.257223563)
  
  # calculate and assign  heading base flight track E-W
  downdir <- geosphere::bearing(c(p$lon2, p$lat2),c(p$lon1, p$lat1),a = 6378137,f = 1 / 298.257223563)
  
  # calculate and assign  heading base flight track trackline to trackline
  crossdir <- geosphere::bearing(c(p$lon2, p$lat2),c(p$lon3, p$lat3),a = 6378137,f = 1 / 298.257223563)
  
  # calculate and assign  distance of the base flight track
  len <- geosphere::distGeo(c(p$lon1, p$lat1), c(p$lon2, p$lat2))
  
  # calculate and assign distance of the cross base flight track
  crosslen <- distGeo(c(p$lon2, p$lat2),c(p$lon3, p$lat3),a = 6378137,f = 1 / 298.257223563)
  
  if (is.null(followSurfaceRes)) {
    followSurfaceRes <- trackDistance
  }
  
  # IF followSurface set track/crossDistance to followSurfaceRes
  if (followSurface) {
    multiply <- floor(len / followSurfaceRes)
    trackDistance <- followSurfaceRes
    #crossDistance<-followSurfaceRes
  } else{
    multiply <- floor(len / trackDistance)
  }
  
  # calculate and assign  number of tracklines
  tracks <- floor(crosslen / crossDistance)
  
  #set initial heading
  heading <- updir
  
  # set universal view direction of the uav
  if (abs(as.numeric(flightParams["uavViewDir"])) == 0) {
    uavViewDir <- updir
  }
  else {
    uavViewDir <- abs(as.numeric(flightParams["uavViewDir"]))
  }
  
  # init of control id #1 common  #99 turnpoints of single tracks
  group <- 1
  
  # set cumulative flightlength to zero
  flightLength <- 0
  
  # initialize djiDF and
  djiDF <- data.frame()
  mavDF <- data.frame()
  
  # define output line var
  lns <- list()
  
  lns <- launch2flightalt(p, lns, uavViewDir, launch2startHeading, uavType)
  
  # assign starting point
  pos <- c(p$lon1, p$lat1)
  # calculates the footprint of the first position and returns a SpatialPolygonsDataFrame
  if (picFootprint)  camera <- calcCamFoot(pos[1], pos[2], uavViewDir, trackDistance, flightAltitude, 0, 0)
  else  camera = "NULL"
  
  # creates the export control parameter set of the first position
  if (uavType == "djip3") {
    lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
  }
  if (uavType == "solo") {
    lns[length(lns) + 1] <-  makeUavPointMAV(lat = pos[2],lon = pos[1], head = uavViewDir, group = 99 )
  }
  # push pos to old pos
  pOld <- pos
  
  # set counter and params for mode = "track" mode
  if (mode == "track") {
    if (uavType == "djip3") {
      lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
    }
    if (uavType == "solo") {
      lns[length(lns) + 1] <- makeUavPointMAV(lat = pos[2],lon = pos[1],head = uavViewDir,group = 99)
    }
    trackDistance <- len
    multiply <- 1
  }
  # set counter and params for mode = "waypoints"
  else if (mode == "waypoints") {
    if (uavType == "djip3") {
      lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
    }
    if (uavType == "solo") {
      lns[length(lns) + 1] <- makeUavPointMAV(lat = pos[2],lon = pos[1],head = uavViewDir,group = 99)
    }
  }
  # set counter and params for mode = "terrainTrack"
  else if (mode == "terrainTrack") group = 99
  #
  cat("calculating waypoints...\n")
  pb <- pb <- txtProgressBar(max = tracks, style = 3)
  # then do for the rest  forward and backward
  for (j in seq(1:tracks)) {
    for (i in seq(1:multiply)) {
      if (mode == "waypoints" || mode == "terrainTrack") {
        if (i >= multiply) {
          group <- 99
        }
        else      {
          group <- 1
        }
      }
      else {
        i <- 2
      }
      
      # calc next coordinate
      pos <- calcNextPos(pOld[1], pOld[2], heading, trackDistance)
      if (picFootprint) camera <- spRbind(camera, calcCamFoot( pos[1], pos[2], uavViewDir, trackDistance, flightAltitude,i,j))
      pOld <- pos
      flightLength <- flightLength + trackDistance
      if (mode == "track") {
        group <- 99
      }
      if (uavType == "djip3") {
        lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = group, p)
      }
      if (uavType == "solo") {
        lns[length(lns) + 1] <- makeUavPointMAV(lat = pos[2], lon = pos[1], head = uavViewDir, group = group)
      }
    }
    
    if ((j %% 2 != 0)) {
      pos <- calcNextPos(pOld[1], pOld[2], crossdir, crossDistance)
      if (picFootprint) camera <-  spRbind(camera, calcCamFoot( pos[1], pos[2], uavViewDir, trackDistance,flightAltitude,i,j))
      pOld <- pos
      flightLength <- flightLength + crossDistance
      if (uavType == "djip3") {
        lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
      }
      if (uavType == "solo") {
        lns[length(lns) + 1] <-
          makeUavPointMAV(
            lat = pos[2],
            lon = pos[1],
            head = uavViewDir,
            group = 99
          )
      }
      heading <- downdir
    }
    
    else if ((j %% 2 == 0)) {
      pos <- calcNextPos(pOld[1], pOld[2], crossdir, crossDistance)
      if (picFootprint) camera <- spRbind(camera, calcCamFoot( pos[1], pos[2], uavViewDir,trackDistance,flightAltitude,i,j))
      pOld <- pos
      flightLength <- flightLength + crossDistance
      
      if (uavType == "djip3") {
        lns[length(lns) + 1] <- makeUavPoint(pos, uavViewDir, group = 99, p)
        heading <- updir
      }
      if (uavType == "solo") {
        lns[length(lns) + 1] <-  makeUavPointMAV( lat = pos[2], lon = pos[1], head = uavViewDir - 180, group = 99)
        heading <- updir
      }
      
    }
    # status bar
    setTxtProgressBar(pb, j)
  }
  close(pb)
  
  #estimate time regarding parameter
  ft <- calculateFlightTime( maxFlightTime,
                             windCondition,
                             maxSpeed,
                             uavOptimumSpeed,
                             flightLength,
                             totalTrackdistance,
                             picRate,
                             logger)
  rawTime <- ft[1]
  maxFlightTime <- ft[2]
  maxSpeed <- ft[3]
  picIntervall <- ft[4]
  
  
  # postprocessing
  fileConn <- file("tmp.csv")
  cat("preprocessing DEM related stuff...\n")
  if (uavType == "djip3") {
    # dump lns to file for read in as csv
    writeLines(unlist(lns[1:length(lns) - 1]), fileConn)
    djiDF <- read.csv("tmp.csv", sep = ",", header = FALSE)
    # add correct header
    names(djiDF) <-unlist(strsplit(makeUavPoint(pos,uavViewDir,group = 99,p,header = TRUE,sep = ' '),split = " "))
    # make it spatial
    sp::coordinates(djiDF) <- ~ lon + lat
    sp::proj4string(djiDF) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
    # now DEM stuff
    result <- analyzeDSM(demFn,djiDF,p,altFilter,horizonFilter,followSurface,followSurfaceRes,terrainSmooth,logger,projectDir,dA,workingDir,locationName)
    # assign adapted dem to demFn
    demFn <- result[[3]]
    dfcor <- result[[2]]
    
    # max numbers of dji waypoints is due to factory limits 98
    # according to start and rth safety we need 6 points for organizig the splitted task
    nofiles <- ceiling(nrow(dfcor@data) / 90)
    maxPoints <- 90
    minPoints <- 1
    # check if the flighttime is forcing more files
    if (nofiles < ceiling(rawTime / maxFlightTime)) {
      nofiles <- ceiling(rawTime / maxFlightTime)
      maxPoints <- ceiling(nrow(dfcor@data) / nofiles) + 1
      mp <- maxPoints
      minPoints <- 1
    }
    # start the creation of the control file(s)
    cat('generate control files...\n')
    # generate single tasks waypoint file for DJI Litchi import format
    calcDjiTask( result[[2]],taskName,nofiles,maxPoints,p,logger, round(result[[6]], digits = 0), trackSwitch,"flightDEM.tif",result[[8]], projectDir,workingDir,locationName)
  }
  else if (uavType == "solo") {
    writeLines(unlist(lns), fileConn)
    mavDF <- read.csv("tmp.csv", colClasses=c("V4"="character",
                                              "V5"="character",
                                              "V6"="character",
                                              "V7"="character"),sep = "\t", header = FALSE)
    names(mavDF) <- c("a","b","c","d","e","f","g","latitude","longitude","altitude","id","j","lat","lon")
    sp::coordinates(mavDF) <- ~ lon + lat
    sp::proj4string(mavDF) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
    
    if (is.null(launchAltitude)) {
      # analyze DEM related stuff
      result <- analyzeDSM(demFn,mavDF,p,altFilter,horizonFilter ,followSurface,followSurfaceRes,terrainSmooth,logger,projectDir =projectDir,dA,workingDir = workingDir,locationName)
      # assign adapted dem to demFn
      lauchPos <- result[[1]]
      dfcor <- result[[2]]
      demFn <- result[[3]]
      
      nofiles <- ceiling(rawTime / maxFlightTime)
      maxPoints <- ceiling(nrow(dfcor@data) / nofiles) + 1
      
    }
    # generate single tasks waypoint file for MAV Solo format
    calcMAVTask(result[[2]],taskName,nofiles,rawTime,mode,trackDistance,maxFlightTime,logger,p,len, multiply,tracks,result,maxSpeed / 3.6,uavType,"flightDEM.tif",maxAlt = result[[6]], projectDir,workingDir,locationName,uavViewDir,cmd)
  }
  close(fileConn)
  
  # if heatMap is requested
  if (heatMap) {
    cat("calculating picture coverage heat map\n")
    fovH <- calcFovHeatmap(camera, result[[4]])
  } else
  {
    fovH <- "NULL"
  }
  
  # call rcShed
  if (!is.null(rcRange)) {
    
    cat("calculating RC-range\n")
    rcCover <-
      rcShed(
        launchP = c(as.numeric(p$launchLon), as.numeric(p$launchLat)),
        flightAlt =  as.numeric(p$flightAltitude),
        rcRange = rcRange,
        dem = result[[4]]
      )
  } else {
    rcCover = "NULL"
  }
  
  
  
  
  # write log file status and params
  log4r::levellog(logger, 'INFO', paste("taskName     : ", taskName))
  log4r::levellog(logger, 'INFO', paste("DEM filename    : ", names(demFn)))
  log4r::levellog(logger, 'INFO', paste("surveyArea      : ", surveyAreaUTM))
  log4r::levellog(logger, 'INFO', paste("launchAltitude  : ", launchAltitude))
  log4r::levellog(logger, 'INFO', paste("followSurface   : ", followSurface))
  log4r::levellog(logger, 'INFO', paste("altfilter       : ", altFilter))
  log4r::levellog(logger, 'INFO', paste("horizonFilter   : ", horizonFilter))
  log4r::levellog(logger, 'INFO', paste("flightPlanMode  : ", flightPlanMode))
  log4r::levellog(logger, 'INFO', paste("flightAltitude  : ", flightAltitude))
  log4r::levellog(logger,
           'INFO',
           paste("presetFlightTask: ", presetFlightTask))
  log4r::levellog(logger, 'INFO', paste("curvesize       : ", p$curvesize))
  log4r::levellog(logger, 'INFO', paste("rotationdir     : ", p$rotationdir))
  log4r::levellog(logger, 'INFO', paste("gimbalmode      : ", p$gimbalmode))
  log4r::levellog(logger,'INFO',paste("gimbalpitchangle: ", p$gimbalpitchangle))
  log4r::levellog(logger, 'INFO', paste("overlap         : ", overlap))
  log4r::levellog(logger, 'INFO', paste("uavViewDir      : ", uavViewDir))
  log4r::levellog(logger, 'INFO', paste("picFootprint    : ", picFootprint))
  log4r::levellog(logger,'INFO',paste("followSurfaceRes: ", followSurfaceRes))
  log4r::levellog(logger, 'INFO', paste("surveyAreaCoords: ", surveyArea))
  log4r::levellog(logger, 'INFO', paste("windCondition   : ", windCondition))
  log4r::levellog(logger, 'INFO', "-")
  log4r::levellog(logger,'INFO',"----- use the following task params! --------------")
  log4r::levellog(logger,'INFO',paste("set RTH flight altitude to    : ", round(result[[6]], digits = 0), " (m)"))
  log4r::levellog(logger,'INFO',paste("set flight speed to a max of: ",round(maxSpeed, digits = 1),"  (km/h)      "))
  log4r::levellog(logger,'INFO',paste("set pic rate to at least : ", picIntervall, "  (sec/pic) "))
  log4r::levellog(logger,'INFO',paste("calculated mission time    : ", rawTime,      "  (min)      "))
  log4r::levellog(logger,'INFO',paste("estimated battery lifetime  : ", maxFlightTime,      "  (min)      "))
  log4r::levellog(logger,'INFO',paste("Area covered               : ", surveyAreaUTM / 10000,      "  (ha)"))
  # return params for visualisation and main results for overview
  if ((flightPlanMode == 'track' | flightPlanMode == 'terrainTrack') & rawTime > maxFlightTime)  {
    note <- "flighttime > battery lifetime! control files have been splitted. Have Fun..."
  }
  else if (flightPlanMode == 'waypoints') {
    note <- "control files are splitted after max 98 waypoints (litchi control file restricted number)"
  }
  else { note <- " Have Fun " }
  dumpFile(paste0(file.path(projectDir, locationName, workingDir, "log/"),strsplit(basename(taskName), "\\.")[[1]][1],'.log'))
  cat("\n ",
      "\n NOTE 1:",as.character(note),"",
      "\n NOTE 2: You will find all parameters in the logfile:",paste0(file.path(projectDir, locationName, workingDir, "log/"),strsplit(basename(taskName), "\\.")[[1]][1],'.log'),"","\n ")
  x <- c(result[[1]], # launch Pos
         result[[2]], # waypoints
         result[[5]], # resampled dem contour
         result[[3]], # original DEM
         result[[4]], # resampled dem
         camera,      # camera footprint (DJI only)
         taskArea,    # Area of flight task
         rcCover,     # Estimated area that is covered by RC
         fovH)        # Heatmap of overlapping Pictures
  names(x) <- c("lp", "wp", "demA", "oDEM", "rDEM", "fp", "fA", "rcA", "hm")
  system(paste0("rm -rf ",file.path(projectDir,locationName,workingDir,"run")))
  return(x)
}
