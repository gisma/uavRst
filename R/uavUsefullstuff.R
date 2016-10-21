# demCorrection performs all DEM related preprocessing and basic analysis stuff
# more substantial 
# (1) it imports and deproject different kind of input DEM/DSM data
# (2)  extracting the launching point altitude
# (3)  extracting all altitudes at the waypointsand the "real" agl flight altitude
# (4)  calculating the overall RTH 
# (5)  filtering in line waypoints according to an altitude difference treshold
# (6)  preprocessing of an highest resolution DSM dealing with clearings and other artefacts
# (7)  generates a sp object of the outer boundary of reliable DEM values

demCorrection<- function(demFn ,df,p,altFilter,followSurface,followSurfaceRes,logger,projectDir,dA,workingDir){
  
  cat("\n load DEM/DSM data...\n")
  ## load DEM data either from a local GDAL File or from a raster object or if nothing is provided tray to download SRTM data
  #if no DEM is provided try to get SRTM data
  if (is.null(demFn)){
    levellog(logger, 'WARN', "CAUTION!!! no DEM file provided I try to download SRTM data... SRTM DATA has a poor resolution for UAVs!!! ")
    cat("\nCAUTION! No DEM data is provided.\n trying to download SRTM data... \n Be aware that the resulution of SRTM is NOT sufficient for terrain following flights!")
    # download corresponding srtm data
    dem<-robubu::getGeoData(name="SRTM",xtent = extent(p$lon1,p$lon3,p$lat1,p$lat3), zone = 1.0,merge = TRUE)
    dem<-setMinMax(dem)
    rundem<- raster::crop(dem,extent(min(p$lon1,p$lon3)-0.0083,max(p$lon1,p$lon3)+0.0083,min(p$lat1,p$lat3)-0.0083,max(p$lat1,p$lat3)+0.0083))
    raster::writeRaster(dem,"tmpdem.tif",overwrite=TRUE)
    # read local dem file
  } else {
    # if already of type raster
    if (class(demFn)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")){
      rundem<-demFn
      rundem<- raster::crop(rundem,extent(min(p$lon1,p$lon3)-0.0083,max(p$lon1,p$lon3)+0.0083,min(p$lat1,p$lat3)-0.0083,max(p$lat1,p$lat3)+0.0083))
      raster::writeRaster(rundem,"tmpdem.tif",overwrite=TRUE)
      dem<-rundem
      # if GEOTIFF or other gdal type of data
    } else{
      rundem<-raster::raster(demFn,xmn=min(p$lon1,p$lon3)-0.0083,xmx=max(p$lon1,p$lon3)+0.0083,ymn=min(p$lat1,p$lat3)-0.0083,ymx=max(p$lat1,p$lat3)+0.0083)
      file.copy(demFn, paste0(file.path(projectDir,workingDir,"run"),"/tmpdem.tif"))    
      dem<-rundem
    }
  }  # end of loading DEM data
  
  
  # check if at least a projection string exist 
  #res<-compareProjCode(as.vector(as.character(rundem@crs)))
  #demll<-rasterCheckAdjustProjection(rundem)
  crsString<-compareProjCode(as.vector(as.character(rundem@crs)))
  if (!crsString) {
    stop("the DEM/DSM is not georeferencend - please provide a correct georeferenced raster object or GeoTiff file\n")
    # if so deproject DEM/DSM because all of the vector data is latlong WGS84
  } else {
    demll<-gdalwarp(srcfile = "tmpdem.tif", dstfile = "demll.tif", overwrite=TRUE,  t_srs="+proj=longlat +datum=WGS84 +no_defs",output_Raster = TRUE )  
    demll<-setMinMax(demll)
    
  }
  
  # if a low altitude flight is planned we have to perform some manipulations
  if (as.numeric(p$flightAltitude)<as.numeric(50)){
    demll<-DEM2FlSurface(p,dem,logger)
  }  # end of <50 meter
  
  # fill gaps a+proj=utm +zone=32 +ellps=GRS80 +units=m +no_defsnd extrapolate 
  #system(paste0("gdal_fillnodata.py   -md 500 -of GTiff ",demFn," filldem.tif"))
  # find local minima/maxima
  #system2("saga_cmd shapes_grid 9 -GRID='dem.sgrd' -MINIMA=NULL -MAXIMA='max'")
  #max<-readOGR(".","max")
  # crop it for speeding up
  #dem<-raster::crop(tmpdem,extent(min(p$lon1,p$lon3,p$lon2)-0.009,max(p$lon1,p$lon2,p$lon3)+0.009,min(p$lat1,p$lat2,p$lat3)-0.007,max(p$lat1,p$lat2,p$lat3)+0.007))
  
  
  # extract all waypoint altitudes
  altitude<-raster::extract(demll,df)
  # get maximum altitude of the task area
  maxAlt<-max(altitude,na.rm = TRUE)
  
  levellog(logger, 'INFO', paste("maximum DEM Altitude : ", maxAlt," m"))
  
  # create sp point object from launchpos 
  pos<-as.data.frame(cbind(p$launchLat,p$launchLon))
  sp::coordinates(pos) <- ~V2+V1
  sp::proj4string(pos) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  # extract launch altitude from DEM
  if (is.na(p$launchAltitude)){
    tmpalt<-raster::extract(demll,pos)  
    p$launchAltitude<-as.numeric(tmpalt)
    # otherwise take it from the parameter set
  } else 
  {
    p$launchAltitude<-as.numeric(p$launchAltitude)
  }
  
  levellog(logger, 'INFO', paste("launching Altitude : ", p$launchAltitude," m"))
  
  # write it back to the p list
  launchAlt<-p$launchAltitude
  
  # calculate the agl flight altitude shift due to launching and max altitude
  p$flightAltitude=as.numeric(p$flightAltitude)+(maxAlt-as.numeric(launchAlt))
  # make a rough estimation of the overall rth altitude
  rthFlightAlt<-p$flightAltitude
  p$rthAltitude=rthFlightAlt
  
  levellog(logger, 'INFO', paste("rthFlightAlt : ", rthFlightAlt," m"))
  
  # if terrainfollowing filter the waypoints by using the altFilter Value
  if (followSurface) {
    # calculate the agl flight altitude
    altitude<-altitude+as.numeric(p$flightAltitude)-maxAlt
    
    #write it to the sp object dataframe
    df$altitude<-altitude
    
    # if terraintrack = true try to reduce the number of waypoints by filtering
    if ( as.character(p$flightPlanMode) == "terrainTrack") {
      sDF<-as.data.frame(df@data)
      dif<-abs(as.data.frame(diff(as.matrix(sDF$altitude))))
      sDF<- sDF[-c(1), ]
      sDF$dif<-dif[,1]
      sDF[is.na(sDF)] <- 0
      fDF<-sDF[sDF$id=="99" | sDF$dif > altFilter , ]
      sDF<- sDF[-c(ncol(sDF),ncol(sDF)-1) ]
      fDF$lon<-fDF$longitude
      fDF$lat<-fDF$latitude
      fDF[complete.cases(fDF),]
      sp::coordinates(fDF) <- ~lon+lat
      sp::proj4string(fDF) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
      df<-fDF
    }
  }
  # create a sp polygon object of the DEM area that is useable for a flight task planning
  if (dA){
    demArea <- rasterToPolygons(clump(demll>0),dissolve = TRUE)}
  
  else{demArea="NULL"}
  writeRaster(demll,"flightDEM.tif",overwrite=TRUE)
  # return results
  return(c(pos,df,rundem,demll,demArea,rthFlightAlt,launchAlt,maxAlt,p))
}


# export data to DJI xchange format 
# (1) controls with respect to  waypoint number and/or batterylifetime  the splitting of the mission files to seperate task files
# (2) checking the return to home and fly to start of the misson tracks with respect to the obstacles to generate a save start and end of a task
generateDjiCSV <-function(df,mission,nofiles,maxPoints,p,logger,rth,trackSwitch=FALSE,dem,maxAlt,projectDir, workingDir){
  minPoints<-1
  addmax<-maxPoints
  if (maxPoints > nrow(df@data)) {maxPoints<-nrow(df@data)}
  # store launchposition and coordinates we need them for the rth calculations
  row1<-df@data[1,1:(ncol(df@data))]
  launchLat<-df@data[1,1]
  launchLon<-df@data[1,2]
  dem<-raster(dem)
  cat(paste0("create ",nofiles, " control files...\n"))
  for (i in 1:nofiles) {
    
    # take current start position of the partial task
    startLat<-df@data[minPoints+1,1] # minPoints+1 because auf adding the endpoint of the task
    startLon<-df@data[minPoints+1,2]
    # take current end position of split task
    endLat<-df@data[maxPoints,1]
    endLon<-df@data[maxPoints,2]
    # generate flight lines from lanch to start and launch to end point of splitted task
    yhome <- c(launchLat,endLat)
    xhome <- c(launchLon,endLon)
    ystart <- c(launchLat,startLat)
    xstart <- c(launchLon,startLon)
    start<-SpatialLines(list(Lines(Line(cbind(xstart,ystart)), ID="start")))
    home<-SpatialLines(list(Lines(Line(cbind(xhome,yhome)), ID="home")))
    sp::proj4string(home) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    sp::proj4string(start) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    
    # calculate minimum rth altitude for each line by identifing max altitude
    #homeRth<-max(unlist(raster::extract(dem,home)))+as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    #startRth<-max(unlist(raster::extract(dem,start)))+as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    homeRth<-raster::extract(dem,home,fun=max,na.rm=TRUE)+ as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    startRth<-raster::extract(dem,start,fun=max,na.rm=TRUE)+ as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    
    # generate an empty raster 
    mask<- dem
    values(mask)=NA
    #...update it with the altitude information of the flightline
    mask<-rasterize(home,mask)
    mask2<-mask*dem
    # and find the position of the max altitude
    idx = which.max(mask2)
    homemaxpos = xyFromCell(mask2,idx)
    # do it again for the second line
    mask<- dem
    values(mask)=NA
    mask<-rasterize(start,mask)
    mask2<-mask*dem
    idx = which.max(mask2)
    startmaxpos = xyFromCell(mask2,idx)
    # log the positions
    levellog(logger, 'INFO', paste("maxaltPos    rth : ", paste0("mission file: ",i," ",homemaxpos[2]," ",homemaxpos[1])))
    levellog(logger, 'INFO', paste("maxaltPos 2start : ", paste0("mission file: ",i," ",startmaxpos[2]," ",startmaxpos[1])))
    
    # calculate rth and 2start headings
    homeheading<-geosphere::bearing(c(endLon,endLat),c(launchLon,launchLat), a=6378137, f=1/298.257223563)
    startheading<-geosphere::bearing(c(launchLon,launchLat),c(startLon,startLat), a=6378137, f=1/298.257223563)
    
    # generate home max alt waypoint
    heading<-homeheading
    altitude<-homeRth+0.33*homeRth
    latitude<-homemaxpos[2]
    longitude<-homemaxpos[1]
    
    # generate ascent waypoint to realize save fly home altitude
    homemaxrow<-cbind(latitude,longitude,altitude,heading,row1[5:length(row1)])
    
    # maximum altitude wp on the way to the mission start
    heading<-startheading
    altitude<-startRth+0.33*startRth
    latitude<-startmaxpos[2]
    longitude<-startmaxpos[1]
    startmaxrow<-cbind(latitude,longitude,altitude,heading,row1[5:length(row1)])
    
    # calculate rth ascent from last task position
    pos<-calcNextPos(endLon,endLat,homeheading,7.5)
    
    # generate rth waypoints
    heading<-homeheading
    altitude<-homeRth
    latitude<-pos[2]
    longitude<-pos[1]
    # generate ascent waypoint to realize save fly home altitude
    ascentrow<-cbind(latitude,longitude,altitude,heading,row1[5:length(row1)])
    # generate home position with heading and altitude
    homerow<-cbind(row1[1:2],altitude,heading,row1[5:length(row1)])
    # genrate launch to start waypoint to realize save fly home altitude
    # calculate rth ascent from last task position
    pos<-calcNextPos(launchLon,launchLat,startheading,7.5)
    heading<-startheading
    altitude<-startRth
    startrow<-cbind(row1[1:2],altitude,heading,row1[5:length(row1)])
    latitude<-pos[2]
    longitude<-pos[1]
    startascentrow<-cbind(latitude,longitude,altitude,heading,row1[5:length(row1)])
    
    # extract the dataframe from the sp pint object
    DF<-df@data[(as.numeric(minPoints)+1):maxPoints,]
    # add the 6 safety points to esch dataframe (i.e. task)
    DF = rbind(startmaxrow,DF)
    DF = rbind(startascentrow,DF)
    DF = rbind(startrow,DF)
    DF = rbind(DF,ascentrow)
    DF = rbind(DF,homemaxrow)
    DF = rbind(DF,homerow)
    
    #if (maxPoints>nrow(DF)){maxPoints<-nrow(DF)}
    write.csv(DF[,1:(ncol(DF)-2)],file = paste0(projectDir,"/", workingDir,"/control/",mission,i,".csv"),quote = FALSE,row.names = FALSE)
    
    levellog(logger, 'INFO', paste("created : ", paste0(strsplit(getwd(),"/tmp")[[1]][1],"/control/",mission,"-",i,".csv")))
    minPoints<-maxPoints
    maxPoints<-maxPoints+addmax
    
    if (maxPoints>nrow(df@data)){
      maxPoints<-nrow(df@data)
      addmax<-maxPoints-minPoints}
  }
  
}



# (DJI only) create the full argument list for one waypoint
makeUavPoint<- function(pos,uavViewDir,group,p,header=FALSE,sep=","){
  # create the value lines
  if (!header){
    # create camera action arguments
    action<-""
    for (i in seq(1:length(p$task[,1]))){ 
      action<-paste0(action,p$task[i,]$x[1],sep)
    }
    # create waypoint plus camera options
    tmp <-    paste0(pos[1],sep,pos[2],sep,pos[2],sep,pos[1],
                     sep,as.character(p$flightAltitude),
                     sep,as.character(uavViewDir),
                     sep,as.character(p$curvesize),
                     sep,as.character(p$rotationdir),
                     sep,as.character(p$gimbalmode),
                     sep,as.character(p$gimbalpitchangle),
                     sep,action,
                     group)
  }
  # create the header
  else {
    action<-""
    for (i in seq(1:length(p$task[,1]))){ 
      action<-paste0(action,p$task[i,]$actionNames[1],sep)
    }
    tmp <-    paste0("lon",sep,"lat",sep,"latitude",sep,"longitude",sep,
                     "altitude",sep,
                     "heading",sep,
                     "curvesize",sep,
                     "rotationdir",sep,
                     "gimbalmode",sep,
                     "gimbalpitchangle",sep,
                     action,"id")    
  }
}



# export data to MAV xchange format 
# (1) controls with respect to  waypoint number and/or batterylifetime  the splitting of the mission files to seperate task files
# (2) calculate and insert rth and fts waypoints with respect to the terrain obstacles to generate a save start and end of a task

generateMavCSV <-function(df,mission,nofiles,rawTime,flightPlanMode,trackDistance,batteryTime,logger,p,len,multiply,tracks,param,speed,uavType,dem,maxAlt,projectDir, workingDir){
  
  minPoints<-1
  # set number of waypoints per file
  maxPoints<-ceiling(nrow(df@data)/nofiles)
  
  if (maxPoints > nrow(df@data)) {maxPoints<-nrow(df@data)}
  
  # set original counter according to battery or number of points (708)
  addmax<-maxPoints
  cat(paste0("create ",nofiles, " control files...\n"))
  
  # store launchposition and coordinates we need them for the rth calculations
  row1<-df@data[1,1:(ncol(df@data))]
  launchLat<-df@data[1,8]
  launchLon<-df@data[1,9]
  
  # read dem
  dem<-raster(dem)
  
  # we need to calculate and insert climb and sink waypoints for each task file  
  for (i in 1:nofiles) {
    # take current start position of the split task
    startLat<-df@data[minPoints+1,8]
    startLon<-df@data[minPoints+1,9]
    
    # take current end position of split task
    endLat<-df@data[maxPoints,8]
    endLon<-df@data[maxPoints,9]
    
    # depending on DEM/DSM sometimes there are no data Values
    if (!is.na(endLat) & !is.na(endLon)) {
      # generate flight lines from lanch to start and launch to end point of splitted task
      home<-makeLine(c(launchLon,endLon),c(launchLat,endLat),"Home")
      start<-makeLine(c(launchLon,startLon),c(launchLat,startLat),"Start")
      
      # calculate minimum rth altitude for each line by identifing max altitude
      homeRth<-raster::extract(dem,home,fun=max,na.rm=TRUE)+ as.numeric(p$flightAltitude)-as.numeric(maxAlt)
      startRth<-raster::extract(dem,start,fun=max,na.rm=TRUE)+ as.numeric(p$flightAltitude)-as.numeric(maxAlt)
      
      # add 1/3 third altitude as safety buffer
      homeRth<-homeRth+0.33*homeRth
      startRth<-startRth+0.33*startRth
      
      # non "raster" implementytion slightly faster but needs full gdal installation
      ## copy raster from template
      ##file.copy("rawzero.tif","home.tif",overwrite = TRUE)
      ##gdal_rasterize(src_datasource = "home.shp", dst_filename = "home.tif" , burn = 1)    
      ##system2("gdal_calc.py"," -A 'home.tif' -B 'flightDEM.tif' --outfile='result1.tif' --calc='A*B' -- overwrite",stdout = NULL)
      ##home<-raster("result1.tif")
      ##home<-setMinMax(home)
      ##idx = which.max(home)
      ##homemaxpos = xyFromCell(home,idx)
      
      # get the max position of the flightlines
      homemaxpos<-getmaxposFromLine(dem,home)
      startmaxpos<-getmaxposFromLine(dem,start)
      
      # calculate heading 
      homeheading<-geosphere::bearing(c(endLon,endLat),c(launchLon,launchLat), a=6378137, f=1/298.257223563)
      startheading<-geosphere::bearing(c(launchLon,launchLat),c(startLon,startLat), a=6378137, f=1/298.257223563)
      
      # generate ascent2start waypoint
      ascent2start<-makeUavPointMAV(lat=calcNextPos(launchLon,launchLat,startheading,5)[2],lon=,calcNextPos(launchLon,launchLat,startheading,5)[1],alt=startRth,head=startheading,group=99,raw=FALSE)
      
      # generate maxStartPos waypoint
      maxStartPos<-makeUavPointMAV(lat=startmaxpos[2],lon=,startmaxpos[1],alt=startRth,head=startheading,group=99,raw=FALSE)
      
      # generate ascent2home waypoint
      ascent2home<-makeUavPointMAV(lat=calcNextPos(endLon,endLat,homeheading,5)[2],lon=,calcNextPos(endLon,endLat,homeheading,5)[1],alt=homeRth,head=homeheading,group=99,raw=FALSE)
      
      # generate maxHomePos waypoint
      maxHomePos<-makeUavPointMAV(lat=homemaxpos[2],lon=,homemaxpos[1],alt=homeRth,head=homeheading,group=99,raw=FALSE)
      
      # if maxpoints is greater than the existing number of points reset it
      DF<-df@data[(as.numeric(minPoints)+1):maxPoints,]
      
      # write and re-read waypoints
      sep<-"\t"
      keeps <- c("a","b","c","d","e","f","g","latitude","longitude","altitude","j")
      DF<-DF[keeps]
      DF[complete.cases(DF),]
      write.table(DF[,1:(ncol(DF))],file = "tmp2.csv",quote = FALSE,row.names = FALSE,sep = "\t")
      
      #read raw waypoint list
      lns <- data.table::fread("tmp2.csv", skip=1L, header = FALSE,sep = "\n", data.table = FALSE)
      
      # define output dataframe
      lnsnew<-data.frame()
      
      # create a standard MAV start sequence
      # create default header line  
      lnsnew[1,1] <- "QGC WPL 110"
      # create homepoint 
      lnsnew[2,1] <-       paste0("0",sep,"1",sep,"0",sep,"16",sep,"0",sep,"0",sep,"0",sep,"0",sep,p$launchLat,sep,p$launchLon,sep,as.character(param$launchAltitude),sep,"1")
      # CREATE takeoff
      lnsnew[3,1] <-       paste0("1",sep,"0",sep,"3",sep,"22",sep,"200.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,as.character(startRth),sep,"1")
      #set mission speed
      lnsnew[4,1] <-       paste0("2",sep,"0",sep,"3",sep,"178",sep,"0.0",sep,speed,sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"1")
      
      # add climb waypoint
      lnsnew[5,1] <-       paste0("3",sep,ascent2start)
      # add maxStartPos waypoint
      lnsnew[6,1] <-       paste0("4",sep,maxStartPos)
      
      # insert "normal" task waypoints
      for (j in  seq(1,addmax-1)){
        # just take care of the index in a MAV file
        lnsnew[j+6,1]<-paste0(as.character(j+4),"\t",lns[j,])
      }
      
      # add climb to homflight altitude waypoint
      lnsnew[length(lnsnew[,1])+1,1] <- paste0(as.character(length(lns[,1])+5),sep,ascent2home)
      # add maxhomepos waypoint
      lnsnew[length(lnsnew[,1])+1,1] <- paste0(as.character(length(lns[,1])+6),sep,maxHomePos)
      
      # insert standard MAV home sequence
      #set rth altitude
      lnsnew[length(lnsnew[,1])+1,1]<-  paste0(as.character(length(lns[,1])+7),sep,"0",sep,"3",sep,"30",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,as.character(homeRth),sep,"1")
      #set max return speed
      lnsnew[length(lnsnew[,1])+1,1] <- paste0(as.character(length(lns[,1])+8),sep,"0",sep,"3",sep,"178",sep,"0.0",sep,"250",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"1")
      # trigger rth event
      lnsnew[length(lnsnew[,1])+1,1] <- paste0(as.character(length(lns[,1])+9),sep,"0",sep,"3",sep,"20",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"1")
      
      # write the control file
      write.table(lnsnew, paste0(projectDir,"/", workingDir,"/control/",mission,i,"_solo.waypoints"), sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE,na = "")
      
      # log event 
      levellog(logger, 'INFO', paste("created : ", paste0(mission,"-",i,".csv")))
      
      # counter handling for the last file
      if (maxPoints>nrow(df@data)){
        oldmin<-minPoints
        oldmax<-maxPoints
        maxPoints<-nrow(df@data)
        minPoints<- oldmax
        addmax<-maxPoints-minPoints
      } else {
        minPoints<-maxPoints
        maxPoints<-maxPoints+addmax
      }
    }
  }
}




getSurveyExtent<- function(surveyArea,projectDir,logger){
  
  # check and read mission area coordinates
  if (is.null(surveyArea)) {
    levellog(logger, 'FATAL', '### external flight area file or coordinates missing - dont know what to to')
    stop("### external flight area file or coordinates missing - don't know what to to")
  }
  else {
    # import flight area if provided by an external vector file
    if (class(surveyArea)=="numeric" & length(surveyArea)>= 8){
      surveyArea<-surveyArea
    }
    else if (class(surveyArea)=="numeric" & length(surveyArea)< 8){
      levellog(logger, 'FATAL', "### you did not provide a launching coordinate")
      stop("### you did not provide a launching coordinate")
    }
    else {
      file.copy( from = surveyArea, to = file.path(projectDir,"data"))
      test<-try(flightBound<-readExternalFlightBoundary(surveyArea))
      if (class(test)!="try-error"){
        surveyArea<-flightBound 
      }else{
        levellog(logger, 'FATAL', "### can not find/read input file")        
        stop("### could not read surveyArea file")
      }
    }
  }
  return(surveyArea)
}

# imports the survey area from a json or kml file
importsurveyArea<- function(fN){
  # read shapefile
  if (path.expand(extension(fN)) == ".json") 
    flightBound<-rgdal::readOGR(dsn = path.expand(fN), layer = "OGRGeoJSON",verbose = FALSE)
  else if (path.expand(extension(fN)) != ".kml" ) 
    flightBound<- rgdal::readOGR(dsn = path.expand(dirname(fN)), layer = tools::file_path_sans_ext(basename(fN)),pointDropZ=TRUE,verbose = FALSE)
  else if (path.expand(extension(fN)) == ".kml" ) {
    flightBound<- rgdal::readOGR(dsn = path.expand(fN), layer = tools::file_path_sans_ext(basename(fN)),pointDropZ=TRUE,verbose = FALSE)    
  }
  flightBound@data<-as.data.frame(cbind(1,1,1,1,1,-1,0,-1,1,1,1))
  names(flightBound@data)<-c("Name", "description", "timestamp", "begin",  "end", "altitudeMode", "tessellate", "extrude", "visibility", "drawOrder", "icon")
  return(flightBound)
  
}

# imports the survey area from a list of for coordinates
readExternalFlightBoundary<- function(fN,extend=FALSE){
  flightBound<-importsurveyArea(fN)
  sp::spTransform(flightBound, CRS("+proj=longlat +datum=WGS84 +no_defs"))
  if (extend){
    x<-raster::extent(flightBound)
    
    # first flightline used for length and angle of the parallels
    lon1<-x@xmin # startpoint
    lat1<-x@ymin # startpoint
    lon2<-x@xmin # endpoint
    lat2<-x@ymax # endpoint
    lon3<-x@xmax # crosswaypoint
    lat3<-x@ymax # crosswaypoint
    if (class(flightBound)=="SpatialPolygonesDataFrame") {
      lauchLon<-flightBound@polygons[[1]]@Polygons[[1]]@coords[4,1] 
      launchLat<-flightBound@polygons[[1]]@Polygons[[1]]@coords[4,2]  
    } else if(class(flightBound)=="SpatialLinesDataFrame") {
      launchLon<-flightBound@lines[[1]]@Lines[[1]]@coords[7,1] 
      launchLat<-flightBound@lines[[1]]@Lines[[1]]@coords[7,2]
    }
  } else{
    if (class(flightBound)=="SpatialPolygonesDataFrame") {
      
      lon1<-flightBound@polygons[[1]]@Polygons[[1]]@coords[1,1] 
      lat1<-flightBound@polygons[[1]]@Polygons[[1]]@coords[1,2] 
      
      lon2<-flightBound@polygons[[1]]@Polygons[[1]]@coords[2,1] 
      lat2<-flightBound@polygons[[1]]@Polygons[[1]]@coords[2,2] 
      
      lon3<-flightBound@polygons[[1]]@Polygons[[1]]@coords[3,1] 
      lat3<-flightBound@polygons[[1]]@Polygons[[1]]@coords[3,2] 
      
      lauchLon<-flightBound@polygons[[1]]@Polygons[[1]]@coords[4,1] 
      launchLat<-flightBound@polygons[[1]]@Polygons[[1]]@coords[4,2]       
    }
    if (class(flightBound)=="SpatialLinesDataFrame") {
      
      lon1<-flightBound@lines[[1]]@Lines[[1]]@coords[1,1] 
      lat1<-flightBound@lines[[1]]@Lines[[1]]@coords[1,2] 
      
      lon2<-flightBound@lines[[1]]@Lines[[1]]@coords[2,1] 
      lat2<-flightBound@lines[[1]]@Lines[[1]]@coords[2,2] 
      
      lon3<-flightBound@lines[[1]]@Lines[[1]]@coords[3,1] 
      lat3<-flightBound@lines[[1]]@Lines[[1]]@coords[3,2]
      
      launchLon<-flightBound@lines[[1]]@Lines[[1]]@coords[4,1] 
      launchLat<-flightBound@lines[[1]]@Lines[[1]]@coords[4,2]
      
    }
    
    
  }
  return(c(lat1,lon1,lat2,lon2,lat3,lon3,launchLat,launchLon))
}


#  function to start litchi as a local instance TO BE DONE
# openLitchi<- function(){
#   tempDir <- tempfile()
#   dir.create(tempDir)
#   currentfiles<-list.files(paste0(.libPaths()[1],"/robubu/htmlwidgets/lib/litchi"))
#   dir.create(file.path(tempDir, currentfiles[1]))
#   currentfiles<-list.files(paste0(.libPaths()[1],"/robubu/htmlwidgets/lib/litchi/"))
#   
#   file.copy(from=paste0(.libPaths()[1],"/robubu/htmlwidgets/lib/litchi"), to=file.path(tempDir), 
#             overwrite = TRUE, recursive = TRUE, 
#             copy.mode = TRUE)
#   
#   htmlFile <- file.path(tempDir, "litchi","index.html")
#   # (code to write some content to the file)
#   utils::browseURL(htmlFile)
#   
# }


# calculate the overlap factor of the camera footprints returning an heatmap
fovHeatmap<- function(footprint,dem){
  p<-split(footprint,footprint@plotOrder)
  t <- raster::raster(nrow=nrow(dem)*2,ncol=ncol(dem)*2)
  t@crs <-dem@crs
  t@extent<-dem@extent
  t<-resample(dem,t)
  t[]<-0
  s<-t
  for (i in seq(1:length(footprint))) {
    tmp<-raster::rasterize(p[[i]],t)
    s <- raster::stack(tmp, s)
  }
  fovhm <- raster::stackApply(s, indices= nlayers(s), fun=sum)
  fovhm[fovhm<1]=NaN
  return(fovhm)
}

# create a sppolygon to estimate the pictures footprint 
taskarea<- function(p,csvFn){
  # construct the 4th corner
  crossdir<-geosphere::bearing(c(p$lon2,p$lat2),c(p$lon3,p$lat3), a=6378137, f=1/298.257223563)
  crosslen<-geosphere::distGeo(c(p$lon2,p$lat2),c(p$lon3,p$lat3), a=6378137, f=1/298.257223563)
  p4<-geosphere::destPoint(c(p$lon1,p$lat1), crossdir,crosslen)
  # create SPDF
  ID = paste0("FlightTask_",basename(csvFn))
  rawPolygon <- sp::Polygon(cbind(c(p$lon1,p$lon2,p$lon3,p4[[1]],p$lon1),c(p$lat1,p$lat2,p$lat3,p4[[2]],p$lat1)))
  areaExtent <- sp::Polygons(list(rawPolygon), ID = ID)
  areaExtent <- sp::SpatialPolygons(list(areaExtent))
  df <- data.frame( ID=1:length(rawPolygon), row.names = ID)
  area <- sp::SpatialPolygonsDataFrame(areaExtent, df)
  sp::proj4string(area) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  return(area)
} 

# calculates the camera footprint 
cameraExtent<- function(lon,lat,heading,distance,flightaltitude,i,j){
  
  t1<-calcNextPos(lon,lat,abs(heading),1.71*flightaltitude/2)
  t2<-calcNextPos(lon,lat,abs(heading),-1*(1.71*flightaltitude/2))
  
  
  yllc<-calcNextPos(t1[1],t1[2],-90+abs(heading),1.71*flightaltitude*0.75/2)[2]
  xllc<-calcNextPos(t1[1],t1[2],-90+ abs(heading),1.71*flightaltitude*0.75/2)[1]
  ylrc<-calcNextPos(t1[1],t1[2],90+abs(heading),1.71*flightaltitude*0.75/2)[2]
  xlrc<-calcNextPos(t1[1],t1[2],90+abs(heading),1.71*flightaltitude*0.75/2)[1]
  
  yulc<-calcNextPos(t2[1],t2[2],-90+abs(heading),1.71*flightaltitude*0.75/2)[2]
  xulc<-calcNextPos(t2[1],t2[2],-90+abs(heading),1.71*flightaltitude*0.75/2)[1]
  yurc<-calcNextPos(t2[1],t2[2],90+abs(heading),1.71*flightaltitude*0.75/2)[2]
  xurc<-calcNextPos(t2[1],t2[2],90+abs(heading),1.71*flightaltitude*0.75/2)[1]
  
  ID = paste0("CameraExtend_",flightaltitude,"_",lon,lat)
  rawPolygon <- sp::Polygon(cbind(c(xulc,xurc,xlrc,xllc,xulc),c(yulc,yurc,ylrc,yllc,yulc)))
  tileExtend <- sp::Polygons(list(rawPolygon), ID = ID)
  tileExtend <- sp::SpatialPolygons(list(tileExtend))
  df <- data.frame( ID=1:length(rawPolygon), row.names = ID)
  frame <- sp::SpatialPolygonsDataFrame(tileExtend, df)
  sp::proj4string(frame) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  return(frame)
} 

getPresetTask<- function (param="remote"){
  #' shows existing camera action presets 
  #' @description 
  #' NOTE: only for flightPlanMode = "waypoint")
  # preset waypoints & orthophoto
  
  if  (param == "multi_ortho") {
    flightParams=actiontype=c(1,0,4,0,5,-60,1,0,4,90,1,0,4,180,1,0,4,270,1,0)
    task<-makeTaskParamList(flightParams[1:length(flightParams)])
  }
  # preset waypoints  take vertical picture at wp
  else if (param == "simple_ortho") { 
    flightParams=actiontype=c(5,-90,1,0)
    task<-makeTaskParamList(flightParams[1:length(flightParams)])
  }
  else if (param == "simple_pano") { 
    flightParams=actiontype=c(4,-180,1,0,4,-128,1,0,4,-76,1,0,4,-24,1,0,4,28,1,0,4,80,1,0,4,132,1,0,-1,0) 
    task<-makeTaskParamList(flightParams[1:length(flightParams)])
  }  # preset waypoints  take vertical picture at wp
  else if (param == "remote") { 
    flightParams=actiontype=c(-1,0)
    task<-makeTaskParamList(flightParams[1:length(flightParams)])
  }
  else if (param == "treetop") { 
    flightParams=actiontype=c(0,1000,5,-90,1,0,1,0,5,-70,1,0,4,-90,1,0,4,90,5,-30,-1,0,-1,0,-1,0,-1,0,-1,0)
    task<-makeTaskParamList(flightParams[1:length(flightParams)])
  }
  else if (param == "nothing") { 
    flightParams=actiontype=c(-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0,-1,0)
    task<-makeTaskParamList(flightParams[1:length(flightParams)])
  }
  return(task)
}

# create and recalculates all arguments for a drone waypoint
makeFlightParam<- function(surveyArea,flightParams,followSurface){
  # retrieve and recalculate the arguments to provide the flight paramaer for litchi
  validPreset<-c("multi_ortho","simple_ortho","simple_pano","remote","treetop","nothing")
  validFlightPlan<-c("waypoints","track","manual")
  stopifnot(flightParams["presetFlightTask"] %in% validPreset)
  stopifnot(flightParams["flightPlanMode"] %in% validFlightPlan)
  
  if (followSurface == TRUE){
    flightParams["flightPlanMode"] = "terrainTrack"
  }
  
  p<-list()
  
  # preset camera action at waypoints 
  task<-getPresetTask(flightParams["presetFlightTask"])  
  
  # flight area coordinates either from external file or from argument list
  p$lat1<-surveyArea[1]
  p$lon1<-surveyArea[2]
  p$lat2<-surveyArea[3]
  p$lon2<-surveyArea[4]
  p$lat3<-surveyArea[5]
  p$lon3<-surveyArea[6]
  p$launchLat<- surveyArea[7]
  p$launchLon<- surveyArea[8]
  p$launchAltitude<-flightParams["launchAltitude"]
  # rest of the arguments  
  p$flightPlanMode<- flightParams["flightPlanMode"] # waypoints, terrainTrack track
  p$flightAltitude<-flightParams["flightAltitude"]  # planned static altitude above ground (note from starting point)
  p$curvesize<-flightParams["curvesize"]      # default may be set t0 zero
  p$rotationdir<-flightParams["rotationdir"]      # default nothing
  p$gimbalmode<-flightParams["gimbalmode"]       # default nothing 
  p$gimbalpitchangle<-flightParams["gimbalpitchangle"] # default nothing
  p$overlap<-overlap<-flightParams["overlap"]    # overlapping factor 0-1 default 0.6
  p$task<-task  # camera task
  p$followSurfaceRes<-flightParams["followSurfaceRes"]
  return(p)
}

# creates task paramter list
makeTaskParamList<- function(x) {
  actionNames<-list()
  j<-1
  for (i in seq(1:(length(x)/2)) ){
    actionNames[j]<-paste0("actiontype",i)
    actionNames[j+1]<-paste0("actionparam",i) 
    j=j+2
  }
  return(cbind(actionNames,x))
  
}

# calculate a new position from given lat lon
calcNextPos<- function(lon,lat,heading,distance){
  p<-geosphere::destPoint(c(lon,lat), heading, distance)
  return(c(p[1],p[2]))
}




dumpFile = function(filepath) {
  con = file(filepath, "r")
  while ( TRUE ) {
    line = readLines(con, n = 1)
    if ( length(line) == 0 ) {
      break
    }
    print(line)
  }
  
  close(con)
}

# calculates the overall flight distance
calcTrackDistance<- function (fliAltRatio,flightAltitude,factor=1.71){
  
  trackDistance<-(fliAltRatio*(factor*flightAltitude))
  
}



calculateFlightTime<- function(maxFlightTime,windCondition,maxSpeed,uavOptimumspeed,flightLength,totalTrackdistance,picRate,logger) {
  # wind speed adaption for reducing the lifetime of the battery Roughly the Beaufort scale is used
  
  if (windCondition==1){
    windConditionFactor<-1
  } else if (windCondition==2){
    windConditionFactor<-0.8
  } else if (windCondition==3){
    windConditionFactor<-0.6
  } else if (windCondition==4){
    windConditionFactor<-0.4
  } else if (windCondition < 4){
    windConditionFactor<-0.0
    levellog(logger, 'INFO', "come on, it is a uav not the falcon...")  
    stop("come on, it is a uav not the falcon...")
  }
  
  # log preset picture rate sec/pic
  levellog(logger, 'INFO', paste("original picture rate: ", picRate,"  (sec/pic) "))    
  
  #   # calculate speed & time parameters  
  if (maxSpeed>uavOptimumspeed) {
    maxSpeed<-uavOptimumspeed
    levellog(logger, 'INFO',paste( "optimum speed forced to ", uavOptimumspeed," km/h \n"))
    cat("\n optimum speed forced to ", uavOptimumspeed," km/h \n")
  }
  # calculate time need to fly the task
  rawTime<-round(((flightLength/1000)/maxSpeed)*60,digit=1)
  
  # calculate the corresponding (raW)  timeintevall for each picture
  picIntervall<-round(rawTime*60/(flightLength/totalTrackdistance),digits = 1)
  levellog(logger, 'INFO', paste("initial speed estimation  : ", round(maxSpeed,digit=1),   "  (km/h)      "))
  while (picIntervall< picRate){
    maxSpeed<-maxSpeed-1
    rawTime<-round(((flightLength/1000)/maxSpeed)*60,digit=1)
    rawTime<-rawTime*windConditionFactor
    picIntervall<-round(rawTime*60/(flightLength/totalTrackdistance),digits = 1)
    levellog(logger, 'INFO', paste("decrease speed to  : ", round(maxSpeed,digit=1),   "  (km/h)      "))
  }
  
  # APPLY battery lifetime loss by windspeed
  maxFlightTime<-maxFlightTime*windConditionFactor
  return(c(rawTime,maxFlightTime,maxSpeed,picIntervall))
}


# assign launching point 
launch2flightalt<- function(p,lns,uavViewDir,launch2startHeading,uavType) {
  launchPos<-c(p$launchLon,p$launchLat)
  if (uavType=="djip3"){lns[length(lns)+1]<-makeUavPoint(launchPos,uavViewDir,group=99,p)}
  if (uavType=="solo"){lns[length(lns)+1]<-makeUavPointMAV(lat=launchPos[2],lon=launchPos[1],head=uavViewDir,group=99)}
  pOld<-launchPos
  pos<-calcNextPos(pOld[1],pOld[2],launch2startHeading,10)
  if (uavType=="djip3"){lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group=99,p)}
  if (uavType=="solo"){lns[length(lns)+1]<-makeUavPointMAV(lat=pos[2],lon=pos[1],head=uavViewDir,group=99)}
  return(lns)
}

MAVTreeCSV <-function(flightPlanMode,trackDistance,logger,p,param,maxSpeed=maxSpeed/3.6){
  mission<-p$missionName
  
  df<-param[[2]]
  dem<-param[[3]]
  maxAlt<-param[[6]]
  
  minPoints<-1
  #nofiles<- ceiling(rawTime/batteryTime)
  nofiles<-1
  maxPoints<-nrow(df@data)
  mp<-maxPoints
  a<-0
  b<-0
  c<-3
  d<-0
  e<-0
  f<-0
  g<-0
  id<-99
  j<-1
  
  
  #if (maxPoints > nrow(df@data)) {maxPoints<-nrow(df@data)}
  # store launchposition and coordinates we need them for the rth calculations
  
  for (i in 1:nofiles) {
    # take current start position of the split task
    actualLat<-df@data[minPoints,8]
    actualLon<-df@data[minPoints,9]
    # take current end position of split task
    nextLat<-df@data[maxPoints,8]
    nextLon<-df@data[maxPoints,9]
    # generate flight lines from lanch to start and launch to end point of splitted task
    yhome <- c(actualLat,nextLat)
    xhome <- c(actualLon,nextLon)
    home<-SpatialLines(list(Lines(Line(cbind(xhome,yhome)), ID="home")))
    sp::proj4string(home) <-sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
    #sp::proj4string(start) <-sp::CRS("+proj=longlat +datum=WGS84 +no_defs")
    
    # calculate minimum rth altitude for each line by identifing max altitude
    homeRTH<-max(unlist(raster::extract(dem,home)))+as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    
    # write and re-read waypoints
    sep<-"\t"
    keeps <- c("a","b","c","d","e","f","g","latitude","longitude","altitude","j")
    df@data<-df@data[keeps]
    write.table(df@data[minPoints:maxPoints,1:(ncol(df@data))],file = "tmp.csv",quote = FALSE,row.names = FALSE,sep = "\t")
    lns <- data.table::fread("tmp.csv", skip=1L, header = FALSE,sep = "\n", data.table = FALSE)
    lnsnew<-data.frame()
    
    # create default header line  
    lnsnew[1,1] <- "QGC WPL 110"
    # create homepoint 
    lnsnew[2,1] <-       paste0("0",sep,"1",sep,"0",sep,"16",sep,"0",sep,"0",sep,"0",sep,"0",sep,p$launchLat,sep,p$launchLon,sep,as.character(param$launchAltitude),sep,"1")
    # CREATE takeoff
    lnsnew[3,1] <-       paste0("1",sep,"0",sep,"3",sep,"22",sep,"200.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,as.character(param$flightAltitude),sep,"1")
    #set mission speed
    lnsnew[4,1] <-       paste0("2",sep,"0",sep,"3",sep,"178",sep,"0.0",sep,maxSpeed,sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"1")
    
    # create "normal" waypoints
    for (j in 1:length(lns[,1])) {
      lnsnew[j+4,1]<-paste0(as.character(j+2),"\t",lns[j,])
    }
    
    #set rth altitude
    lnsnew[length(lnsnew[,1])+1,1]<-  paste0(as.character(length(lns[,1])+1),sep,"0",sep,"3",sep,"30",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,as.character(homeRTH),sep,"1")
    #set max return speed
    lnsnew[length(lnsnew[,1])+1,1] <- paste0(as.character(length(lns[,1])+1),sep,"0",sep,"3",sep,"178",sep,"0.0",sep,"250",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"1")
    # trigger rth event
    lnsnew[length(lnsnew[,1])+1,1] <- paste0(as.character(length(lns[,1])+1),sep,"0",sep,"3",sep,"20",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,"1")
    # write the control file
    write.table(lnsnew, paste0(strsplit(getwd(),"/run")[[1]][1],"/control/",mission,i,"_solo.waypoints"), sep="\t", row.names=FALSE, col.names=FALSE, quote = FALSE,na = "")
    # log event 
    levellog(logger, 'INFO', paste("created : ", paste0(mission,"-",i,".csv")))
    minPoints<-maxPoints
    maxPoints<-maxPoints+mp
    if (maxPoints>nrow(df@data)){
      maxPoints<-nrow(df@data)
    }
  }
  
}


readTreeTrack<- function(treeTrack){
  tTkDF<-read.csv(treeTrack,sep="\t",header = TRUE)
  sp::coordinates(tTkDF) <- ~x+y
  sp::proj4string(tTkDF) <- sp::CRS("+proj=utm +zone=33 +datum=WGS84 +no_defs")
  tTkDF<-spTransform(tTkDF, sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
  return(tTkDF)
}

makeFlightPath<- function(treeList,p,uavType,task,demFn,logger){
  
  # calc next coordinate
  lns<-list()
  
  # assign launching point 
  
  #lns[length(lns)+1]<-makeUavPoint(launchPos,uavViewDir,group=99,p)
  fileConn<-file("treepoints.csv")
  for(i in 2:nrow(treeList)-1){
    
    #if (mode =="track"){group<-99}
    #if (uavType=="djip3"){lns[length(lns)+1]<-makeUavPoint(pos,uavViewDir,group,p)}
    #if (uavType=="solo"){lnsMAV[length(lnsMAV)+1]<-makeUavPointMAV(pos,uavViewDir,group,p)}
    
    #print(treeListDf[i,4])
    
    
    if (uavType=="djip3"){
      forward<-geosphere::bearing(treeList@coords[i,],treeList@coords[i+1,], a=6378137, f=1/298.257223563)
      backward<-geosphere::bearing(treeList@coords[i+1,],treeList@coords[i,], a=6378137, f=1/298.257223563)
      p$task<- getPresetTask("treetop")
      lns[length(lns)+1]<- makeUavPoint(treeList@coords[i,],forward,p,group=99)
      p$task<- getPresetTask("nothing")
      posUp<- calcNextPos(treeList@coords[i,][1],treeList@coords[i,][2],heading=forward,distance=p$climbDist)
      lns[length(lns)+1]<- makeUavPoint  (posUp,forward,p,group=1)
      posDown<- calcNextPos(treeList@coords[i+1,][1],treeList@coords[i+1,][2],backward,distance=p$climbDist)
      lns[length(lns)+1]<- makeUavPoint(posDown,forward,p,group=1)
      writeLines(unlist(lns), fileConn)
    }
    else if (uavType=="solo"){
      forward<-geosphere::bearing(treeList@coords[i,],treeList@coords[i+1,], a=6378137, f=1/298.257223563)
      backward<-geosphere::bearing(treeList@coords[i+1,],treeList@coords[i,], a=6378137, f=1/298.257223563)
      p$task<- getPresetTask("treetop")
      lns[length(lns)+1]<- makeUavPointMAV(lat=treeList@coords[i,][2],lon=treeList@coords[i,][1],head=forward,group=99)
      p$task<- getPresetTask("nothing")
      posUp<- calcNextPos(treeList@coords[i,][1],treeList@coords[i,][2],heading=forward,distance=p$climbDist)
      lns[length(lns)+1]<- makeUavPointMAV(lat=posUp[2],lon=posUp[1],forward,group=99) 
      posDown<- calcNextPos(treeList@coords[i+1,][1],treeList@coords[i+1,][2],backward,distance=p$climbDist)
      lns[length(lns)+1]<- makeUavPointMAV(lat=posDown[2],lon=posDown[1],forward,group=99) 
      writeLines(unlist(lns), fileConn)
      
    }
    
  }
  close(fileConn)
  if (uavType=="djip3"){
    cat("calculating DEM related stuff\n")
    djiDF<-read.csv("treepoints.csv",sep=",",header = FALSE)
    names(djiDF) <-unlist(strsplit( makeUavPoint(pos,uavViewDir,group=99,p,header = TRUE,sep=' '),split = " "))
    sp::coordinates(djiDF) <- ~lon+lat
    sp::proj4string(djiDF) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    result<-getAltitudes(demFn ,djiDF,p,followSurfaceRes=5,logger)
    #result<-demCorrection(demFn, djiDF,p,p$altFilter,p$followSurface,p$followSurfaceRes,logger,projectDir)
    #result<-demCorrection(demFn ,djiDF,p,followSurface=followSurface,followSurfaceResfollowSurfaceRes,logger=logger,projectDir=projectDir)
    #    write.csv(djiDF@data,file = paste0(strsplit(getwd(),"/tmp")[[1]][1],"/control/","mission",".csv"),quote = FALSE,row.names = FALSE)
    #writeDjiTreeCsv(result[[2]],p$missionName)
    writeDjiTreeCSV(result[[2]],p$missionName,1,94,p,logger,round(result[[4]],digit=0),trackSwitch,result[[3]],result[[6]])
    
    return(result)
    
  } else if (uavType=="solo"){
    cat("calculating DEM related stuff\n")
    df<-read.csv("treepoints.csv",sep="\t",header = FALSE)
    #names(df) <-unlist(strsplit( makeUavPointMAV(pos,uavViewDir,group=99,p,header = TRUE,sep=' '),split = " "))
    names(df) <-c("a","b","c","d","e","f","g","lat","lon","latitude","longitude","altitude","id","j")
    sp::coordinates(df) <- ~lon+lat
    sp::proj4string(df) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    result<-getAltitudes(demFn ,df,p,followSurfaceRes=5,logger)
    MAVTreeCSV(flightPlanMode="track",trackDistance=10000,logger=logger,p=p,param=result,maxSpeed=p$maxSpeed)
    
    return(result)
    
  }
}

getAltitudes<- function(demFn ,df,p,followSurfaceRes,logger){
  if (is.null(demFn)){
    levellog(logger, 'WARN', "CAUTION!!! no DEM file provided I try to download SRTM data... SRTM DATA has a poor resolution for UAVs!!! ")
    cat("\nCAUTION! No DEM data is provided.\n trying to download SRTM data... \n Be aware that the resulution of SRTM is NOT sufficient for terrain following flights!")
    # download corresponding srtm data
    dem<-robubu::getGeoData(name="SRTM",xtent = extent(p$lon1,p$lon3,p$lat1,p$lat3), zone = 1.0,merge = TRUE)
    dem<-setMinMax(dem)
    rundem<- raster::crop(dem,extent(min(p$lon1,p$lon3)-0.0083,max(p$lon1,p$lon3)+0.0083,min(p$lat1,p$lat3)-0.0083,max(p$lat1,p$lat3)+0.0083))
    raster::writeRaster(dem,"tmpdem.tif",overwrite=TRUE)
  } else {
    # read local dem file
    if (class(demFn)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")){
      rundem<-demFn
      rundem<- raster::crop(rundem,extent(min(p$lon1,p$lon3)-0.0083,max(p$lon1,p$lon3)+0.0083,min(p$lat1,p$lat3)-0.0083,max(p$lat1,p$lat3)+0.0083))
      raster::writeRaster(rundem,"tmpdem.tif",overwrite=TRUE)
      dem<-rundem
    } else{
      rundem<-raster::raster(demFn,xmn=min(p$lon1,p$lon3)-0.0083,xmx=max(p$lon1,p$lon3)+0.0083,ymn=min(p$lat1,p$lat3)-0.0083,ymx=max(p$lat1,p$lat3)+0.0083)
      #rundem<- raster::crop(rundem,extent(min(p$lon1,p$lon3)-0.0083,max(p$lon1,p$lon3)+0.0083,min(p$lat1,p$lat3)-0.0083,max(p$lat1,p$lat3)+0.0083))
      file.copy(demFn, paste0(file.path(projectDir,workingDir,"run"),"/tmpdem.tif"))
      #raster::writeRaster(rundem,"tmpdem.tif",overwrite=TRUE)
      dem<-rundem
    }
  }  # end of loading DEM data
  
  
  # check if at least a projection string exist 
  #res<-compareProjCode(as.vector(as.character(rundem@crs)))
  #demll<-rasterCheckAdjustProjection(rundem)
  crsString<-compareProjCode(as.vector(as.character(rundem@crs)))
  if (!crsString) {
    stop("the DEM/DSM is not georeferencend - please provide a correct georeferenced raster object or GeoTiff file\n")
    # if so deproject DEM/DSM because all of the vector data is latlong WGS84
  } else {
    demll<-gdalwarp(srcfile = "tmpdem.tif", dstfile = "demll.tif", overwrite=TRUE,  t_srs="+proj=longlat +datum=WGS84 +no_defs",output_Raster = TRUE )  
    demll<-setMinMax(demll)
    
  }
  
  # extract all waypoint altitudes
  altitude<-raster::extract(demll,df)
  # get maximum altitude of the task area
  maxAlt<-max(altitude,na.rm = TRUE)
  log4r::levellog(logger, 'INFO', paste("maximum DEM Altitude : ", maxAlt," m"))
  # if no manually provided launch altitude exist get it from DEM
  pos<-as.data.frame(cbind(p$launchLat,p$launchLon))
  sp::coordinates(pos) <- ~V2+V1
  sp::proj4string(pos) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  if (p$launchAltitude==-9999){
    tmpalt<-raster::extract(demll,pos)  
    p$launchAltitude<-as.numeric(tmpalt)
    # otherwise take it from the parameter set
  } else 
  {
    p$launchAltitude<-as.numeric(p$launchAltitude)
  }
  log4r::levellog(logger, 'INFO', paste("launching Altitude : ", p$launchAltitude," m"))
  launchAlt<-p$launchAltitude
  # calculate the flight altitude shift due to launching and max altitude
  p$flightAltitude=as.numeric(p$flightAltitude)+(maxAlt-as.numeric(launchAlt))
  p$aboveTreeAlt=as.numeric(p$aboveTreeAlt)+(maxAlt-as.numeric(launchAlt))
  
  rthFlightAlt<-p$flightAltitude
  p$rthAltitude=rthFlightAlt
  log4r::levellog(logger, 'INFO', paste("rthFlightAlt : ", rthFlightAlt," m"))
  rawAltitude<-altitude
  altitude<-altitude+as.numeric(p$flightAltitude)-maxAlt
  df$altitude<-altitude
  
  
  taltitude<-as.data.frame(rawAltitude+as.numeric(p$aboveTreeAlt)-maxAlt)
  taltitude$id=df@data$id
  names(taltitude)<-c("altitude","id")
  tmp<-df@data
  tmp$altitude[tmp$id == 99 ]<-taltitude$altitude[taltitude$id==99 ]
  df$altitude<-tmp$altitude
  return<-c(pos,df,demll,rthFlightAlt,launchAlt,maxAlt)
  names(return)<-c("lp","wp","dsm","rth","la","xa")
  return(return)
}

readLaunchPos<- function(fN,extend=FALSE){
  if (class(fN) != "numeric") {
    flightBound<-importsurveyArea(fN)
    launchLon<-flightBound@polygons[[1]]@Polygons[[1]]@coords[1,1] 
    launchLat<-flightBound@polygons[[1]]@Polygons[[1]]@coords[1,2] 
  }
  else{
    # create SPDF
    # points from scratch
    coords = cbind(fN[1],fN[2])
    launchPos = sp::SpatialPoints(coords)
    launchPos = SpatialPointsDataFrame(coords, as.data.frame("LaunchPosition"))
    # promote data frame to spatial
    sp::proj4string(launchPos) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  }
  return(launchPos)
}

# export data to xternal format deals with the splitting of the mission files
writeDjiTreeCsv <-function(df,mission){
  # max numbers of waypoints is 99
  nofiles<-ceiling(nrow(df@data)/96)
  maxPoints<-96
  minPoints<-1
  maxFlightLength <- 15
  
  
  
  for (i in 1:nofiles) {
    if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
    write.csv(df@data[minPoints:maxPoints,1:(ncol(df@data)-2)],file = paste0(strsplit(getwd(),"/tmp")[[1]][1],"/control/",mission,i,".csv"),quote = FALSE,row.names = FALSE)
    minPoints<-maxPoints
    maxPoints<-maxPoints+96
    
    if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
  }
}

# export data to xternal format deals with the splitting of the mission files
writeDjiTreeCSV <-function(df,mission,nofiles,maxPoints,p,logger,rth,trackSwitch=FALSE,dem,maxAlt){
  minPoints<-1
  if (maxPoints > nrow(df@data)) {maxPoints<-nrow(df@data)}
  # store launchposition and coordinates we need them for the rth calculations
  row1<-df@data[1,1:(ncol(df@data))]
  
  launchLat<-p$launchLat
  launchLon<-p$launchLon
  
  for (i in 1:nofiles) {
    # take current start position of the split task
    startLat<-df@data[minPoints,1]
    startLon<-df@data[minPoints,2]
    # take current end position of split task
    endLat<-df@data[maxPoints,1]
    endLon<-df@data[maxPoints,2]
    # generate flight lines from lanch to start and launch to end point of splitted task
    yhome <- c(launchLat,endLat)
    xhome <- c(launchLon,endLon)
    ystart <- c(launchLat,startLat)
    xstart <- c(launchLon,startLon)
    start<-SpatialLines(list(Lines(Line(cbind(xstart,ystart)), ID="start")))
    home<-SpatialLines(list(Lines(Line(cbind(xhome,yhome)), ID="home")))
    sp::proj4string(home) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    sp::proj4string(start) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
    
    # calculate minimum rth altitude for each line by identifing max altitude
    homeRth<-max(unlist(raster::extract(dem,home)))+as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    startRth<-max(unlist(raster::extract(dem,start)))+as.numeric(p$flightAltitude)-as.numeric(maxAlt)
    
    # calculate rth heading 
    homeheading<-geosphere::bearing(c(endLon,endLat),c(launchLon,launchLat), a=6378137, f=1/298.257223563)
    startheading<-geosphere::bearing(c(startLon,startLat),c(launchLon,launchLat), a=6378137, f=1/298.257223563)
    
    altitude<-startRth
    latitude<-  launchLat<-p$launchLat
    longitude<-launchLon<-p$launchLon
    heading<-startheading
    # generate ascent waypoint to realize save fly home altitude
    rowStart<-cbind(latitude,longitude,altitude,heading,row1[5:ncol(df@data)])
    
    # calculate rth ascent from last task position
    pos<-calcNextPos(endLon,endLat,homeheading,10)
    
    # generate rth waypoints
    heading<-homeheading
    altitude<-homeRth
    latitude<-pos[2]
    longitude<-pos[1]
    # generate ascent waypoint to realize save fly home altitude
    ascentrow<-cbind(latitude,longitude,altitude,heading,rowStart[5:ncol(df@data)])
    # generate home position with heading and altitude
    homerow<-cbind(rowStart[1:2],altitude,heading,rowStart[5:ncol(df@data)])
    # genrate launch to start waypoint to realize save fly home altitude
    heading<-homeheading
    altitude<-startRth
    startrow<-cbind(rowStart[1:2],altitude,heading,rowStart[5:ncol(df@data)])
    
    # append this three points to each part of the splitted task
    DF<-df@data[minPoints:maxPoints,]
    DF = rbind(startrow,DF)
    DF = rbind(DF,ascentrow)
    DF = rbind(DF,homerow)
    
    #if (maxPoints>nrow(DF)){maxPoints<-nrow(DF)}
    write.csv(DF[,1:(ncol(DF)-2)],file = paste0(strsplit(getwd(),"/tmp")[[1]][1],"/control/",mission,i,".csv"),quote = FALSE,row.names = FALSE)
    
    
    levellog(logger, 'INFO', paste("created : ", paste0(strsplit(getwd(),"/tmp")[[1]][1],"/control/",mission,"-",i,".csv")))
    minPoints<-maxPoints
    maxPoints<-maxPoints+94
    
    if (maxPoints>nrow(df@data)){maxPoints<-nrow(df@data)}
  }
}


################################

long2UTMzone <- function(long) {
  (floor((long + 180)/6) %% 60) + 1
}
rad2deg <- function(rad) {(rad * 180) / (pi)}

deg2rad <- function(deg) {(deg * pi) / (180)}

# inserts a row in a dataframe
insertRow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

# create the full argument list for one waypoint in MAV format
makeUavPointMAV<- function(lat=0,lon=0,alt=100,head=0,a=0,b=3,c=16,d=0,e=0,f=0,g=0,j=1,dif=22,header=FALSE,sep="\t",speed="11.8",group,lf=FALSE,raw=TRUE){
  a<-a
  b<-b
  c<-c
  d<-d
  e<-e
  f<-f
  g<-g
  id<-group
  j<-j
  dif<-dif
  heading<-head
  altitude<-alt
  latitude<-lat
  longitude<-lon
  if(raw){
    wpLine<-paste0(a,sep,b,sep,c,sep,d,sep,e,sep,f,sep,g,sep,round(latitude,digit=6),sep,round(longitude,digit=6),sep,round(altitude,digit=1),sep,id,sep,j,sep,latitude,sep,longitude)
  } else {
    wpLine<-paste0(a,sep,b,sep,c,sep,d,sep,e,sep,f,sep,g,sep,round(latitude,digit=6),sep,round(longitude,digit=6),sep,round(altitude,digit=1),sep,j) 
  }
  if (lf) {
    LF<-"\n"
  } else {
    LF<-NULL}
  
  # CREATE NORMAL WAYPOINT
  
  return(wpLine)    
}

# create the full argument list for one waypoint in MAV format
#makeUavPoint_MAV<- function(coord=NULL,heading=NULL,group=NULL,p=NULL,header=FALSE,sep="\t",speed="11.8"){
#  # create the value lines
#  if (!header){
#    # CREATE NORMAL WAYPOINT
#    tmp <-    paste0("0",sep,"3",sep,"16",sep,"0.0",sep,"0.0",sep,"0.0",sep,"0.0",sep,pos[2],sep,pos[1],sep,pos[2],sep,pos[1],sep,as.character(p$flightAltitude),sep,group,sep,"1\n")
#    
#  }
#  # create the header
#}

DEM2FlSurface<- function(p,dem,logger){
  
  cat("\n optimizing the DSM for low altitude flights...\n")
  
  # resample dem to followTerrainRes to do so it  is easier to use a projection like UTM
  tmpdem<-gdalwarp(srcfile = "demll.tif", dstfile = "tmpdem.tif",  overwrite=TRUE,  t_srs=paste0("+proj=utm +zone=",long2UTMzone(p$lon1)," +datum=WGS84"),output_Raster = TRUE ,tr=c(as.numeric(p$followSurfaceRes),as.numeric(p$followSurfaceRes)))
  
  # deproject it again to latlon
  demll<-gdalwarp(srcfile = "tmpdem.tif", dstfile = "demll.tif", overwrite=TRUE,  t_srs="+proj=longlat +datum=WGS84 +no_defs",output_Raster = TRUE )
  
  
  # export it to SAGA
  gdalwarp("demll.tif","demll.sdat", overwrite=TRUE,  of='SAGA')
  
  # fill sinks (clearings) that are 0-30 meters deep
  ret<-system2("saga_cmd", c("ta_preprocessor 2", "-DEM=demll.sgrd", "-SINKROUTE=NULL", "-DEM_PREPROC='flightdem.sdat'", "-METHOD=1", "-THRESHOLD=1", "-THRSHEIGHT=30.000000"),stdout=TRUE, stderr=TRUE)
  if (grep("%okay",ret)){
    cat("filling clearings performs okay\n")}
  else {
    stop("Crucial Error in filling flight surface")
  }
  
  # filter the result
  ret<-system2("saga_cmd", c("grid_filter 0","-INPUT='flightdem.sgrd'", "-RESULT='flightsurface.sdat'" ,"-METHOD=0", "-MODE=0" ,paste0("-RADIUS=",p$followSurfaceRes)),stdout=TRUE, stderr=TRUE)
  if (grep("%okay",ret)){ 
    cat("filtering flight surface performs okay\n")}
  else {
    stop("Crucial Error in filtering flight surface")}
  # make a raster object
  demll<-raster("flightsurface.sdat")
  demll<-setMinMax(demll)
  dem<-setMinMax(dem)
  
  # take the maximum alt difference of boths DEMs as a correction value
  altCor<-ceiling(maxValue(dem)-maxValue(demll))
  demll=demll+altCor
  writeRaster(demll,"flightDEM.tif", overwrite=TRUE)
  levellog(logger, 'INFO', paste("altitude shift              : ",altCor,      "  (meter)")) 
  return(demll)
  
}

# Project Raster* objects for mapView 
rasterCheckAdjustProjection <- function(x) {
  llcrs <- "+proj=longlat +datum=WGS84 +no_defs"
  
  is.fact <- raster::is.factor(x)[1]
  
  non_proj_waning <-
    paste("supplied", class(x)[1], "has no projection information!", "\n",
          "provide a correctly georeferenced data raster object or 'GDAL File")
  
  if (is.fact) {
    x <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(llcrs)),
      method = "ngb")
    x <- raster::as.factor(x)
  } else {
    x <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(llcrs)),
      method = "bilinear")
  }
  
  return(x)
  
}

# Check projection of objects according to their keywords -------

compareProjCode <- function (x){
  proj <- datum <- nodefs <- "FALSE"
  allWGS84<- as.vector(c("+init=epsg:4326", "+proj=longlat", "+datum=WGS84", "+no_defs", "+ellps=WGS84", "+towgs84=0,0,0"))
  s<-as.vector(strsplit(x," "))
  for (i in seq(1:length(s[[1]]))){
    
    if (s[[1]][i] == "+init=epsg:4326") {
      proj <- datum <- nodefs <- "TRUE"
    }
    if (s[[1]][i] == "+proj=longlat") {
      proj<- "TRUE"
    }
    if (s[[1]][i] == "+no_defs") {
      nodefs<-"TRUE"
    }
    if (s[[1]][i] == "+datum=WGS84") {
      datum<-"TRUE"
    }
  }
  if (proj == "TRUE" & nodefs == "TRUE" &  datum == "TRUE") {
    ret<-TRUE
  } else{
    ret=FALSE
  }
  return(ret)
}

# Check and potentially adjust projection of objects to be rendered -------

checkAdjustProjection <- function(x) {
  
  if (class(x)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    x <- rasterCheckAdjustProjection(x)
  }
  
  return(x)
}

# create an spatiallineobject from 2 points
# optional export as shapefile
makeLine<- function(Lon,Lat,ID,export=FALSE){  
  line<-SpatialLines(list(Lines(Line(cbind(Lon,Lat)), ID=ID)))
  sp::proj4string(line) <-CRS("+proj=longlat +datum=WGS84 +no_defs")
  if (export){
    writeLinesShape(home,"home.shp")
    writeLinesShape(start,"start.shp")
  }
  return(line)
}

getmaxposFromLine <- function(dem,line){
  mask<- dem
  values(mask)=NA
  #...update it with the altitude information of the flightline
  mask<-rasterize(line,mask)
  mask2<-mask*dem
  # and find the position of the max altitude
  idx = which.max(mask2)
  maxPos = xyFromCell(mask2,idx)
  return(maxPos)
}