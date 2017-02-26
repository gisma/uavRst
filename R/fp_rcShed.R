if (!isGeneric('rcShed')) {
  setGeneric('rcShed', function(x, ...)
    standardGeneric('rcShed'))
}
#'@name rcShed
#'@title calculates the viewshed alias remote control range for a given launching point and the calculated flight altitude.
#'
#'@description
#' viewshed analysis to derive an rough estimation of rc range.
#'
#'@usage rcShed(runDir,launchP,flightAlt)
#'
#'@author Chris Reudenbach
#'
#'
#'@param runDir the choosen working directory
#'@param launchP actual launching position aka position of the RC
#'@param flightAlt flight altitude


#'@return rcShed basically returns mask raster 0= no/poor RC signal 1=sane RC signal
#'
#'
#'@export rcShed
#'@examples
#'#### Example to use rcShed for a typical uav flight
#' # setup GIS environ
#' envGIS<- initRGIS("~/proj/drone",'uniwald',"~/proj/drone/uniwald/mrbiko.tif")  
#' # call rcShed
#' rcRange<-rcShed(envGIS,launchP = c(8.692, 50.842316), flightAlt = 100, rcRange = 1000,dem="mrbiko.tif")
#'

rcShed <- function(launchP = NULL, 
                    launchAlt = NULL, 
                    flightAlt = 100, 
                    rcRange = 1000, 
                    dem = NULL) {
  
  link2GI::linkSAGA()
  # create launching pos
  coord <- data.frame(as.numeric(launchP[1]),as.numeric(launchP[2]),100)
  names(coord) <- c("lon","lat","alt")
  sp::coordinates(coord) <- ~lon+lat
  sp::proj4string(coord) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  rgdal::writeOGR(coord, ".", "view", driver = "ESRI Shapefile",overwrite_layer = TRUE)
  
  # convert dem
  gdalwarp("demll.tif", "dem.sdat", overwrite = TRUE, s_srs = 'EPSG:4326', of = 'SAGA')  
  viewshed <- system(paste(sagaCmd," ta_lighting 6",
                           " -ELEVATION 'dem.sdat'",
                           " -VISIBILITY 'vis.sgrd'",
                           " -POINTS 'view.shp'",
                           " -FIELD_HEIGHT 'alt'",
                           " -METHOD 0"),
                     intern = TRUE)
  if (grep("100%okay",x = viewshed))  cat("rc-range analysis okay")
  
  gdalwarp("vis.sdat", "viewmask.tif", overwrite = TRUE, s_srs = 'EPSG:4326', of = 'GTiff')  
  rcInsight <- raster::raster("viewmask.tif")
  rcRadius  <- raster::raster(rcInsight)
  values(rcRadius) = NA
  rcRadius <- distanceFromPoints(rcRadius,coord) 
  rcRadius[rcRadius > rcRange] <- 0
  rcRadius[rcRadius > 0] <- 1
  rcContactZone <- rcRadius*rcInsight
  cext <- viewExtent(coord@coords[1],coord@coords[2],0,rcRange)
  # crop it for speeding up
  rcContactZone <- raster::crop(rcContactZone,extent(cext))
  return(rcContactZone) 
}

viewExtent <- function(lon,lat,heading,rcRange){
  rcRange <- rcRange*1.5
  t1 <- calcNextPos(lon,lat,abs(heading),rcRange)
  t2 <- calcNextPos(lon,lat,abs(heading),-rcRange)
  
  
  yllc <- calcNextPos(t1[1],t1[2],-90,rcRange)[2]
  xllc <- calcNextPos(t1[1],t1[2],-90,rcRange)[1]
  ylrc <- calcNextPos(t1[1],t1[2],90,rcRange)[2]
  xlrc <- calcNextPos(t1[1],t1[2],90,rcRange)[1]
  
  yulc <- calcNextPos(t2[1],t2[2],-90,rcRange)[2]
  xulc <- calcNextPos(t2[1],t2[2],-90,rcRange)[1]
  yurc <- calcNextPos(t2[1],t2[2],90,rcRange)[2]
  xurc <- calcNextPos(t2[1],t2[2],90,rcRange)[1]
  
  ID <- paste0("CameraExtend_",rcRange,"_",lon,lat)
  rawPolygon <- sp::Polygon(cbind(c(xulc,xurc,xlrc,xllc,xulc),c(yulc,yurc,ylrc,yllc,yulc)))
  tileExtend <- sp::Polygons(list(rawPolygon), ID = ID)
  tileExtend <- sp::SpatialPolygons(list(tileExtend))
  df <- data.frame( ID = 1:length(rawPolygon), row.names = ID)
  frame <- sp::SpatialPolygonsDataFrame(tileExtend, df)
  sp::proj4string(frame) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  return(frame)
}