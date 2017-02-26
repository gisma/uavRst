

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