if (!isGeneric('fa_basicTreeCrownFilter')) {
  setGeneric('fa_basicTreeCrownFilter', function(x, ...)
    standardGeneric('fa_basicTreeCrownFilter'))
}

#'@name fa_basicTreeCrownFilter
#'@title applies minnimum height and min/max area thresholds on the raw tree crown geometries
#'
#'@description
#' applies minnimum height and min/max area thresholds on the raw tree crown geometries crowns
#'

#'@author Chris Reudenbach
#'
#'
#'@param crownFn filname of OGR comliant vector file
#'@param minTreeAlt minimum height in meter that will be regarded as tree
#'@param crownMinArea minimum area of crowns that is accepted
#'@param crownMaxArea maximum area of crowns that is accepted



#'@return fa_basicTreeCrownFilterbasically returns SPDF  with the crown polygons and all calculated parameters
#'
#'
#'@export fa_basicTreeCrownFilter


fa_basicTreeCrownFilter<- function(crownFn,
                              minTreeAlt = 5, 
                              crownMinArea = 3, 
                              crownMaxArea =150) {
  # read crown vector data set
  crownarea <- rgdal::readOGR(dirname(crownFn),tools::file_path_sans_ext(basename(crownFn)), verbose = FALSE)
  
  crownarea@proj4string <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  # calculate area
  crownarea[is.na(crownarea$chmQ10)]<- 0
  
  crownarea@data$area <- rgeos::gArea(crownarea,byid = TRUE)
  # filter for min, tree height and min max crown area
  crownarea <-  crownarea[crownarea@data$chmQ10 >= minTreeAlt ,]
  crownarea <- crownarea[crownarea@data$area > crownMinArea,]
  crownarea <- crownarea[crownarea@data$area < crownMaxArea,]
  crownarea <- crownarea[crownarea$VALUE >= 0,]
  #  filter for solidity and WL ratio
   crowns <- crownarea
  # calculate centroids as synthetic tree stems of the crowns
  sT <- rgeos::gCentroid(crowns,byid = TRUE)
  crowns@data$xcoord <- sT@coords[,1]
  crowns@data$ycoord <- sT@coords[,2]
  centerTrees <- crowns@data
  sp::coordinates(centerTrees) <- ~xcoord+ycoord
  sp::proj4string(centerTrees) <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  
  # save centerTrees and crowns as shapefile
  rgdal::writeOGR(obj = centerTrees,
                  layer = "cTr", 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  rgdal::writeOGR(obj = crowns,
                  layer = "cro", 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  return(list(centerTrees,crowns))
}