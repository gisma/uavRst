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
#'@param mintreeAltParam parameter that is used for filtering mintreealt default ist Median "chmQ50"
#'@param crownSTDW parameter that optionally filters for the STDV of the crown altitudes default is NULL
#'@param TAopt optional parameter that my be used for filtering default is NULL
#'@param opt threshold value for optional filter default is NULL



#'@return fa_basicTreeCrownFilterbasically returns SPDF  with the crown polygons and all calculated parameters
#'
#'
#'@export fa_basicTreeCrownFilter


fa_basicTreeCrownFilter<- function(crownFn,
                                   minTreeAlt = 10, 
                                   crownMinArea = 5, 
                                   crownMaxArea =100,
                                   mintreeAltParam = "chmQ50",
                                   crownSTDW = NULL,
                                   opt = NULL,
                                   TAopt = NULL) {
  # read crown vector data set
  if (class(crownFn)=="character")  
    crownarea <- raster::shapefile(crownFn)
  else 
    crownarea <- crownFn
  
  crownarea@proj4string <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  # calculate area
  crownarea[is.na(crownarea$chmQ10)]<- 0
  crownarea@data$area <- rgeos::gArea(crownarea,byid = TRUE)
  # filter for min, tree height and min max crown area
  crownarea <- crownarea[eval(parse(text=paste("crownarea@data$",mintreeAltParam,sep = ""))) >= minTreeAlt ,]
  crownarea <- crownarea[crownarea@data$area > crownMinArea,]
  crownarea <- crownarea[crownarea@data$area < crownMaxArea,]
  crownarea <- crownarea[crownarea$VALUE >= 0,]
  if (!is.null(crownSTDW)) crownarea <- crownarea[crownarea@data$chmSTDDEV > crownSTDW,]
  #  filter for arbitray threshold
  if (!is.null(TAopt)) crownarea <- crownarea[eval(parse(text=paste0("crownarea@data$",TAopt)))  > opt ,]
  crowns <- crownarea
  # calculate centroids as synthetic tree stems of the crowns
  sT <- rgeos::gCentroid(crowns,byid = TRUE)
  crowns@data$xcoord <- sT@coords[,1]
  crowns@data$ycoord <- sT@coords[,2]
  crowns@data$height <- crownarea@data$chmRANGE
  centerTrees <- crowns@data
  sp::coordinates(centerTrees) <- ~xcoord+ycoord
  sp::proj4string(centerTrees) <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  
  # save centerTrees and crowns as shapefile
  # export geojson
  #sf::st_write(sf::st_as_sf(centerTrees), "cTr.geojson",delete_dsn=TRUE,driver="GeoJSON")
  # export geojson
  #sf::st_write(sf::st_as_sf(crowns), "cro.geojson",delete_dsn=TRUE,driver="GeoJSON")
  
  return(list(centerTrees,crowns))
}