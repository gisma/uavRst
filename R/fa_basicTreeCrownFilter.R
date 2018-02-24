if (!isGeneric('fa_basicTreeCrownFilter')) {
  setGeneric('fa_basicTreeCrownFilter', function(x, ...)
    standardGeneric('fa_basicTreeCrownFilter'))
}

#'@name fa_basicTreeCrownFilter
#'@title basic filtering of crown polygons using altitude, area and other optional thresholds 
#'
#'@description
#' applies basic filtering of crown polygons using altitude, area and other optional thresholds 
#'

#'@author Chris Reudenbach
#'
#'
#'@param crownFn filname of OGR comliant vector file
#'@param minTreeAlt minimum height in meter that will be regarded as tree
#'@param minCrownArea minimum area of crowns that is accepted
#'@param maxCrownArea maximum area of crowns that is accepted
#'@param mintreeAltParam parameter that is used for filtering mintreealt default ist Median "chmQ50"
#'@param crownSTDW parameter that optionally filters for the STDV of the crown altitudes default is NULL
#'@param TAopt optional parameter that my be used for filtering default is NULL
#'@param opt threshold value for optional filter default is NULL
#'@param proj4string proj4 string 




#'@return fa_basicTreeCrownFilterbasically returns SPDF  with the crown polygons and all calculated parameters
#'
#'
#'@export fa_basicTreeCrownFilter


fa_basicTreeCrownFilter<- function(crownFn,
                                   minTreeAlt = 10, 
                                   minCrownArea = 5, 
                                   maxCrownArea =100,
                                   mintreeAltParam = "chmQ50",
                                   crownSTDW = NULL,
                                   opt = NULL,
                                   TAopt = NULL,
                                   proj4string="+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") {
  # read crown vector data set
  if (class(crownFn)=="character")  
    crownarea <- rgdal::readOGR(path_run,"crowns", verbose = FALSE)
  else 
    crownarea <- crownFn
  
  crownarea@proj4string <- sp::CRS(proj4string)
  # calculate area
  crownarea[is.na(crownarea$chmQ10)]<- 0
  crownarea@data$area <- rgeos::gArea(crownarea,byid = TRUE)
  # filter for min, tree height and min max crown area
  crownarea <- crownarea[eval(parse(text=paste("crownarea@data$",mintreeAltParam,sep = ""))) >= minTreeAlt ,]
  crownarea <- crownarea[crownarea@data$area > minCrownArea,]
  crownarea <- crownarea[crownarea@data$area < maxCrownArea,]
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