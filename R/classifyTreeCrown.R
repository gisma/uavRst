if (!isGeneric('classifyTreeCrown')) {
  setGeneric('classifyTreeCrown', function(x, ...)
    standardGeneric('classifyTreeCrown'))
}

#'@name classifyTreeCrown
#'@title calcualte and post-classifies the morphological structure of raw tree crowns
#'
#'@description
#' calcualte and post-classifies the morphological structure of raw tree crowns
#'
#'@usage classifyTreeCrown(runDir,currentP,allP)
#'
#'@author Chris Reudenbach
#'
#'
#'@param crownFn filname of OGR comliant vector file
#'@param funNames morphological parameters to be calculated
#'@param minTreeAlt minimum height in meter that will be regarded as tree
#'@param crownMinArea minimum area of crowns that is accepted
#'@param crownMaxArea maximum area of crowns that is accepted
#'@param solidity minimum solidity of crowns that is accepted
#'@param WLRatio minimum WLRatio of crowns that is accepted


#'@return classifyTreeCrown basically returns SPDF  with the crown polygons and all calculated parameters
#'
#'
#'@export classifyTreeCrown
#'@examples
#'#### Example to use classifyTreeCrown for a common analysis of the
#'     estimated spreading distances of an specifified area
#'
#' #
#'   trees_crowns <- classifyTreeCrown(crownFn = paste0(pd_gi_run,"crownsHeight.shp"),segType = 1, 
#'                                       funNames = c("eccentricityboundingbox","solidity"),
#'                                       minTreeAlt = 5, 
#'                                       crownMinArea = 3, 
#'                                       crownMaxArea = 300, 
#'                                       solidity = 1, 
#'                                       WLRatio = 0.5)
#'
classifyTreeCrown <- function(crownFn,segType="2", 
                              funNames = c("eccentricityboundingbox","solidity"),
                              minTreeAlt = 5, 
                              crownMinArea = 3, 
                              crownMaxArea =150, 
                              solidity = 1, 
                              WLRatio = 0.5) {
  # read crown vector data set
  crownarea <- rgdal::readOGR(dirname(crownFn),tools::file_path_sans_ext(basename(crownFn)), verbose = FALSE)
  crownarea@proj4string <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  # calculate area
  crownarea@data$area <- rgeos::gArea(crownarea,byid = TRUE)
  # filter for min, tree height and min max crown area
  crownarea <-  crownarea[crownarea@data$chmMAX >= minTreeAlt ,]
  crownarea <- crownarea[crownarea@data$area > crownMinArea & 
                           crownarea@data$area < crownMaxArea,]
  # calculate more metrics
  crownarea <- uavRst::caMetrics(crownarea,funNames = funNames)
  #  filter for solidity and WL ratio
  crowns <- crownarea[as.numeric(crownarea@data$solidity) != solidity &
                        as.numeric(crownarea@data$eccentricityboundingbox) > WLRatio ,]
  # calculate centroids as synthetic trees and ass all knoledge from the crowns
  sT <- rgeos::gCentroid(crowns,byid = TRUE)
  crowns@data$xcoord <- sT@coords[,1]
  crowns@data$ycoord <- sT@coords[,2]
  centerTrees <- crowns@data
  sp::coordinates(centerTrees) <- ~xcoord+ycoord
  sp::proj4string(centerTrees) <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  
  # save centerTrees and crowns as shapefile
  rgdal::writeOGR(obj = centerTrees,
                  layer = paste0("cTr_",segType), 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  rgdal::writeOGR(obj = crowns,
                  layer = paste0("cro_",segType), 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  return(list(centerTrees,crowns))
}