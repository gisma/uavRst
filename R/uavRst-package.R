#'  Unmanned Aerial Vehicle Remote Sensing Tools
#'
#' @description The package provides some R-GI functionality for dealing with data as retrieved by Unmanned Aerial Vehicle VIS/NIR Remote Sensing 
#'
#' @name uavRst
#' @docType package
#' @title Unmanned Aerial Vehicle Remote Sensing Tools - awesome tools to manipulate and analyze RGB imagery and point clouds
#' @author Hanna Meyer, Lars Opgenoorth, Chris Reudenbach, Environmental Informatics Marburg \cr
#' \cr
#' \emph{Maintainer:} Chris Reudenbach \email{reudenbach@@uni-marburg.de}
#'
#' @import stringr zoo foreach sp raster htmlwidgets htmltools Rcpp rgeos rgdal gdalUtils tools maptools doParallel velox CAST 
#' @importFrom geosphere bearing
#' @importFrom geosphere distGeo
#' @importFrom geosphere destPoint
#' @importFrom igraph clusters
#' @importFrom igraph graph
#' @importFrom igraph V
#' @importFrom data.table fread
#' @importFrom spatial.tools create_blank_raster 
#' @import log4r
#' @import caret
#' @importFrom utils write.table
#' @importFrom grDevices colorRampPalette
#' @import crayon sf
#' @importFrom utils modifyList
#' @importFrom ForestTools SegmentCrowns
#' @importFrom ForestTools TreeTopFinder
#' @importFrom itcSegment itcIMG
#' @importFrom rLiDAR FindTreesCHM
#' @importFrom rLiDAR ForestCAS
#' @import pROC
#' @importFrom lidR tree_detection
#' @importFrom lidR writeLAS
#' @importFrom lidR readLAS
#' @importFrom lidR lasclipRectangle
#' @import methods
#' @import RSAGA

#' @import reshape2
#' @import rgrass7
#'
#' @useDynLib uavRst
#' @keywords package
NULL
#' @docType data
#' @name mrbiko
#' @title DEM data set of Marburg-Biedenkopf
#' @description DEM data set resampled to 20 m resolution
#' @format \code{"raster::raster"}
NULL
