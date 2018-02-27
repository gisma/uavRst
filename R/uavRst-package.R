#' Unmanned Aerial Vehicle Remote Sensing Tools
#'
#' The package provides some  R-GI functionality for dealing with Unmanned Aerial Vehicle Remote Sensing 
#'
#' @name uavRst
#' @docType package
#' @title Unmanned Aerial Vehicle Remote Sensing Tools - awesome gis tools to fly
#' \cr
#' \emph{Maintainer:} Chris Reudenbach \email{reudenbach@@uni-marburg.de}
#'
#' @import stringr zoo foreach sp raster htmlwidgets htmltools Rcpp rgeos rgdal gdalUtils tools maptools mapview doParallel velox CAST 
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
#' @import  lattice
#' @import latticeExtra
#' @import crayon sf
#' @importFrom utils modifyList
#' @importFrom ForestTools SegmentCrowns
#' @importFrom ForestTools TreeTopFinder
#' @importFrom itcSegment itcIMG
#' @importFrom rLiDAR FindTreesCHM
#' @importFrom rLiDAR ForestCAS
#' @import pROC
#' @import lidR
#' @import methods
#' @import RSAGA
#' @import osmar
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
