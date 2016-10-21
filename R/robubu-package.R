#' the rolling burning bus - whatever, it runs fine for now 
#'
#' The package provides some hybrid R-GI functionality
#'
#' @name robubu-package
#' @docType package
#' @title the rolling burning bus - awesome gis tools fyi
#' @author Chris Reudenbach,
#' \cr
#' \emph{Maintainer:} Chris Reudenbach \email{reudenbach@@uni-marburg.de}
#'
#' @details The package name is a tribute to Randall Munroe's great xkcd webcomics and especially \href{http://xkcd.com/1695/}{Code Quality 2}
#' @import stringr foreach sp raster htmlwidgets htmltools Rcpp rgdal gdalUtils tools maptools mapview 
#' @importFrom geosphere bearing
#' @importFrom geosphere distGeo
#' @importFrom geosphere destPoint
#' @importFrom igraph clusters
#' @importFrom igraph graph
#' @importFrom igraph V
#' @import log4r
#' @import caret
#'  
#'
#' @useDynLib robubu
#' @keywords package
#' 
NULL
#'
#' @docType data
#' @name campsQ2
#' @title Antarctic Facilities
#' @description Antarctic facilities. Type of facility attribute is ""feature_ty"
#' @details The data show the Antarctic facilities (2012) as provided by \href{https://www.comnap.aq}{comnap} via the \href{www.quantarctica.org}{Quantarctica2} data compilation that is compiled by the \href{http://www.npolar.no/en}{Norwegian Polar Institute}
#' @format \code{sp::SpatialPointsDataFrame}
NULL
#'
#' @docType data
#' @name roadsGRL
#' @title roads data from Greenland as provided by geofabrik.de
#' @description Roads snapshot March(2016) from the OSM data extract as provided by geofabrik.de
#' @details The data show the roads (snapshot 03/2013) as downloaded by \href{http://download.geofabrik.de/north-america/greenland-latest.shp.zip}{geofabrik}. The geofabrik extracts are based on the OpenStreetMap \href{http://www.openstreetmap.org/}{OSM} data. For further information see also \href{http://download.geofabrik.de/north-america/greenland.html}{Geofabrik Downloads}
#' @format \code{sp::SpatialPointsDataFrame}
NULL
#'
#' @docType data
#' @name world
#' @title world boundary data set
#' @description world boundary data set
#' @details This dataset is from the mapdata package
#' \code{\link{mapdata}}.
#' @format \code{"SpatialPolygonDataFrame-class"}
#' @source
#' \url{https://cran.r-project.org/web/packages/mapdata}

NULL
#'
#' @docType data
#' @name mrbiko
#' @title DEM data set of Marburg-Biedenkopf
#' @description DEM data set resampled to 20 m resolution
#' @format \code{"raster::raster"}


