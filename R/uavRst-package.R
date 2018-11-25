#'  Unmanned Aerial Vehicle Remote Sensing Tools
#'
#' @name uavRst
#' @docType package
#' @title Unmanned Aerial Vehicle Remote Sensing Tools - some cool tools to manipulate and analyze UAV derived RGB ortho imagery and point clouds.
#' @description 
#' In general the  \code{uavRst} remote sensing toolbox tries to support the use of UAV derived imagery and pointclouds 
#' as a cheap and easy to use alternative/complement to LiDAR data. Howeverit is far from being mature. \cr
#' \code{uavRst} provides functionality to analyze poor quality RGB images as taken by low budget ready to fly uavs. This includes preconfigured 
#' machine learning based classification workflows, comprehensive texture analysis and segmentation algorithms as well as forest relevant calculations
#' of metrics and measures on the derived products.    
#' @note 
#' For most of the functions you need a bunch of third party software. The most comfortable way to fulfill
#' these requirements is to install 'QGIS', 'GRASS'- and 'SAGA-GIS' following the excellent \href{https://CRAN.R-project.org/package=RQGIS}{RQGIS}. 
#' For most of the LiDAR related operations the great R package \href{https://CRAN.R-project.org/package=lidR}{lidR} is used. \cr\cr
#' However for some of the basic point cloud related operations you will need to install the 'LAStool' software, that can be downloaded
#'  \href{http://lastools.org/download/LAStools.zip}{here} here and is provided by rapidlasso. 
#'  Please download it and unzip it as usual. For Windows systems it is by default expected that you put it  at \code{C:/LASTools}, running  Linux at \code{~/apps/LASTools}. 
#'  For running LAStools tools under Linux you first need to install wine. \cr\cr All of the mentioned software packages have to be correctly installed.
#'  Most of it tested under Windows and Linux and should run... The most easiest way to obtain a fairly good runtime enviroment is to setup Linux as a dual boot system or in a VB. 
#'  You will find some tutorials and examples at the uavRst Wiki. Please feel free to participate. \cr\cr
#' 
#' 
#' @author Hanna Meyer, Thomas Nauss,Florian Detsch, Lars Opgenoorth, Chris Reudenbach, Environmental Informatics Marburg \cr
#' \cr
#' \emph{Maintainer:} Chris Reudenbach \email{reudenbach@@uni-marburg.de}
#' @import raster
#' @import foreach
#' @useDynLib uavRst
#' @keywords package
NULL
#' @docType data
#' @name mrbiko
#' @title DEM data set of Marburg-Biedenkopf
#' @description DEM data set resampled to 20 m resolution
#' @format \code{"raster::raster"}
NULL

#' @docType data
#' @name pacman
#' @title example raster data set for demonstration usage
#' @description dump of the well know pac man game
#' @format \code{"raster::raster"}
NULL
#' @docType data
#' @name trp_seg
#' @title Optional tree position raster map
#' @description Example data set containing optional treepoint positions sampled in the Maburg University Forest (MOF). The resolution is 10 cm. ETRS89 UTM32
#' @format \code{"raster::raster"}
NULL
#' @docType data
#' @name chm_seg
#' @title Canopy height model raster map
#' @description Example data set containing the canopy height map of a small plot sampled in the Maburg University Forest (MOF). The resolution is 10 cm. ETRS89 UTM32
#' @format \code{"raster::raster"}
NULL
#' @docType data
#' @name rgb
#' @title RGB Orthoimagefrom an arbitray MOF plot
#' @description Example data set containing a RGB Orthoimage of a small plot sampled in the Maburg University Forest (MOF). The resolution is 10 cm. ETRS89 UTM32
#' @format \code{"raster::raster"}
NULL