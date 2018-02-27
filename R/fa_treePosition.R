if (!isGeneric('treePos')) {
  setGeneric('treePos', function(x, ...)
    standardGeneric('treePos'))
}

#'@name treePos
#'@title Find potential tree positions using a canopy height model
#'
#'@description
#' Find potential tree positions using a canopy height model using a iterative watershed algorithm. return basically returns a  vector data sets with the tree crown geometries and a bunch of corresponding indices
#'
#'@author Chris Reudenbach
#'
#'@param chm  spatial raster object
#'@param minTreeAlt default is 5 
#'@param minTreeAltParam default is "chmQ20"
#'@param minCrownArea    default is 3 minimum area of crown
#'@param maxCrownArea    default is 225 maximum area of crown
#'@param output      default is 0,     # 0=s treePos value 1=segment id
#'@param join        default is 2,     # 0=no join, 1=treePos2saddle diff, 2=treePos2treePos diff
#'@param thresh      default is 0.05,  # threshold for join difference in m
#'@param split default  is TRUE switch if splitting of the polygons is called

#'
#'
#'@export treePos
#'@examples
#'\dontrun{
#' # Tree segmentation based on a CHM
#'  treePos(chm = rasterobj,  "nameofSAGAFile")
#'}
#'
treePos <- function(chm = NULL,
                                  minTreeAlt       = 10,
                                  minTreeAltParam  = "chmQ20",
                                  minCrownArea     = 3,
                                  maxCrownArea     = 150,
                                  output      = 1,     # 0= treePos value 1=segment id
                                  join        = 1,     # 0=no join, 1=treePos2saddle diff, 2=treePos2treePos diff
                                  thresh      = 0.10,  # threshold for join difference in m
                                  split = TRUE,
                                  giLinks = NULL
                                  
)  {
  cat("\n:: start crown identification...\n")
  options(warn=-1)
  
  if (is.null(giLinks)){
    giLinks <- linkBuilder()
  }
  
  gdal <- giLinks$gdal
  saga <- giLinks$saga
  sagaCmd<-saga$sagaCmd
  raster::writeRaster(chm,paste0("chm.sdat"),overwrite = TRUE,NAflag = 0)
  raster::writeRaster(chm,paste0("chm.tif"),overwrite = TRUE,NAflag = 0)
  #r2saga(chm,"chm")

    cat(":: run pre-segmentation...\n")
    # first segment run is a simple watershed segmentation just for deriving more reliable treePoss 
    # TODO improve different advanceds treePos finding algorithms
    ret <- system(paste0(sagaCmd, " imagery_segmentation 0 ",
                         " -GRID "     ,path_run,"chm.sgrd",
                         " -SEGMENTS " ,path_run,"dummyCrownSegments.sgrd",
                         " -SEEDS "    ,path_run,"treePos.shp",
                         " -OUTPUT "   ,output, 
                         " -DOWN 1"    , 
                         " -JOIN "     ,join,
                         " -THRESHOLD ",thresh, 
                         " -EDGE 0")
                  ,intern = TRUE)
    
    # convert filtered crown clumps to shape format for descriptive running statitics 
    ret <- system(paste0(sagaCmd, " shapes_grid 6 ",
                         " -GRID "    ,path_run,"dummyCrownSegments.sgrd",
                         " -POLYGONS ",path_run,"dummyCrownSegment.shp",
                         " -CLASS_ALL 1",
                         " -CLASS_ID 1.000000",
                         " -SPLIT 1"),
                  intern = TRUE)
    cat(":: filter results...\n")
    
    # 
    cat(":: find max height position...\n")
    dummycrownsStat <- uavRst::xpolystat(c("chm"), spdf ="dummyCrownSegment.shp")

    trees_crowns <- fa_basicTreeCrownFilter(crownFn = dummycrownsStat,
                                            minTreeAlt = minTreeAlt,
                                            minCrownArea = minCrownArea,
                                            maxCrownArea = maxCrownArea,
                                            minTreeAltParam = minTreeAltParam 
    )
    rgdal::writeOGR(obj    = trees_crowns[[2]],
                    layer  = "dummyCrownSegment", 
                    driver = "ESRI Shapefile", 
                    dsn    = path_run, 
                    overwrite_layer = TRUE)
    
    cat(":: find max height position...\n")
    ts <-  extractMaxPosPoly(paste0(path_run,"chm.tif"),"dummyCrownSegment",poly_split = split)
    # create raw zero mask
    treePos <- ts[[1]] * chm
    raster::writeRaster(treePos,"treePos0.sdat",overwrite = TRUE,NAflag = 0)
    #r2saga(treePos,"treePos0")
    # extract stats
    
    # reclass extracted treePoss to minTreeAlt
    ret <- system(paste0(sagaCmd, "  grid_tools 15 ",
                         " -INPUT "  ,path_run,"treePos0.sgrd",
                         " -RESULT " ,path_run,"treePos.sgrd",
                         " -METHOD 0 ",
                         " -OLD "    ,minTreeAlt ,
                         " -NEW 0.00000",
                         " -SOPERATOR 1",
                         " -NODATAOPT 0",
                         " -OTHEROPT 0",
                         " -RESULT_NODATA_CHOICE 1 ",
                         " -RESULT_NODATA_VALUE 0.000000")
                  ,intern = TRUE)
    
    # TODO SF
    # trees <- sf::st_read(paste0(path_run,"treePos.shp"))
    localmaxima<-raster::raster(paste0(path_run,"treePos.sdat"))
    localmaxima@crs <- chm@crs
  return(localmaxima)
}


#' individual tree detection whitin the LiDAR-derived Canopy Height Model (CHM) 'rLiDAR'
#' @description Detects and computes the location and height of individual trees within 
#' the LiDAR-derived Canopy Height Model (CHM). The algorithm implemented in this function 
#' is local maximum with a fixed window size. Carlos A. Silva et all.: R package \href{https://CRAN.R-project.org/package=rLiDAR}{rLiDAR}\cr
#' @param chm Canopy height model in \link[raster]{raster} or \link[raster]{SpatialGridDataFrame} file format. Should be the same that was used to create
#' the input for \code{treePos}.
#' @param movingWin Size (in pixels) of the moving window to detect local maxima.
#' @param minTreeAlt Height threshold (m) below a pixel cannot be a local maximum. Local maxima values are used to define tree tops.
#' @import rLiDAR
#' @export treePosRL
#' @examples 
#' \dontrun{
#'  treePosITC <- treePosRL(chm,fws,minht)
#' }


treePosRL <- function(chm =NULL,
                      movingWin = 7,
                      minTreeAlt = 2) {
  
  # if (class(treePos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
  #   chm <- raster::raster(chm)
  # } 
  localmaxima <- raster::rasterFromXYZ(rLiDAR::FindTreesCHM(chm, fws = movingWin, minht=minTreeAlt))
  localmaxima@crs <- chm@crs
  return(localmaxima)
}

#' Tree top detection based on local maxima filters 'lidR'
#' @description Tree top detection based on local maxima filters. There are two types of filter. The first,
#' called for gridded objects, works on images with a matrix-based algorithm and the second one, called for 
#' point clouds, works at the point cloud level without any rasterization. Jean-Romain Roussel and David Auty:
#' R package \href{https://CRAN.R-project.org/package=lidR}{lidR}\cr
#' @param chm Canopy height model in \link[raster]{raster}, \code{lasmetrics}, \code{matrix} or  object of \code{class LAS}.
#' Should be the same that was used to create
#' the input for \code{treePos}.
#' @param movingWin Size (in pixels) of the moving window to detect local maxima.
#' @param minTreeAlt Height threshold (m) below a pixel cannot be a local maximum. Local maxima values are used to define tree tops.
#' @import rLiDAR
#' @export treePoslidR
#' @examples 
#' \dontrun{
#'  crownslidR <-treePoslidR(chm,fws,minht)
#' }


treePoslidR <- function(chm =NULL,
                      movingWin = 7,
                      minTreeAlt = 2) {
  
  # if (class(treePos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
  #   chm <- raster::raster(chm)
  # } 

  localmaxima <- lidR::tree_detection(x = chm, ws=movingWin, hmin = minTreeAlt)
  localmaxima@crs <- chm@crs
  return(localmaxima)
}


#' Tree top finder 'ForestTools'
#' @description Implements the variable window filter algorithm (Popescu & Wynne, 2004) 
#' for detecting treetops from a canopy height model. Andrew Plowright:
#' R package \href{https://CRAN.R-project.org/package=ForestTools}{ForestTools}\cr
#' @param chm Canopy height model in \link[raster]{raster}, \code{lasmetrics}, \code{matrix} or  object of \code{class LAS}.
#' Should be the same that was used to create
#' the input for \code{treePos}.
#' @param winFun	function. The function that determines the size of the window at any given 
#' location on the canopy. It should take the value of a given CHM pixel as its only argument, 
#' and return the desired radius of the circular search window when centered on that pixel.
#' @param minTreeAlt Height threshold (m) below a pixel cannot be a local maximum. Local maxima values are used to define tree tops.
#' @param maxCrownArea numeric. A single value of the maximum individual tree crown radius expected. 
#' height of \code{treePos}.
#' @import rLiDAR
#' @export treePosFT
#' @examples 
#' \dontrun{
#'  crownsFT <-treePosFT(chm = chmR,
#'                       minTreeAlt = 2, 
#'                       maxCrownArea = maxCrownArea)
#' }


treePosFT <- function(chm =NULL,
                        winFun = function(x){0.5 * ((x^2) * 0.0090 + 2.51)},
                        minTreeAlt = 2, 
                        maxCrownArea = maxCrownArea,
                        verbose = TRUE) {
  
  # if (class(treePos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
  #   chm <- raster::raster(chm)
  # } 
  maxcrown <- sqrt(maxCrownArea/ pi) * 4
  
  localmaxima <- ForestTools::TreeTopFinder(CHM = chm, 
                                            winFun = winFun, 
                                            minHeight = minTreeAlt,
                                            maxWinDiameter = ceiling(maxcrown),
                                            verbose = verbose)
  # create raw zero mask
  treePos <- 0 * chm
  localmaxima<-raster::rasterize(localmaxima,treePos)
  
  return(localmaxima)
}
