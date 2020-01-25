if (!isGeneric('treepos')) {
  setGeneric('treepos', function(x, ...)
    standardGeneric('treepos'))
}

#'@name treepos_GWS
#'@title Find potential tree positions using a canopy height model
#'
#'@description
#' Find potential tree positions using a canopy height model by using an iterative watershed algorithm. Basically returns a  vector data sets with the tree crown geometries and a bunch of corresponding indices.
#'
#'@author Chris Reudenbach
#'
#'@param chm  raster* canopy height model
#'@param minTreeAlt numeric. minimum height of trees to be integrated in the analysis
#'@param minTreeAltParam character. code for the percentile that is used as tree height treshold. It is build using the key letters \code{chmQ} and adding the percentile i.e. "10". Default is \code{chmQ20}
#'@param minCrownArea    numeric. minimum area in square meter (if you use projected data) of the projected tree crowns
#'@param maxCrownArea    numeric. maximum area in square meter (if you use projected data) of the projected tree crowns
#'@param join        numeric. Join Segments based on Threshold Value, 0=no join, 1=treepos_GWS2saddle diff, 2=treepos_GWS2treepos diff. see also \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_0.html}{SAGA GIS Help}
#'@param thresh      numeric. Specify a threshold value as minimum difference between neighboured segments in meter. see also \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_0.html}{SAGA GIS Help}
#'@param split split polygons default is TRUE
#'@param cores number of cores to be used
#'@param giLinks        list. of GI tools cli paths
#'@return raster* object
#'
#'return raster* object
#'@export treepos_GWS
#'@examples
#'\dontrun{
#'
#' # required packages
#' require(uavRst)
#' require(link2GI)
#'
#' # create and check the links to the GI software
#' giLinks<-uavRst::linkGI()
#' if (giLinks$saga$exist & giLinks$otb$exist & giLinks$grass$exist) {
#'
#' # project folder
#' projRootDir<-tempdir()
#'
#' # create subfolders please mind that the pathes are exported as global variables
#' paths<-link2GI::initProj(projRootDir = projRootDir,
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")
#'
#'  data(chm_seg)
#'
#' # calculate treepos using uavRst generic approach
#'  tPos <- uavRst::treepos_GWS(chm = chm_seg[[1]],
#'                          minTreeAlt = 2,
#'                          maxCrownArea = 150,
#'                          join = 1,
#'                          thresh = 0.35,
#'                          split=TRUE,
#'                          cores=1,
#'                          giLinks = giLinks )
#'}
#'##+}
#'
treepos_GWS <- function(chm = NULL,
                                  minTreeAlt       = 10,
                                  minTreeAltParam  = "chmQ20",
                                  minCrownArea     = 3,
                                  maxCrownArea     = 150,
                                  join        = 1,     # 0=no join, 1=treepos2saddle diff, 2=treepos2treepos diff
                                  thresh      = 0.10,  # threshold for join difference in m
                                  split = TRUE,
                                  cores = parallel::detectCores(),
                                  giLinks = NULL

)  {
  if (!exists("path_run")) path_run = tempdir()
  message("\n:: start crown identification...\n")
  options(warn=-1)

  if (is.null(giLinks)){
    giLinks <- linkGI()
  }

  gdal <- giLinks$gdal
  saga <- giLinks$saga
  sagaCmd<-saga$sagaCmd
  raster::writeRaster(chm,file.path(R.utils::getAbsolutePath(path_run),"chm.sdat"),bylayer=TRUE,overwrite = TRUE,NAflag = 0)
  raster::writeRaster(chm,file.path(R.utils::getAbsolutePath(path_run),"chm.tif"),overwrite = TRUE,NAflag = 0)
  #r2saga(chm,"chm")

    message(":: run pre-segmentation...\n")
    # first segment run is a simple watershed segmentation just for deriving more reliable treepos´
    # TODO improve different advanceds treepos finding algorithms
    ret <- system(paste0(sagaCmd, " imagery_segmentation 0 ",
                         " -GRID "     ,file.path(R.utils::getAbsolutePath(path_run),"chm.sgrd"),
                         " -SEGMENTS " ,file.path(R.utils::getAbsolutePath(path_run),"dummyCrownSegments.sgrd"),
                         " -SEEDS "    ,file.path(R.utils::getAbsolutePath(path_run),"treepos.shp"),
                         " -OUTPUT 0",
                         " -DOWN 1"    ,
                         " -JOIN "     ,join,
                         " -THRESHOLD ",thresh,
                         " -EDGE 0")
                  ,intern = TRUE)

    # convert filtered crown clumps to shape format for descriptive running statitics
    ret <- system(paste0(sagaCmd, " shapes_grid 6 ",
                         " -GRID "    ,file.path(R.utils::getAbsolutePath(path_run),"dummyCrownSegments.sgrd"),
                         " -POLYGONS ",file.path(R.utils::getAbsolutePath(path_run),"dummyCrownSegment.shp"),
                         " -CLASS_ALL 1",
                         " -CLASS_ID 1.000000",
                         " -SPLIT 1"),
                  intern = TRUE)
    message(":: filter results...\n")

    #
    message(":: find max height position...\n")
    if (cores<2) para<-0
    else para = 1
    dummycrownsStat <- uavRst::poly_stat(c("chm"), 
                                         spdf =file.path(R.utils::getAbsolutePath(path_run),"dummyCrownSegment.shp"),
                                         parallel = para)

    trees_crowns <- crown_filter(crownFn = dummycrownsStat,
                                            minTreeAlt = minTreeAlt,
                                            minCrownArea = minCrownArea,
                                            maxCrownArea = maxCrownArea,
                                            minTreeAltParam = minTreeAltParam
    )
    rgdal::writeOGR(obj    = trees_crowns[[2]],
                    layer  = "dummyCrownSegment",
                    driver = "ESRI Shapefile",
                    dsn    = file.path(R.utils::getAbsolutePath(path_run)),
                    overwrite_layer = TRUE)

    message(":: find max height position...\n")
    ts <-  poly_maxpos(file.path(R.utils::getAbsolutePath(path_run),"chm.tif"),
                       file.path(R.utils::getAbsolutePath(path_run),"dummyCrownSegment"),
                       polySplit = split,
                       cores = cores)
    # create raw zero mask ts[[1]] = seeds ts[[2]] = maxpos
    treepos <- ts[[1]] * chm
    raster::writeRaster(treepos,
                        file.path(R.utils::getAbsolutePath(path_run),"treepos0.sdat"),
                        overwrite = TRUE,
                        NAflag = 0)

    # reclass extracted treeposs to minTreeAlt
    ret <- system(paste0(sagaCmd, "  grid_tools 15 ",
                         " -INPUT "  ,file.path(R.utils::getAbsolutePath(path_run),"treepos0.sgrd"),
                         " -RESULT " ,file.path(R.utils::getAbsolutePath(path_run),"treepos.sgrd"),
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
    # trees <- sf::st_read(paste0(path_run,"treepos.shp"))
    localmaxima<-raster::raster(file.path(R.utils::getAbsolutePath(path_run),"treepos.sdat"))
    localmaxima@crs <- chm@crs
    # workaround for strange effects with SAGA
    # even if all params are identical it is dealing with different grid systems
    localmaxima<-raster::resample(localmaxima, chm , method = 'bilinear')
    localmaxima[localmaxima<=0]<-0
    # remove temporary files
    flist<-list()
    flist<-append(flist, Sys.glob(file.path(R.utils::getAbsolutePath(path_run),"treepos0.*")))
    flist<-append(flist, Sys.glob(file.path(R.utils::getAbsolutePath(path_run),"dummyCrownSegment*")))
    flist<-append(flist, Sys.glob(file.path(R.utils::getAbsolutePath(path_run),"treepos.*")))
    flist<-append(flist, Sys.glob(file.path(R.utils::getAbsolutePath(path_run),"chmStat.*")))
    flist<-append(flist, Sys.glob(file.path(R.utils::getAbsolutePath(path_run),"polyStat.*")))
    res<-file.remove(unlist(flist))
  return(localmaxima)
}


#' 'rLiDAR' based tree detection of a LiDAR-derived Canopy Height Model (CHM)
#' @description Detects and computes the location and height of individual trees within
#' the LiDAR-derived Canopy Height Model (CHM). The algorithm implemented in this function
#' is local maximum with a fixed window size. Carlos A. Silva et all.: R package \href{https://CRAN.R-project.org/package=rLiDAR}{rLiDAR}\cr
#' @param chm Canopy height model in \code{raster} or \code{SpatialGridDataFrame} file format. Should be the same that was used to create
#' the input for \code{treepos}.
#' @param movingWin Size (in pixels) of the moving window to detect local maxima.
#' @param minTreeAlt Height threshold (m) below a pixel cannot be a local maximum. Local maxima values are used to define tree tops.
#' @export treepos_RL
#' @return  raster* object
#' @examples
#' \dontrun{
#' ## required packages
#' require(uavRst)
#'
#' ## load data
#' data(chm_seg)
#'
#' ## find trees
#' tPosRL <- treepos_RL(chm = chm_seg[[1]],
#'                     movingWin = 3,
#'                     minTreeAlt = 10)
#' ## visualisation
#' raster::plot(tPosRL)
#' }



treepos_RL <- function(chm =NULL,
                      movingWin = 7,
                      minTreeAlt = 2) {

  # if (class(treepos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
  #   chm <- raster::raster(chm)
  # }
  localmaxima <- raster::rasterFromXYZ(rLiDAR::FindTreesCHM(chm, fws = movingWin, minht=minTreeAlt))
  localmaxima@crs <- chm@crs
  return(localmaxima)
}

#' @title tree top detection based on local maxima filters as provided by 'lidR'
#' @description Tree top detection based on local maxima filters. There are two types of filter. The first,
#' called for gridded objects, works on images with a matrix-based algorithm. And the second one, called for
#' point clouds, works at the point cloud level without any rasterization. Jean-Romain Roussel and David Auty:
#' R package \href{https://CRAN.R-project.org/package=lidR}{lidR}\cr
#' @param chm Canopy height model in \code{raster}, \code{lasmetrics}, \code{matrix} or  object of \code{class LAS}.
#' Should be the same that was used to create
#' the input for \code{treepos}.
#' @param movingWin Size (in pixels) of the moving window to detect local maxima.
#' @param minTreeAlt Height threshold (m) below a pixel cannot be a local maximum. Local maxima values are used to define tree tops.
#' @return  raster* object
#' @export treepos_lidR
#' @examples
#' \dontrun{


#'require(uavRst)
#'## required packages
#'require(uavRst)
#'
#'  data(chm_seg)
#'
#'## find trees
#'tPoslidR <- treepos_lidR(chm = chm_seg[[1]],
#'                     movingWin = 3,
#'                     minTreeAlt = 15)
#'## visualisation
#' # mapview::mapview(tPoslidR)
#' raster::plot(tPoslidR)
#' }




treepos_lidR <- function(chm =NULL,
                      movingWin = 7,
                      minTreeAlt = 2) {

  # if (class(treepos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
  #   chm <- raster::raster(chm)
  # }

  localmaxima <- lidR::tree_detection(x = chm, ws=movingWin, hmin = minTreeAlt)
  localmaxima@crs <- chm@crs
  return(localmaxima)
}


#' 'ForestTools' tree top detection
#' @description Implements the variable window filter algorithm (Popescu & Wynne, 2004)
#' for detecting treetops from a canopy height model. Andrew Plowright:
#' R package \href{https://CRAN.R-project.org/package=ForestTools}{ForestTools}\cr
#' @param chm Canopy height model in \code{raster}, \code{lasmetrics}, \code{matrix} or  object of \code{class LAS}.
#' Should be the same that was used to create
#' the input for \code{treepos}.
#' @param winFun	function. The function that determines the size of the window at any given
#' location on the canopy. It should take the value of a given CHM pixel as its only argument,
#' and return the desired radius of the circular search window when centered on that pixel.
#' @param minTreeAlt Height threshold (m) below a pixel cannot be a local maximum. Local maxima values are used to define tree tops.
#' @param maxCrownArea numeric. A single value of the maximum individual tree crown radius expected.
#' @param verbose quiet (1)
#' height of \code{treepos}.
#' @return  raster* object
#' @export treepos_FT
#' @examples

#' ##- required packages
#'  require(uavRst)
#'
#'  data(chm_seg)
#'  
#' ##- call ForestTools treepos function
#'  tposFT <- treepos_FT(chm = chm_seg[[1]],
#'             minTreeAlt = 10,
#'             maxCrownArea = 150)
#'             
#' ##- visualize it
#' # mapview::mapview(tposFT)
#' raster::plot(tposFT)




treepos_FT <- function(chm =NULL,
                        winFun = function(x){0.5 * ((x^2) * 0.0090 + 2.51)},
                        minTreeAlt = 2,
                        maxCrownArea = maxCrownArea,
                        verbose = TRUE) {

  # if (class(treepos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
  #   chm <- raster::raster(chm)
  # }
  maxcrown <- sqrt(maxCrownArea/ pi) * 4 * 1/raster::res(chm)[[1]]

  localmaxima <- ForestTools::vwf(CHM = chm,
                                            winFun = winFun,
                                            minHeight = minTreeAlt,
                                            maxWinDiameter = ceiling(maxcrown),
                                            verbose = verbose)
  # create raw zero mask
  treepos <- 0 * chm
  localmaxima<-raster::rasterize(localmaxima,treepos)

  return(localmaxima)
}
