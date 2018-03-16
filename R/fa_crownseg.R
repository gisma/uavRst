
#'@title seeded region growing tree crown segmentation based on 'SAGA GIS'
#'
#'@description
#' Tree segmentation based on a CHM, basically returns a vector data set with the tree crown geometries and a bunch of corresponding indices. After the segementation itself, the results are hole filled and optionally, it can be filtered by a majority filter.
#'
#'@author Chris Reudenbach
#'
#'@param treepos  raster* object
#'@param minTreeAlt  numeric. The minimum height value for a \code{chm} pixel is to be considered as part of a crown segment.
#' All \code{chm} pixels beneath this value will be masked out. Note that this value should be lower than the minimum
#' height of \code{treepos}.
#'@param minTreeAltParam character. code for the percentile that is used as tree height treshold. It is build using the key letters \code{chmQ} and adding the percentile i.e. "10". Default is \code{chmQ20}
#'@param chm raster*. Canopy height model in \code{raster} format. Should be the same that was used to create the input for \code{treepos}.
#'@param leafsize       integer. bin size of grey value sampling range from 1 to 256 see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param normalize      integer. logical switch if data will be normalized (1) see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param neighbour      integer. von Neumanns' neighborhood (0) or Moore's (1) see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param method         integer. growing algorithm for feature space and position (0) or feature space only (1), see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param thVarSpatial   numeric. Variance in Feature Space  see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param thVarFeature   numeric. Variance in Position Space  see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param thSimilarity   mumeric. Similarity Threshold see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param segmentationBands    character. a list of raster data that is used for the segmentation. The canopy height model \code{c("chm")} is mandantory. see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param giLinks        list. of GI tools cli paths
#'@param majorityRadius numeric. kernel size for the majority filter out spurious pixel
#'@export
#'@examples
#'\dontrun{
#'
#' # required packages
#'  require(uavRst)
#'  require(curl)
#'  require(link2GI)
#' 
#' # project folder
#'  projRootDir<-tempdir()
#' 
#' # create subfolders please mind that the pathes are exported as global variables
#'  paths<-link2GI::initProj(projRootDir = projRootDir,
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")
#' # get the data
#'  url1 <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/treepos_3-3.tif"
#'  url2 <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/chm_3-3.tif"
#'  res1 <- curl::curl_download(url1, paste0(path_run,"treepos_3-3.tif"))
#'  res2 <- curl::curl_download(url2, paste0(path_run,"chm_3-3.tif"))
#' 
#' # make the folders and linkages
#'  giLinks<-uavRst::get_gi()
#' 
#' # read chm data
#'  chmR<- raster::raster(paste0(path_run,"chm_3-3.tif"))
#'  tPos<- raster::raster(paste0(path_run,"treepos_3-3.tif"))
#' 
#' # tree segmentation
#'  crowns <- chmseg_uav( treepos = tPos, 
#'                        chm = chmR,
#'                        minTreeAlt = 3,
#'                        neighbour = 0,
#'                        thVarFeature = 1.,
#'                        thVarSpatial = 1.,
#'                        thSimilarity = 0.00001,
#'                        giLinks = giLinks )
#'                        
#'}

chmseg_uav <- function(treepos = NULL,
                            chm = NULL,
                            minTreeAlt         =2,
                            minTreeAltParam = "chmQ20",
                            leafsize       = 256,
                            normalize      = 0,
                            neighbour      = 1,
                            method         = 0,
                            thVarFeature   = 1.,
                            thVarSpatial   = 1.,
                            thSimilarity   = 0.002,
                            segmentationBands    = c("chm"),
                       majorityRadius    = 3.000,
                            giLinks = NULL) {
  proj<- raster::crs(treepos)
  if (class(treepos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    raster::writeRaster(treepos,file.path(path_run,"treepos.sdat"),overwrite = TRUE,NAflag = 0)
  }
  if (class(chm) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    chm[chm<minTreeAlt] = -1
    raster::writeRaster(chm,file.path(path_run,"chm.sdat"),overwrite = TRUE,NAflag = 0)
  }


  cat("::: run main segmentation...\n")
  # create correct param lists
  #segmentationBands<-c("HI","GLI")
  if (is.null(giLinks)){
    giLinks <- get_gi()
  }

  gdal <- giLinks$gdal
  saga <- giLinks$saga
  sagaCmd<-saga$sagaCmd
  invisible(env<-RSAGA::rsaga.env(path =saga$sagaPath,modules = saga$sagaModPath))

  param_list <- paste0(path_run,segmentationBands,".sgrd;",collapse = "")

  # Start final segmentation algorithm as provided by SAGA's seeded Region Growing segmentation (imagery_segmentation 3)
  # TODO sensitivity analysis of the parameters


  RSAGA::rsaga.geoprocessor(lib = "imagery_segmentation", module = 3,
                            param = list(SEEDS = paste(path_run,"treepos.sgrd", sep = ""),
                                         FEATURES = param_list,
                                         SEGMENTS = paste(path_run,"crowns.sgrd", sep = ""),
                                         LEAFSIZE = leafsize,
                                         NORMALIZE = normalize,
                                         NEIGHBOUR = neighbour,
                                         METHOD   =  method,
                                         SIG_1    =  thVarFeature,
                                         SIG_2    =  thVarSpatial,
                                         THRESHOLD = thSimilarity),
                            intern = TRUE,
                            invisible = TRUE)

  # fill the holes inside the crowns (simple approach)
  # TODO better segmentation
  if (majorityRadius > 0){
    outname<- "sieve_pre_tree_crowns.sdat"
    ret <- system(paste0("gdal_sieve.py -8 ",
                         path_run,"crowns.sdat ",
                         path_run,outname,
                         " -of SAGA"),
                  intern = TRUE)
    # apply majority filter for smoothing the extremly irregular crown boundaries
    ret <- system(paste0(sagaCmd, " grid_filter 6 ",
                         " -INPUT "   ,path_run,"sieve_pre_tree_crowns.sgrd",
                         " -RESULT "  ,path_run,"crowns.sgrd",
                         " -MODE 0",
                         " -RADIUS "  ,majorityRadius,
                         " -THRESHOLD 0.0 "),
                  intern = TRUE)
  }


  # convert filtered crown clumps to shape format
  ret <- system(paste0(sagaCmd, " shapes_grid 6 ",
                       " -GRID "     ,path_run,"crowns.sgrd",
                       " -POLYGONS " ,path_run,"crowns.shp",
                       " -CLASS_ALL 1" ,
                       " -CLASS_ID 1.0",
                       " -SPLIT 1"),
                intern = TRUE)


  crowns <- rgdal::readOGR(path_run,"crowns", verbose = FALSE)
  #crowns<-tree_crowns[tree_crowns$VALUE > 0,]
  sp::proj4string(crowns)<-proj

  # extract chm stats by potential crown segments
  statRawCrowns <- uavRst::poly_stat(c("chm"),
                                     spdf = crowns)

  rgdal::writeOGR(obj = statRawCrowns,
                  dsn = path_run,
                  layer = "crowns",
                  driver= "ESRI Shapefile",
                  overwrite=TRUE)
  # simple filtering of crownareas based on tree height min max area and artifacts at the analysis/image borderline
  tree_crowns <- crown_filter(crownFn = paste0(path_run,"statRawCrowns.shp"),
                                                 minTreeAlt = minTreeAlt,
                                                 minCrownArea = 0,
                                                 maxCrownArea = 250,
                                                 minTreeAltParam = "chmQ20" )[[2]]

  options(warn=0)
  cat("segmentation finsihed...\n")
  return(tree_crowns)
}



#' Watershed segmentation based on 'ForestTools'
#' @description  'ForestTools' segmentation of individual tree crowns based on a canopy height model and initial seeding points (trees). Very fast algorithm based on the imagr watershed algorithm.
#' Andrew Plowright: R package \href{https://CRAN.R-project.org/package=ForestTools}{'ForestTools'}
#' @param treepos \code{SpatialPointsDataFrame}. The point locations of treetops. The function will generally produce a
#' number of crown segments equal to the number of treetops.
#' @param chm raster*. Canopy height model in \code{raster} format. Should be the same that was used to create
#' the input for \code{treepos}.
#' @param minTreeAlt numeric. The minimum height value for a \code{CHM} pixel to be considered as part of a crown segment.
#' All \code{chm} pixels beneath this value will be masked out. Note that this value should be lower than the minimum
#' height of \code{treepos}.
#' @param format character. Format of the function's output. Can be set to either 'raster' or 'polygons'.
#' @param verbose to be quiet FALSE 
#'
#' @import ForestTools
#'
#' @export
#' @examples
#' \dontrun{
#' 
#' # required packages
#' require(uavRst)
#' require(curl)
#' 
#' # project folder
#' projRootDir<-tempdir()
#' 
#' # create subfolders please mind that the pathes are exported as global variables
#'  paths<-link2GI::initProj(projRootDir = projRootDir,
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")
#' # project folder
#' projRootDir<-tempdir()
#' 
#' # get the data
#'  url <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/treepos_3-3.tif"
#'  url2 <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/chm_3-3.tif"
#'  res <- curl::curl_download(url, paste0(path_run,"treepos_3-3.tif"))
#'  res2 <- curl::curl_download(url2, paste0(path_run,"chm_3-3.tif"))
#' 
#' # read chm data
#'  chmR<- raster::raster(paste0(path_run,"chm_3-3.tif"))
#'  tPos<- raster::raster(paste0(path_run,"treepos_3-3.tif"))
#' 
#' # segmentation
#'  crownsFT <- chmseg_FT(chm = chmR,
#'                        treepos = tPos,
#'                        format = "polygons",
#'                        minTreeAlt = 2,
#'                        verbose = FALSE)
#'
#' }


chmseg_FT <- function(treepos = NULL,
                              chm = NULL,
                              minTreeAlt = 2,
                              format = "polygons",
                              verbose = FALSE) {

  if (class(treepos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    treepos <- raster::rasterToPoints(treepos,spatial = TRUE)
  } else {
    r<-raster::raster(treepos)
    treepos <- raster::rasterToPoints(treepos,spatial = TRUE)
  }

  # Crown segmentation
  crownsFT <- ForestTools::SegmentCrowns(treetops = treepos,
                                         CHM = chm,
                                         format = format,
                                         minHeight = minTreeAlt,
                                         verbose = verbose)

  # # Writing Shapefile
  # rgdal::writeOGR(obj = crownsFT,
  #                 dsn = paste0(path_output, "crowns_FT"),
  #                 layer = "crowns_FT",
  #                 driver= "ESRI Shapefile",
  #                 overwrite=TRUE)

  return(crownsFT)
}

#' Watershed segmentation based on 'rLiDAR'
#' @description  'rLiDAR' segmentation of individual tree crowns based on a canopy height model and initial seeding points (trees). Generic segmentation algorithm
#' Carlos A. Silva et all.: R package \href{https://CRAN.R-project.org/package=rLiDAR}{rLiDAR}\cr
#'
#' @param treepos numeric. \code{matrix} or \code{data.frame} with three columns (tree xy coordinates and height).
#' number of crown segments equal to the number of treetops.
#' @param chm raster*. Canopy height model in \code{raster} or \code{SpatialGridDataFrame} file format. Should be the same that was used to create
#' the input for \code{treepos}.
#' @param maxCrownArea numeric. A single value of the maximum individual tree crown radius expected. Default 10.0 m.
#' height of \code{treepos}.
#' @param exclusion numeric. A single value from 0 to 1 that represents the percent of pixel exclusion.
#' @importFrom rLiDAR FindTreesCHM
#' @export
#' @examples
#' \dontrun{
#' 
#' # required packages
#'  require(uavRst)
#'  require(curl)
#'  require(link2GI)
#' 
#' # project folder
#'  projRootDir<-tempdir()
#' 
#' # create subfolders please mind that the pathes are exported as global variables
#'  paths<-link2GI::initProj(projRootDir = projRootDir,
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")
#' # get the data
#'  url1 <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/treepos_3-3.tif"
#'  url2 <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/chm_3-3.tif"
#'  res1 <- curl::curl_download(url1, paste0(path_run,"treepos_3-3.tif"))
#'  res2 <- curl::curl_download(url2, paste0(path_run,"chm_3-3.tif"))
#' 
#' # read chm data
#'  chmR<- raster::raster(paste0(path_run,"chm_3-3.tif"))
#'  tPos<- raster::raster(paste0(path_run,"treepos_3-3.tif"))
#'
#' # segmentation
#'  crownsRL <- chmseg_RL(chm= chmR,
#'                        treepos= tPos,
#'                        maxCrownArea = 150,
#'                        exclusion = 0.2)
#' }


chmseg_RL <- function(treepos = NULL,
                              chm = NULL,
                              maxCrownArea = 150,
                              exclusion = 0.2) {

  # if (class(treepos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
  #   treepos <- raster::rasterToPoints(treepos,spatial = TRUE)
  # } else {
  #   r<-raster::raster(treepos)
  #   treepos <- raster::rasterToPoints(treepos,spatial = TRUE)
  # }

  maxcrown <- sqrt(maxCrownArea/ pi)
  # Crown segmentation

  xyz <- as.data.frame(raster::rasterToPoints(treepos))
  names(xyz)<- c("x","y","height")
  canopy<-rLiDAR::ForestCAS(chm = chm,
                            loc = xyz,
                            maxcrown = maxcrown,
                            exclusion =exclusion)
  canopy[[1]]@proj4string <- chm@crs
  # Writing Shapefile
  rgdal::writeOGR(obj = canopy[[1]],
                  dsn = paste0(path_output, "crowns_LR"),
                  layer = "crowns_LR",
                  driver= "ESRI Shapefile",
                  overwrite=TRUE)

  return(canopy[[1]])
}


#' Decision tree segmentation method to grow individual tree crowns based on 'itcSegment'
#' @description Segmentation of individual tree crowns as polygons based on a LiDAR derived canopy height model.
#' Michele Dalponte: R package \href{https://CRAN.R-project.org/package=itcSegment}{itcSegment}.
#'  M. Dalponte, F. Reyes, K. Kandare, and D. Gianelle,
#'  "Delineation of Individual Tree Crowns from ALS and Hyperspectral data: a comparison among four methods,"
#'  European Journal of Remote Sensing, Vol. 48, pp. 365-382, 2015.
#'
#' @param chm raster*, Canopy height model in \code{raster} or \code{SpatialGridDataFrame} file format. Should be the same that was used to create
#' the input for \code{treepos}.
#' @param maxCrownArea numeric. A single value of the maximum individual tree crown radius expected. Default 10.0 m.
#' height of \code{treepos}.
#' @param EPSG character. The EPSG code of the reference system of the CHM raster image.
#' @param movingWin numeric. Size (in pixels) of the moving window to detect local maxima. \href{https://CRAN.R-project.org/package=itcSegment}{itcSegment}
#' @param minTreeAlt numeric. Height threshold (m) below a pixel cannot be a local maximum. Local maxima values are used to define tree tops.\href{https://CRAN.R-project.org/package=itcSegment}{itcSegment}
#' @param TRESHSeed numeric. seeding threshold. \href{https://CRAN.R-project.org/package=itcSegment}{itcSegment}
#' @param TRESHCrown numeric. crowns threshold. \href{https://CRAN.R-project.org/package=itcSegment}{itcSegment}
#' @import itcSegment
#' @export chmseg_ITC
#' @examples
#' \dontrun{
#' 
#' # required packages
#'  require(uavRst)
#'  require(curl)
#' 
#' # project folder
#'  projRootDir<-tempdir()
#' 
#' # create subfolders please mind that the pathes are exported as global variables
#'  paths<-link2GI::initProj(projRootDir = projRootDir,
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")
#' 
#' # get the data
#'  url <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/treepos_3-3.tif"
#'  url2 <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/chm_3-3.tif"
#'  res <- curl::curl_download(url, paste0(path_run,"treepos_3-3.tif"))
#'  res2 <- curl::curl_download(url2, paste0(path_run,"chm_3-3.tif"))
#' 
#' # read chm data
#'  chmR<- raster::raster(paste0(path_run,"chm_3-3.tif"))
#'  tPos<- raster::raster(paste0(path_run,"treepos_3-3.tif"))
#' 
#' # segmentation
#'  crownsITC<- chmseg_ITC(chm = chmR,
#'                         EPSG =3064,
#'                         movingWin = 3,
#'                         TRESHSeed = 0.45,
#'                         TRESHCrown = 0.55,
#'                         minTreeAlt = 2,
#'                         maxCrownArea = 150)
#' }


chmseg_ITC <- function(chm =NULL,
                               EPSG =3064,
                               movingWin = 7,
                               TRESHSeed = 0.45,
                               TRESHCrown = 0.55,
                               minTreeAlt = 2,
                               maxCrownArea = 100) {

  # if (class(treepos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
  #   chm <- raster::raster(chm)
  # }

  maxcrown <- sqrt(maxCrownArea/ pi)*2

  crown_polygon <- itcSegment::itcIMG(imagery = chm,
                                      epsg = EPSG,
                                      TRESHSeed =  0.45,
                                      TRESHCrown = 0.55,
                                      searchWinSize = movingWin,
                                      th = minTreeAlt,
                                      DIST = maxcrown,
                                      ischm = TRUE)

  rgdal::writeOGR(crown_polygon,
                  dsn = paste0(path_output, "crowns_itc", "localMax", minTreeAlt, "_crownDiam", maxCrownArea),
                  layer = "result",
                  driver= "ESRI Shapefile",
                  overwrite=TRUE)



  return(crown_polygon)
}



