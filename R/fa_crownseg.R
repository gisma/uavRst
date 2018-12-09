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
#' @param maxCrownArea numeric. A single value of the maximum projected tree crown area allowed. Default 100 sqm.
#' @param minCrownArea numeric. A single value of the minimum projected tree crown area allowed. Default 3 sqm
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
#'@param parallel running parallel or not default = 1
#'@export
#'@examples
#' \dontrun{
#' ##- required packages
#' require(uavRst)
#' require(link2GI)
#' ##- linkages
#' ##- create and check the links to the GI software
#' giLinks<-uavRst::linkAll(linkItems = c("saga","gdal"))
#' if (giLinks$saga$exist ) {
#'
#' ##- project folder
#' projRootDir<-tempdir()
#'
#' ##- create subfolders please mind that the pathes are exported as global variables
#' paths<-link2GI::initProj(projRootDir = projRootDir,
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")
#' ##- overide trailing backslash issue
#'  path_run<-ifelse(Sys.info()["sysname"]=="Windows", sub("/$", "",path_run),path_run)
#'  setwd(path_run)
#'  unlink(paste0(path_run,"*"), force = TRUE)
#'
#' ##- get the data
#'  data(chm_seg)
#'  data(trp_seg)
#'
#' ##- tree segmentation
#' crowns_GWS <- chmseg_GWS( treepos = trp_seg[[1]],
#'                       chm = chm_seg[[1]],
#'                       minTreeAlt = 10,
#'                       neighbour = 0,
#'                       thVarFeature = 1.,
#'                       thVarSpatial = 1.,
#'                       thSimilarity = 0.003,
#'                       giLinks = giLinks )[[2]]
#'
#'##- visualize it
#'raster::plot(crowns_GWS)
#' }
#'}




chmseg_GWS <- function(treepos = NULL,
                       chm = NULL,
                       minTreeAlt         =2,
                       minTreeAltParam = "chm_Q20",
                       minCrownArea =  3,
                       maxCrownArea = 100,
                       leafsize       = 256,
                       normalize      = 0,
                       neighbour      = 1,
                       method         = 0,
                       thVarFeature   = 1.,
                       thVarSpatial   = 1.,
                       thSimilarity   = 0.002,
                       segmentationBands    = c("chm"),
                       majorityRadius    = 3.000,
                       parallel = 1,
                       giLinks = NULL) {
  if (!exists("path_run")) path_run = tempdir()
  proj<- raster::crs(treepos)
  if (class(treepos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    raster::writeRaster(treepos,file.path(R.utils::getAbsolutePath(path_run),"treepos.sdat"),overwrite = TRUE,NAflag = 0)
  }
  if (class(chm) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    chm[chm<minTreeAlt] = -1
    raster::writeRaster(chm,file.path(R.utils::getAbsolutePath(path_run),"chm.sdat"),overwrite = TRUE,NAflag = 0)
  }


  cat("::: run main segmentation...\n")
  # create correct param lists
  #segmentationBands<-c("HI","GLI")
  if (is.null(giLinks)){
    giLinks <- linkAll()
  }

  gdal <- giLinks$gdal
  saga <- giLinks$saga
  sagaCmd <- R.utils::getAbsolutePath(saga$sagaCmd)
  if (Sys.info()["sysname"]=="Windows") saga$sagaPath <- utils::shortPathName(saga$sagaPath)
  invisible(env<-RSAGA::rsaga.env(path =saga$sagaPath))

  param_list <- paste0(file.path(R.utils::getAbsolutePath(path_run),paste0(segmentationBands,".sgrd;")),collapse = "")

  # Start final segmentation algorithm as provided by SAGA's seeded Region Growing segmentation (imagery_segmentation 3)
  # TODO sensitivity analysis of the parameters

  RSAGA::rsaga.geoprocessor(lib = "imagery_segmentation", module = 3,
                            param = list(SEEDS =  file.path(R.utils::getAbsolutePath(path_run),"treepos.sgrd"),
                                         FEATURES = param_list,
                                         SEGMENTS = file.path(R.utils::getAbsolutePath(path_run),"crowns.sgrd"),
                                         LEAFSIZE = leafsize,
                                         NORMALIZE = normalize,
                                         NEIGHBOUR = neighbour,
                                         METHOD   =  method,
                                         SIG_1    =  thVarFeature,
                                         SIG_2    =  thVarSpatial,
                                         THRESHOLD = thSimilarity),
                            env = env,
                            intern = TRUE,
                            invisible = FALSE,show.output.on.console = TRUE)
    
   seg_GWS<-fileProcStatus(module = "imagerysegementation3",file = "crowns.sgrd",listname = "seg")

  
  # fill the holes inside the crowns (simple approach)
  # TODO better segmentation
  if (majorityRadius > 0 ){
    outname<- "crowns1.sdat"
    
    if (!is.null(gdal$version$py[[1]])){
      if (grepl(gdal$version$py[[1]][2,1],pattern = "gdal"))
        ret <- system(paste0(gdal$version$py[[1]][17,]," -8 ",
                             file.path(R.utils::getAbsolutePath(path_run)),"/","crowns.sdat ",
                             file.path(R.utils::getAbsolutePath(path_run)),"/",outname,
                             " -of SAGA"), 
                      intern = TRUE)}
    else {
      file.copy(file.path(R.utils::getAbsolutePath(path_run),"/crowns.sgrd"),file.path(R.utils::getAbsolutePath(path_run),"/crowns1.sgrd"),overwrite = TRUE)
      file.copy(file.path(R.utils::getAbsolutePath(path_run),"/crowns.sdat"),file.path(R.utils::getAbsolutePath(path_run),"/crowns1.sdat"),overwrite = TRUE)
      cat(getCrayon()[[2]]("\n GDAL Python module 'sieve' is NOT found. Expected to be at: "),
          getCrayon()[[4]](names(gdal$python_utilities),"\n" ))
      }
    # apply majority filter for smoothing the extremly irregular crown boundaries
  } else {
    file.copy(file.path(R.utils::getAbsolutePath(path_run),"/crowns.sgrd"),file.path(R.utils::getAbsolutePath(path_run),"/crowns1.sgrd"),overwrite = TRUE)
    file.copy(file.path(R.utils::getAbsolutePath(path_run),"/crowns.sdat"),file.path(R.utils::getAbsolutePath(path_run),"/crowns1.sdat"),overwrite = TRUE)
}
    if (RSAGA::rsaga.get.version(env = env) > "3.0.0") {
    ret <- system(paste0(sagaCmd, " grid_filter 6 ",
                         " -INPUT "   ,file.path(R.utils::getAbsolutePath(path_run)),"/crowns1.sgrd",
                         " -RESULT "  ,file.path(R.utils::getAbsolutePath(path_run)),"/crowns2.sgrd",
                         " -TYPE 0",
                         " -KERNEL_RADIUS "  ,majorityRadius,
                         " -THRESHOLD 0.0 "),
                  intern = TRUE)  
    } 
    else {
      ret <- system(paste0(sagaCmd, " grid_filter 6 ",
                           " -INPUT "   ,file.path(R.utils::getAbsolutePath(path_run)),"/crowns1.sgrd",
                           " -RESULT "  ,file.path(R.utils::getAbsolutePath(path_run)),"/crowns2.sgrd",
                           " -MODE 0",
                           " -RADIUS "  ,majorityRadius,
                           " -THRESHOLD 0.0 "),
                    intern = TRUE)
    }
  





   
   seg_GWS<-append(seg_GWS,fileProcStatus("sievefilter","crowns1.sgrd",listname = "seg"))
  # convert filtered crown clumps to shape format
  ret <- system(paste0(sagaCmd, " shapes_grid 6 ",
                       " -GRID "     ,file.path(R.utils::getAbsolutePath(path_run)),"/crowns2.sgrd",
                       " -POLYGONS " ,file.path(R.utils::getAbsolutePath(path_run)),"/crowns.shp",
                       " -CLASS_ALL 1" ,
                       " -CLASS_ID 1.0",
                       " -SPLIT 1"),
                intern = TRUE)
  
  seg_GWS<-append(seg_GWS,fileProcStatus("shapes_grid6","crowns.shp",listname = "seg"))
  if (seg_GWS$shapes_grid6)
    crowns   <- shp2so(file.path(R.utils::getAbsolutePath(paste0(path_run,"/crowns.shp"))))
    # crowns <- rgdal::readOGR(dsn = file.path(R.utils::getAbsolutePath(path_run)),
    #                        layer = "crowns", 
    #                        verbose = FALSE)
  else return(cat(paste0("File ",file.path(R.utils::getAbsolutePath(path_run)),"/crowns.shp not found \n")))
  
  #crowns<-tree_crowns[tree_crowns$VALUE > 0,]
  sp::proj4string(crowns)<-proj
  
  
  # extract chm stats by potential crown segments
  statRawCrowns <- uavRst::poly_stat(c("chm"),
                                     spdf = crowns,
                                     path_run = path_run,
                                     parallel = parallel,
                                     giLinks = giLinks)
 
  seg_GWS<-append(seg_GWS,fileProcStatus("poly_stat","spdf.shp",listname = "seg"))
  
  if (seg_GWS$poly_stat)
{    rgdal::writeOGR(obj = statRawCrowns,
                    dsn = file.path(R.utils::getAbsolutePath(path_run)),
                    layer = "crownsstat",
                    driver= "ESRI Shapefile",
                    overwrite=TRUE)
  }
  else return(cat(paste0("File ",file.path(R.utils::getAbsolutePath(path_run)),"spdf.shp not found \n")))
  

  # simple filtering of crownareas based on tree height min max area and artifacts at the analysis/image borderline
  tree_crowns <- crown_filter(crownFn = file.path(R.utils::getAbsolutePath(path_run),"crownsstat.shp"),
                              minTreeAlt = minTreeAlt,
                              minCrownArea = minCrownArea,
                              maxCrownArea = maxCrownArea,
                              minTreeAltParam = minTreeAltParam )
  
  #nrow(tree_crowns[[1]])
  options(warn=0)
  cat("segmentation finsihed...\n")
  sp::proj4string(statRawCrowns)<-sp::proj4string(tree_crowns[[1]])
  tree_crowns<- append(tree_crowns,statRawCrowns)
  return(tree_crowns)
}



#' Watershed segmentation based on 'ForestTools'
#' @description  'ForestTools' segmentation of individual tree crowns based on a canopy height model and initial seeding points (trees). Very fast algorithm based on the imagr watershed algorithm.
#' Andrew Plowright: R package \href{https://CRAN.R-project.org/package=ForestTools}{'ForestTools'}
#' @param treepos \code{raster*}. The positions of the estimated top of trees. The function will generally produce a
#' number of crown segments equal to the number of treetops.
#' @param chm raster*. Canopy height model in \code{raster} format. Should be the same that was used to create
#' the input for \code{treepos}.
#' @param minTreeAlt numeric. The minimum height value for a \code{CHM} pixel to be considered as part of a crown segment.
#' All \code{chm} pixels beneath this value will be masked out. Note that this value should be lower than the minimum
#' height of \code{treepos}.
#' @param format character. Format of the function's output. Can be set to either 'raster' or 'polygons'.
#' @param winRadius numeric the fixed radius written in the SPDF of the tree top file if not generated by Foresttools
#' @param verbose to be quiet FALSE
#'
#' @export
#' 
#' @examples

#'  require(uavRst)
#' ## get the data
#'  data(chm_seg)
#'  data(trp_seg)
#' ## you may create the Foresttools tree tops
#' ## otherwise the raster is transformed to a SPDF according to the FT format   
#' #trp <- ForestTools::vwf(chm_seg[[1]], winFun = function(x){x * 0.06 + 0.5}, minHeight = 2)
#' ## segmentation
#' crownsFT <- chmseg_FT(chm = chm_seg[[1]],
#'                        treepos = trp_seg[[1]],
#'                        format = "polygons",
#'                        minTreeAlt = 23,
#'                        winRadius = 1.5,
#'                        verbose = FALSE)
#'
#' ## Visualisation
#' raster::plot(crownsFT)
#' 






chmseg_FT <- function(treepos = NULL,
                      chm = NULL,
                      minTreeAlt = 2,
                      format = "polygons",
                      winRadius = 1.5,
                      verbose = FALSE) {

  if (class(treepos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    # add projection of chm TODO
    pr<-raster::crs(raster::projection(raster::raster(chm)))
    raster::projection(treepos) <- pr
    treepos <- raster::rasterToPoints(treepos,spatial = TRUE)
    # reformat it to the needs of mcws
    treepos@data$layer <- 1
    treepos@data$winRadius <- 1.5
    treepos@data$treeID <- seq(1:nrow(treepos@data))
    names(treepos)<-c("height","treeID","winRadius","layer")
    
    
  }  

  # Crown segmentation
  crownsFT <- ForestTools::mcws(treetops = treepos,
                                 CHM = chm,
                                 format = format,
                                 minHeight = minTreeAlt,
                                 verbose = verbose)

  
  
  
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
#' @param maxCrownArea numeric. A single value of the minimum projected tree crown area allowed. Default is 100 sqm
#' height of \code{treepos}.
#' @param exclusion numeric. A single value from 0 to 1 that represents the percent of pixel exclusion.

#' @export
#' @examples

#'\dontrun{
#' ## required packages
#'  require(uavRst)

#' ## read chm and tree position data
#'  data(chm_seg)
#'  data(trp_seg)

#' ## segmentation
#'  crownsRL <- chmseg_RL(chm= chm_seg[[1]],
#'                        treepos= trp_seg[[1]],
#'                        maxCrownArea = 150,
#'                        exclusion = 0.2)
#' ## visualisation
#'  raster::plot(crownsRL)
#'  } 





chmseg_RL <- function(treepos = NULL,
                      chm = NULL,
                      maxCrownArea = 100,
                      exclusion = 0.2) {
  if (!exists("path_run")) path_run = tempdir()
  if (class(treepos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    treepos <- raster::rasterToPoints(treepos,spatial = TRUE)
  } else {
    stop("Please provide an object of tye raster*")
  }

  maxcrown <- sqrt(maxCrownArea / pi)
  # Crown segmentation

  xyz <- as.data.frame(treepos)
  xyz2 = xyz[,c(2,3,1)]
  names(xyz2) <- c("x", "y", "height")
  canopy <- rLiDAR::ForestCAS(
    chm = chm,
    loc = xyz2,
    maxcrown = maxcrown,
    exclusion = exclusion
  )
  canopy[[1]]@proj4string <- chm@crs
  # Writing Shapefile
  rgdal::writeOGR(
    obj = canopy[[1]],
    dsn = file.path(R.utils::getAbsolutePath(path_run)),
    layer = "crowns_LR",
    driver = "ESRI Shapefile",
    overwrite = TRUE
  )

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
#' @param maxCrownArea numeric. A single value of the maximum projected tree crown area allowed. Default 100 sqm.
#' height of \code{treepos}.
#' @param EPSG character. The EPSG code of the reference system of the CHM raster image.
#' @param movingWin numeric. Size (in pixels) of the moving window to detect local maxima. \href{https://CRAN.R-project.org/package=itcSegment}{itcSegment}
#' @param minTreeAlt numeric. Height threshold (m) below a pixel cannot be a local maximum. Local maxima values are used to define tree tops.\href{https://CRAN.R-project.org/package=itcSegment}{itcSegment}
#' @param TRESHSeed numeric. seeding threshold. \href{https://CRAN.R-project.org/package=itcSegment}{itcSegment}
#' @param TRESHCrown numeric. crowns threshold. \href{https://CRAN.R-project.org/package=itcSegment}{itcSegment}

#' @export chmseg_ITC
#' @examples


#'  require(uavRst)
#'  data(chm_seg)
#' ##- segmentation
#' crownsITC<- chmseg_ITC(chm = chm_seg[[1]],
#'                        EPSG =3064,
#'                        movingWin = 7,
#'                        TRESHSeed = 0.45,
#'                        TRESHCrown = 0.55,
#'                        minTreeAlt = 5,
#'                        maxCrownArea = 50)
#'
#' ##- visualisation
#' raster::plot(crownsITC)





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
  if (!exists("path_run")) path_run = tempdir()
  maxcrown <- sqrt(maxCrownArea/ pi)*2

  crown_polygon <- itcSegment::itcIMG(imagery = chm,
                                      epsg = EPSG,
                                      TRESHSeed =  0.45,
                                      TRESHCrown = 0.55,
                                      searchWinSize = movingWin,
                                      th = minTreeAlt,
                                      DIST = maxcrown,
                                      ischm = TRUE)
  # rgdal::writeOGR(crown_polygon,
  #                 dsn = paste0(path_run, "crowns_itc", "localMax", minTreeAlt, "_crownDiam", maxCrownArea),
  #                 layer = "result",
  #                 driver= "ESRI Shapefile",
  #                 overwrite=TRUE)
  return(crown_polygon)
}
