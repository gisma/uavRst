if (!isGeneric('fa_crownSegmentation')) {
  setGeneric('fa_crownSegmentation', function(x, ...)
    standardGeneric('fa_crownSegmentation'))
}

#'@name fa_crownSegmentation
#'@title Tree crown segmentation based on a seeded region growing algorithm
#'
#'@description
#' Tree segmentation based on a CHM, basically returns a  vector data sets with the tree crown geometries and a bunch of corresponding indices. After the segementation itself the results are hole filled and optionally filtered by a majority filter in the 3*3 surrounding.
#'
#'@author Chris Reudenbach
#'
#'@param treePos  spatial raster object
#'@param minTreeAlt  numeric. The minimum height value for a \code{chm} pixel to be considered as part of a crown segment.
#' All \code{chm} pixels beneath this value will be masked out. Note that this value should be lower than the minimum
#' height of \code{treePos}.
#'@param mintreeAltParam default is "chmQ20"
#' @param chm Canopy height model in \link[raster]{raster} format. Should be the same that was used to create
#' the input for \code{treePos}.
#'@param leafsize       integer. bin size of grey value sampling range from 1 to 256 
#'@param normalize      integer.  logical switch if data will be normalized (1) 
#'@param neighbour      integer.  von Neumanns' neighborhood (0) or Moore's (1) 
#'@param method         integer. growing algorithm for feature space and position (0) or feature space only (1)
#'@param thVarSpatial   numeric. spatial variance 
#'@param thVarFeature   numeric. spatial variance 
#'@param thSimilarity   mumeric. similarity threshold 
#'@param seed_params    vector. of characters corresponding with the used attributes. The altitude values from surface model \code{c("chm")} is mandantory. 
#'@param giLinks        list. of GI tools cli pathes  
#'@export fa_crownSegmentation
#'@examples
#'\dontrun{
#' # Tree segmentation based on a CHM
#'  fa_crownSegmentation(x = rasterobj,  "nameofSAGAFile")
#'}
#'
#'

fa_crownSegmentation <- function(treePos = NULL,
                                   chm = NULL,
                                  minTreeAlt         =2,
                                 mintreeAltParam = "chmQ20",
                                  leafsize       = 256,
                                  normalize      = 0,
                                  neighbour      = 1,
                                  method         = 0,
                                  thVarFeature   = 1.,
                                  thVarSpatial   = 1.,
                                  thSimilarity   = 0.002,
                                  seed_params    = c("chm"),
                                  majority_radius    = 3.000,
                                  giLinks = NULL) {
  proj<- raster::crs(treePos)
  if (class(treePos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    raster::writeRaster(treePos,file.path(path_run,"treePos.sdat"),overwrite = TRUE,NAflag = 0)
  }
  if (class(chm) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    chm[chm<minTreeAlt] = -1
    raster::writeRaster(chm,file.path(path_run,"chm.sdat"),overwrite = TRUE,NAflag = 0)
  }
  
  
  cat("::: run main segmentation...\n")
  # create correct param list s
  #seed_params<-c("HI","GLI")
  if (is.null(giLinks)){
    giLinks <- linkBuilder()
  }
  
  gdal <- giLinks$gdal
  saga <- giLinks$saga
  sagaCmd<-saga$sagaCmd
    
  
  param_list <- paste0(path_run,seed_params,".sgrd;",collapse = "")
  
  # Start final segmentation algorithm as provided by SAGA's seeded Region Growing segmentation (imagery_segmentation 3)
  # TODO sensitivity analysis of the parameters
  ret <- system(paste0(sagaCmd, " imagery_segmentation 3 ",
                       " -SEEDS "    ,path_run,"treePos.sgrd",
                       " -FEATURES '", param_list,
                       "' -SEGMENTS ",path_run,"crowns.shp",
                       " -LEAFSIZE " ,leafsize,
                       " -NORMALIZE ",normalize,
                       " -NEIGHBOUR ",neighbour, 
                       " -METHOD "   ,method,
                       " -SIG_1 "    ,thVarFeature,
                       " -SIG_2 "    ,thVarSpatial,
                       " -THRESHOLD ",thSimilarity),
                intern = TRUE)
  
  # fill holes inside the crowns (simple approach)
  # TODO better segmentation
  if (majority_radius > 0){
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
                         " -RADIUS "  ,majority_radius,
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
  statRawCrowns <- uavRst::xpolystat(c("chm"),
                                     spdf = crowns)
  
  # export geojson
  sf::st_write(sf::st_as_sf(statRawCrowns), "crowns.geojson",delete_dsn=TRUE,driver="GeoJSON")
  
  # simple filtering of crownareas based on tree height min max area and artifacts at the analysis/image borderline
  tree_crowns <- uavRst::fa_basicTreeCrownFilter(crownFn = paste0(path_run,"crowns.geojson"),
                                                  minTreeAlt = minTreeAlt,
                                                  minCrownArea = 0,
                                                  maxCrownArea = 250,
                                                  mintreeAltParam = "chmQ20" )[[2]]
  
  options(warn=0)
  cat("segmentation finsihed...\n")
  return(tree_crowns)
} 



#' very fast watershed segemenation algorithm provided by 'ForestTools'
#' @description Segmentation of individual tree crowns based on a canopy height model and initial seeding points (trees). Very fast algorithm based on the imagr watershed algorithm.
#' Andrew Plowright: R package \href{https://CRAN.R-project.org/package=ForestTools}{'ForestTools'}
#' @name fa_crownSegmentationFT

#' @param treePos \link[sp]{SpatialPointsDataFrame}. The point locations of treetops. The function will generally produce a
#' number of crown segments equal to the number of treetops.
#' @param chm Canopy height model in \link[raster]{raster} format. Should be the same that was used to create
#' the input for \code{treePos}.
#' @param minTreeAlt numeric. The minimum height value for a \code{CHM} pixel to be considered as part of a crown segment.
#' All \code{chm} pixels beneath this value will be masked out. Note that this value should be lower than the minimum
#' height of \code{treePos}.
#' @param format string. Format of the function's output. Can be set to either 'raster' or 'polygons'.


#' @export fa_crownSegementationFT
#' @examples 
#' \dontrun{
#'  crownsFT <- crownSegementationFT(chm = kootenayCHM,
#'                                  treePos = tpos 
#'                                 format = "polygons", 
#'                                 minTreeAlt = 1.5, 
#'                                 verbose = FALSE)
#' }

## packages
require(ForestTools)

fa_crownSegementationFT <- function(treePos = NULL, 
                                    chm = NULL,
                                    minTreeAlt = 2,
                                    format = "polygons",
                                    verbose = FALSE) {
  
  if (class(treePos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    treePos <- raster::rasterToPoints(treePos,spatial = TRUE)
  } else {
    r<-raster::raster(treePos)
    treePos <- raster::rasterToPoints(treePos,spatial = TRUE)
  }
      
      # Crown segmentation
      crownsFT <- ForestTools::SegmentCrowns(treetops = treePos, 
                                CHM = chm, 
                                format = format, 
                                minHeight = minTreeAlt, 
                                verbose = verbose)
      
      # Writing Shapefile
      rgdal::writeOGR(obj = crownsFT,
               dsn = paste0(path_output, "crowns_FT"),
               layer = "crowns_FT",
               driver= "ESRI Shapefile",
               overwrite=TRUE)

  return(crownsFT)
}