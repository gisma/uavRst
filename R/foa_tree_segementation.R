if (!isGeneric('foa_tree_segementation')) {
  setGeneric('foa_tree_segementation', function(x, ...)
    standardGeneric('foa_tree_segementation'))
}

#'@name foa_tree_segementation
#'@title Tree segementation based on a CHM
#'
#'@description
#' Tree segementation based on a CHM
#'
#'@author Chris Reudenbach
#'
#'@param chmFn filname auf SAGA grid 
#'@param minTreeAlt      = 3,    # -thresholdfor minimum tree altitude in meter
#'@param thtreeNodes     = 6,    # minimum number of ldd connections
#'@param crownMinArea    = 7,    #(approx 1.25 m diameter)
#'@param crownMaxArea    = 200,  #(approx 17.8 m diameter)
#'@param WLRatio         = 0.53, # crown width length ratio
#'@param thLongit        = 0.5,  # crown longitudiness 
#'@param solidity        = 1.0, # solidity 
#'@param is0_output      = 0,     # 0= seed value 1=segment id
#'@param is0_join        = 2,     # 0=no join, 1=seed2saddle diff, 2=seed2seed diff
#'@param is0_thresh      = 0.05,  # threshold for join difference in m
#'@param is3_leafsize    = 8,
#'@param is3_normalize   = 1,
#'@param is3_neighbour   = 1,
#'@param is3_method      = 0,
#'@param is3_sig1        =  0.1,
#'@param is3_sig2        = 3.01,
#'@param is3_threshold   = 0.001,
#'@param majority_radius = 5.000

#'@return basically returns a  vector data sets with the tree crown geometries and a bunch of corresponding indices
#'
#'
#'@export foa_tree_segementation
#'@examples
#'\dontrun{
#' # Tree segementation based on a CHM
#'  foa_tree_segementation(chmFn =  "nameofSAGA.grd")
#'}
#'
foa_tree_segementation <- function(chmFn = NULL,
                                   minTreeAlt      = 3,    # -thresholdfor minimum tree altitude in meter
                                   thtreeNodes     = 6,    # minimum number of ldd connections
                                   crownMinArea    = 7,    #(approx 1.25 m diameter)
                                   crownMaxArea    = 200,  #(approx 17.8 m diameter)
                                   WLRatio         = 0.53, # crown width length ratio
                                   thLongit        = 0.5,  # crown longitudiness 
                                   solidity        = 1.0, # solidity 
                                   is0_output      = 0,     # 0= seed value 1=segment id
                                   is0_join        = 2,     # 0=no join, 1=seed2saddle diff, 2=seed2seed diff
                                   is0_thresh      = 0.05,  # threshold for join difference in m
                                   is3_leafsize    = 8,
                                   is3_normalize   = 1,
                                   is3_neighbour   = 1,
                                   is3_method      = 0,
                                   is3_sig1        =  0.1,
                                   is3_sig2        = 3.01,
                                   is3_threshold   = 0.001,
                                   majority_radius = 5.000
                                   
)  {
  cat(":: run segementation...\n")
  chmFn <- path.expand(chmFn)
  ret <- system(paste0(sagaCmd, " imagery_segmentation 0 ",
                       " -GRID "     ,chmFn,
                       " -SEGMENTS " ,path_run,"dummyCrownSegments.sgrd",
                       " -SEEDS "    ,path_run,"treeSeeds.shp",
                       " -OUTPUT "   ,is0_output, 
                       " -DOWN 1"    , 
                       " -JOIN "     ,is0_join,
                       " -THRESHOLD ", is0_thresh, 
                       " -EDGE 1")
                ,intern = TRUE)
  # read from the analysis (imagery_segmentation 0)
  trees <- rgdal::readOGR(path_run,"treeSeeds")
  # apply tree min height filter 
  trees <- trees[trees$VALUE > minTreeAlt ,] 
  trees@proj4string <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  # converting to raster 
  rawTrees  <-  chmR * 0.0
  raster::writeRaster(rawTrees,paste0(path_run,"treeSeeds.tif"),overwrite = TRUE)
  ret <- system(paste0("gdal_rasterize ",
                       path_run,"treeSeeds.shp ", 
                       path_run,"treeSeeds.tif",
                       " -l treeSeeds",
                       " -a VALUE"),
                intern = TRUE)
  rawTrees  <- raster::raster(paste0(path_run,"treeSeeds.tif"))
  rawTrees[rawTrees <= 0] <- NA
  raster::writeRaster(rawTrees,paste0(path_run,"treeSeeds.tif"),overwrite = TRUE)
  # convert to SAGA
  gdalUtils::gdalwarp(paste0(path_run,"treeSeeds.tif"), 
                      paste0(path_run,"treeSeeds.sdat"), 
                      overwrite = TRUE,  
                      of = 'SAGA',
                      verbose = FALSE)
  ret <- system(paste0(sagaCmd," grid_tools 15 ",
                       " -INPUT ",path_run,"treeSeeds.sgrd",
                       " -RESULT ",path_run,"seeds.sgrd",
                       " -METHOD 0",
                       " -OLD 0.00",
                       " -NEW 0.00",
                       " -SOPERATOR 0",
                       " -NODATAOPT 0",
                       " -NODATA 0.0",
                       "  -RESULT_NODATA_CHOICE 1",
                       " -RESULT_NODATA_VALUE 0.000000"),
                intern = TRUE)
  
  # SAGA Seeded Region Growing segmentation (imagery_segmentation 3)
  ret <- system(paste0(sagaCmd, " imagery_segmentation 3 ",
                       " -SEEDS "   ,path_run,"seeds.sgrd",
                       " -FEATURES '"   ,
                       path_run,"VVI.sgrd;", 
                       path_run,"VARI.sgrd;",
                       path_run,"chm.sgrd'",
                       " -SEGMENTS "   ,path_run,"pre_tree_crowns.sgrd",
                       " -LEAFSIZE "   ,is3_leafsize,
                       " -NORMALIZE ",is3_normalize,
                       " -NEIGHBOUR ",is3_neighbour, 
                       " -METHOD ",is3_method,
                       " -SIG_1 ",is3_sig1,
                       " -SIG_2 ",is3_sig2,
                       " -THRESHOLD ",is3_threshold),
                intern = TRUE)
  
  ret <- system(paste0("gdal_sieve.py -8 ",
                       path_run,"pre_tree_crowns.sdat ",
                       path_run,"fpre_tree_crowns.sdat",
                       " -of SAGA"),
                intern = TRUE)
  
  ret <- system(paste0(sagaCmd, " grid_filter 6 ",
                       " -INPUT ",path_run,"fpre_tree_crowns.sgrd",
                       " -RESULT ",path_run,"tree_crowns.sgrd",
                       " -MODE 0",
                       " -RADIUS ",majority_radius,
                       " -THRESHOLD 0.000000 "),
                intern = TRUE)
  
  ret <- system(paste0(sagaCmd, " shapes_grid 6 ",
                       " -GRID ",path_run,"tree_crowns.sgrd",
                       " -POLYGONS ",path_run,"tree_crowns.shp",
                       " -CLASS_ALL 1",
                       " -CLASS_ID 1.000000",
                       " -SPLIT 1"),
                intern = TRUE)
  
  # calculate statistics for each crown 
  ret <-  system(paste0(sagaCmd, " shapes_grid 2 ",
                        " -GRIDS ",path_run,"chm.sgrd ",
                        " -POLYGONS ",path_run,"tree_crowns.shp",
                        " -NAMING 1",
                        " -METHOD 2",
                        " -COUNT 1 -MIN  1 -MAX 1 -RANGE 1 -SUM 1 -MEAN 1 -VAR 1 -STDDEV 1",
                        " -QUANTILE 10",
                        " -PARALLELIZED 1",
                        " -RESULT ",path_run,"crownsStat.shp"),
                 intern = TRUE)
  
  # dirty combining of data tables
  ch <- rgdal::readOGR(path_run,"tree_crowns")
  names(ch) <- gsub(names(ch),pattern = "\\NAME",replacement = "NAMEgeom")
  names(ch) <- gsub(names(ch),pattern = "\\ID",replacement = "IDgeom")
  names(ch) <- gsub(names(ch),pattern = "\\VALUE",replacement = "VALUEgeom")
  stats     <- rgdal::readOGR(path_run,"crownsStat")
  ch@data   <- cbind(ch@data,stats@data)
  names(ch) <- gsub(names(ch),pattern = "\\.",replacement = "")
  #ch_s <- gSimplify(ch, 0.1, topologyPreserve=TRUE)
  rgdal::writeOGR(obj = ch, 
                  layer = "treecrowns", 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  trees_crowns <- classifyTreeCrown(crownFn = paste0(path_run,"treecrowns.shp"),  
                                      funNames = c("eccentricityboundingbox","solidity"),
                                      minTreeAlt = minTreeAlt, 
                                      crownMinArea = crownMinArea, 
                                      crownMaxArea = crownMaxArea, 
                                      solidity = solidity, 
                                      WLRatio = WLRatio)
  
  rgdal::writeOGR(obj = trees_crowns_3[[2]], 
                  layer = "finalcrowns", 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  
  # pixvalues <- basicExtraction(x = chmR,fN = trees_crowns_2[[2]],responseCat = "ID")
  
  return(trees_crowns)
} 