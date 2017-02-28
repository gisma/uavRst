if (!isGeneric('fa_tree_segementation')) {
  setGeneric('fa_tree_segementation', function(x, ...)
    standardGeneric('fa_tree_segementation'))
}

#'@name fa_tree_segementation
#'@title Tree segementation based on a CHM
#'
#'@description
#' Tree segementation based on a CHM
#'
#'@author Chris Reudenbach
#'
#'@param x  spatial raster object
#'@param minTreeAlt      = 3,    # -thresholdfor minimum tree altitude in meter
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
#'@export fa_tree_segementation
#'@examples
#'\dontrun{
#' # Tree segementation based on a CHM
#'  fa_tree_segementation(x = rasterobj,  "nameofSAGAFile")
#'}
#'
fa_tree_segementation <- function(x = NULL,
                                   minTreeAlt      = 3,    # -thresholdfor minimum tree altitude in meter
                                   crownMinArea    = 7,    #(approx 1.25 m diameter)
                                   crownMaxArea    = 200,  #(approx 17.8 m diameter)
                                   WLRatio         = 0.53, # crown width length ratio
                                   thLongit        = 0.5,  # crown longitudiness 
                                   solidity        = 1.0, # solidity 
                                   is0_output      = 1,     # 0= seed value 1=segment id
                                   is0_join        = 1,     # 0=no join, 1=seed2saddle diff, 2=seed2seed diff
                                   is0_thresh      = 0.09,  # threshold for join difference in m
                                   is3_leafsize    = 8,
                                   is3_normalize   = 1,
                                   is3_neighbour   = 1,
                                   is3_method      = 0,
                                   is3_sig1        =  0.1,
                                   is3_sig2        = 3.01,
                                   is3_threshold   = 0.001,
                                   majority_radius = 5.000
                                   
)  {
  cat("\n:: start crown identification...\n")
  options(warn=-1)
  if (!exists(sagaCmd)) link2GI::linkSAGA()
  uavRst:::R2SAGA(x,"chm")
  cat(":: run seed finding...\n")
  # technical reclassification of negative values to 0.1
  # TODO check why negativ and check reclassification
  # ret <- system(paste0(sagaCmd, "  grid_tools 15 ",
  #                      " -INPUT "     ,path_run,"chm.sgrd",
  #                      " -RESULT "     ,path_run,"chm.sgrd",
  #                      " -METHOD 0 ",
  #                      " -OLD 0.000000",
  #                      " -NEW 0.100000",
  #                      " -SOPERATOR 1",
  #                      " -NODATAOPT 0",
  #                      " -OTHEROPT 0",
  #                      " -RESULT_NODATA_CHOICE 1 ",
  #                      " -RESULT_NODATA_VALUE 0.000000")
  #               ,intern = TRUE)
  # first segment run is a simple watershed segementation just for deriving more reliable seeds 
  # TODO improve seed finding
  ret <- system(paste0(sagaCmd, " imagery_segmentation 0 ",
                       " -GRID "     ,path_run,"chm.sgrd",
                       #" -SEGMENTS " ,path_run,"dummyCrownSegments.sgrd",
                       " -SEEDS "    ,path_run,"treeSeeds.shp",
                       " -OUTPUT "   ,is0_output, 
                       " -DOWN 1"    , 
                       " -JOIN "     ,is0_join,
                       " -THRESHOLD ", is0_thresh, 
                       " -EDGE 0")
                ,intern = TRUE)
  cat(":: run seed conversion...\n")
  
  # import segments into SAGA
  ret <- system(paste0(sagaCmd, "  io_gdal 3 ",
                       " -FILES "     ,path_run,"treeSeeds.shp",
                       " -GEOM_TYPE 0")
                ,intern = TRUE)
  

  # rasterize vector segments 
  ret <- system(paste0(sagaCmd, " grid_gridding 0 ",
                       " -INPUT "     ,path_run,"treeSeeds.shp",
                       #" -GRID "     ,path_run,"treeSeeds.sgrd",
                       " -OUTPUT 0",
                       " -MULTIPLE 1",
                       " -GRID_TYPE 2",
                       " -TARGET_DEFINITION 0",
                       " -GRID ",path_run,"treeSeeds.sgrd",
                        " -TARGET_USER_SIZE ", raster::res(x)[1],
                        " -TARGET_USER_XMIN ",x@extent[1],
                        " -TARGET_USER_XMAX ",x@extent[2],
                        " -TARGET_USER_YMIN ",x@extent[3],
                        " -TARGET_USER_YMAX ",x@extent[4],
                        " -TARGET_USER_FITS 1",
                         " -COUNT NULL")
                ,intern = TRUE)
  
  # do some crude stuff so adapt the resolution wich is lost during transformation
  ts <- uavRst:::SAGA2R("treeSeeds", ext = extent(x))
  ts <- raster::resample(ts, x, method = 'bilinear')
  
  # create raw zero mask
  treeseeds <- ts * x
  uavRst:::R2SAGA(treeseeds,"treeSeeds")
  
  # reclass extracted seeds to minTreeAlt
  ret <- system(paste0(sagaCmd, "  grid_tools 15 ",
                       " -INPUT "     ,path_run,"treeSeeds.sgrd",
                       " -RESULT "     ,path_run,"seeds.sgrd",
                       " -METHOD 0 ",
                       " -OLD ",minTreeAlt ,
                       " -NEW 0.00000",
                       " -SOPERATOR 1",
                       " -NODATAOPT 0",
                       " -OTHEROPT 0",
                       " -RESULT_NODATA_CHOICE 1 ",
                       " -RESULT_NODATA_VALUE 0.000000")
                ,intern = TRUE)
  
  # TODO SF
  # trees <- sf::st_read(paste0(path_run,"treeSeeds.shp"))

  cat(":: run main segementation...\n")
  # Start final segmentation algorithm as provided by SAGA's seeded Region Growing segmentation (imagery_segmentation 3)
  # TODO sensitivity analysis of the parameters
  ret <- system(paste0(sagaCmd, " imagery_segmentation 3 ",
                       " -SEEDS "   ,path_run,"seeds.sgrd",
                       " -FEATURES '"   ,
                       path_run,"VVI.sgrd;", 
                       path_run,"dsm.sgrd;",
                       path_run,"chm.sgrd'",
                       " -SEGMENTS "   ,path_run,"pre_tree_crowns.shp",
                       " -LEAFSIZE "   ,is3_leafsize,
                       " -NORMALIZE ",is3_normalize,
                       " -NEIGHBOUR ",is3_neighbour, 
                       " -METHOD ",is3_method,
                       " -SIG_1 ",is3_sig1,
                       " -SIG_2 ",is3_sig2,
                       " -THRESHOLD ",is3_threshold),
                intern = TRUE)
  # fill holes inside the crowns (simple approach)
  # TODO better segmentation
  ret <- system(paste0("gdal_sieve.py -8 ",
                       path_run,"pre_tree_crowns.sdat ",
                       path_run,"fpre_tree_crowns.sdat",
                       " -of SAGA"),
                intern = TRUE)
  
  # apply majority filter for smoothing the extremly irregular crown boundaries 
  ret <- system(paste0(sagaCmd, " grid_filter 6 ",
                       " -INPUT ",path_run,"fpre_tree_crowns.sgrd",
                       " -RESULT ",path_run,"tree_crowns.sgrd",
                       " -MODE 0",
                       " -RADIUS ",majority_radius,
                       " -THRESHOLD 0.000000 "),
                intern = TRUE)
  
  # convert filtered crown clumps to shape format for descriptive running statitics 
  ret <- system(paste0(sagaCmd, " shapes_grid 6 ",
                       " -GRID ",path_run,"tree_crowns.sgrd",
                       " -POLYGONS ",path_run,"tree_crowns.shp",
                       " -CLASS_ALL 1",
                       " -CLASS_ID 1.000000",
                       " -SPLIT 1"),
                intern = TRUE)
  
  cat(":: run statitiscs...\n")
  # calculate chm statistics for each crown 
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
  # TODO sf
  ch <- rgdal::readOGR(path_run,"tree_crowns",verbose = FALSE)
  names(ch) <- gsub(names(ch),pattern = "\\NAME",replacement = "NAMEgeom")
  names(ch) <- gsub(names(ch),pattern = "\\ID",replacement = "IDgeom")
  names(ch) <- gsub(names(ch),pattern = "\\VALUE",replacement = "VALUEgeom")
  stats     <- rgdal::readOGR(path_run,"crownsStat",verbose = FALSE)
  ch@data   <- cbind(ch@data,stats@data)
  names(ch) <- gsub(names(ch),pattern = "\\.",replacement = "")
  #ch_s <- gSimplify(ch, 0.1, topologyPreserve=TRUE)
  rgdal::writeOGR(obj = ch, 
                  layer = "treecrowns", 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  cat(":: run post-classification...\n")
  # very simple postclassifcation based on crownarea and morphometric indizies 
  # TODO improve calssification by training 
  trees_crowns <- fa_classifyTreeCrown(crownFn = paste0(path_run,"treecrowns.shp"),  
                                      funNames = c("eccentricityboundingbox","solidity"),
                                      minTreeAlt = minTreeAlt, 
                                      crownMinArea = crownMinArea, 
                                      crownMaxArea = crownMaxArea, 
                                      solidity = solidity, 
                                      WLRatio = WLRatio)
  # write to disk 
  rgdal::writeOGR(obj = ch, 
                  layer = "finalcrowns", 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  
  # pixvalues <- basicExtraction(x = chmR,fN = trees_crowns_2[[2]],responseCat = "ID")
  
  options(warn=0)
  cat("segementation finsihed...")
  return(trees_crowns)
} 
