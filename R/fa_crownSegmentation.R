if (!isGeneric('fa_crown_segmentation')) {
  setGeneric('fa_crown_segmentation', function(x, ...)
    standardGeneric('fa_crown_segmentation'))
}

#'@name fa_crown_segmentation
#'@title Tree segementation based on a CHM
#'
#'@description
#' Tree segementation based on a CHM
#'
#'@author Chris Reudenbach
#'
#'@param x  spatial raster object
#'@param minTreealt default is 5 
#'@param is0_output      default is 0,     # 0=s seed value 1=segment id
#'@param is0_join        default is 2,     # 0=no join, 1=seed2saddle diff, 2=seed2seed diff
#'@param is0_thresh      default is 0.05,  # threshold for join difference in m
#'@param is3_leafsize    default is 8,
#'@param is3_normalize   default is 1,
#'@param is3_neighbour   default is 1,
#'@param is3_method      default is 0,
#'@param is3_sig1        default is  0.1,
#'@param is3_sig2        default is 3.01,
#'@param is3_threshold   default is 0.001,
#'@param is3_seed_params default is c("GLI","HI") rgb image derived indices
#'@param majority_radius default is 5.000
#'@param seeding default  is TRUE switch if seeding is called
#'@param split default  is TRUE switch if splitting of the polygons is called

#'@return basically returns a  vector data sets with the tree crown geometries and a bunch of corresponding indices
#'
#'
#'@export fa_crown_segmentation
#'@examples
#'\dontrun{
#' # Tree segementation based on a CHM
#'  fa_crown_segmentation(x = rasterobj,  "nameofSAGAFile")
#'}
#'
fa_crown_segmentation <- function(x = NULL,
                                  minTreeAlt = 5,
                                  crownMinArea = 3,
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
                                   is3_seed_params = c("GLI","HI"),
                                   majority_radius = 5.000,
                                  seeding = TRUE,
                                  split = TRUE
                                   
)  {
  cat("\n:: start crown identification...\n")
  options(warn=-1)
  if (!exists(sagaCmd)) link2GI::linkSAGA()
  uavRst:::R2SAGA(x,"chm")
  if (seeding){
  cat(":: run seed finding...\n")
  # first segment run is a simple watershed segementation just for deriving more reliable seeds 
  # TODO improve different advanceds seed finding algorithms
  ret <- system(paste0(sagaCmd, " imagery_segmentation 0 ",
                       " -GRID "     ,path_run,"chm.sgrd",
                       " -SEGMENTS " ,path_run,"dummyCrownSegments.sgrd",
                       " -SEEDS "    ,path_run,"treeSeeds.shp",
                       " -OUTPUT "   ,is0_output, 
                       " -DOWN 1"    , 
                       " -JOIN "     ,is0_join,
                       " -THRESHOLD ", is0_thresh, 
                       " -EDGE 0")
                ,intern = TRUE)
  
  # convert filtered crown clumps to shape format for descriptive running statitics 
  ret <- system(paste0(sagaCmd, " shapes_grid 6 ",
                       " -GRID ",path_run,"dummyCrownSegments.sgrd",
                       " -POLYGONS ",path_run,"dummyCrownSegment.shp",
                       " -CLASS_ALL 1",
                       " -CLASS_ID 1.000000",
                       " -SPLIT 1"),
                intern = TRUE)
  tmp <- rgdal::readOGR(path_run,"dummyCrownSegment",verbose = FALSE)
  tmp <- tmp[tmp$VALUE >= 0,]
  tmp@data$area <- rgeos::gArea(tmp,byid = TRUE)
  tmp <- tmp[tmp$area > crownMinArea,]
  rgdal::writeOGR(obj = tmp,
                  layer = "dummyCrownSegment", 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  
  cat(":: find max height position...\n")
  ts <-  uavRst:::get_max_posFromPoly(paste0(path_run,"chm.tif"),"dummyCrownSegment",poly_split = split)
  
  # create raw zero mask
  seeds <- ts[[1]] * x
  uavRst:::R2SAGA(seeds,"treeSeeds")
  
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
  }
  
  cat(":: run main segementation...\n")
  # create correct param list s
  param_list <- paste0(path_run,is3_seed_params,".sgrd;")

  # Start final segmentation algorithm as provided by SAGA's seeded Region Growing segmentation (imagery_segmentation 3)
  # TODO sensitivity analysis of the parameters
  ret <- system(paste0(sagaCmd, " imagery_segmentation 3 ",
                       " -SEEDS "   ,path_run,"seeds.sgrd",
                       " -FEATURES '"   , param_list,
                       "' -SEGMENTS "   ,path_run,"pre_tree_crowns.shp",
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
  

  tree_crowns <- rgdal::readOGR(path_run,"tree_crowns", verbose = FALSE)

  options(warn=0)
  cat("segementation finsihed...")
  return( tree_crowns)
} 
