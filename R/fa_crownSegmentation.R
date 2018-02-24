if (!isGeneric('fa_crown_segmentation')) {
  setGeneric('fa_crown_segmentation', function(x, ...)
    standardGeneric('fa_crown_segmentation'))
}

#'@name fa_crown_segmentation
#'@title Tree segmentation based on a CHM
#'
#'@description
#' Tree segmentation based on a CHM, basically returns a  vector data sets with the tree crown geometries and a bunch of corresponding indices. After the segementation itself the results are hole filled and optionally filtered by a majority filter in the 3*3 surrounding.
#'
#'@author Chris Reudenbach
#'
#'@param seeds  spatial raster object
#'@param is3_leafsize       integer, bin size of grey value sampling range from 1 to 256 default is 8,
#'@param is3_normalize      integer,  logical switch if data will be normalized or not default is 1,
#'@param is3_neighbour      integer,  von Neumanns' neighborhood (0) or Moore's (1) default is 0,
#'@param is3_method         integer, growing algorithm for feature space and position (0) or feature space only (1)
#'@param is3_thVarSpatial   numerical, spatial variance default is  0.05
#'@param is3_thVarFeature  numerical, spatial variance default is  0.05,
#'@param is3_thSimilarity   mumerical similarity threshold default is  0.00005,
#'@param is3_seed_params    vector of characters corresponding with the used attributes default is c("chm") altitude values from surface model
#'@param giLinks            list of GI tools cli pathes  default is NULL
#'@export fa_crown_segmentation
#'@examples
#'\dontrun{
#' # Tree segmentation based on a CHM
#'  fa_crown_segmentation(x = rasterobj,  "nameofSAGAFile")
#'}
#'
fa_crown_segmentation <- function(seeds = "seed.sgrd",
                                  is3_leafsize       = 8,
                                  is3_normalize      = 1,
                                  is3_neighbour      = 0,
                                  is3_method         = 0,
                                  is3_thVarFeature   = 0.05,
                                  is3_thVarSpatial   = 0.05,
                                  is3_thSimilarity   = 0.00005,
                                  is3_seed_params    = c("chm"),
                                  majority_radius    = 2.000,
                                  giLinks = NULL) {
  
  cat("::: run main segmentation...\n")
  # create correct param list s
  #is3_seed_params<-c("HI","GLI")
  if (is.null(giLinks)){
    giLinks <- linkBuilder()
  }
  
  gdal <- giLinks$gdal
  saga <- giLinks$saga
  sagaCmd<-saga$sagaCmd
    
  
    param_list <- paste0(path_run,is3_seed_params,".sgrd;",collapse = "")
  
  # Start final segmentation algorithm as provided by SAGA's seeded Region Growing segmentation (imagery_segmentation 3)
  # TODO sensitivity analysis of the parameters
  ret <- system(paste0(sagaCmd, " imagery_segmentation 3 ",
                       " -SEEDS "    ,path_run,"seed.sgrd",
                       " -FEATURES '", param_list,
                       "' -SEGMENTS ",path_run,"tree_crowns.shp",
                       " -LEAFSIZE " ,is3_leafsize,
                       " -NORMALIZE ",is3_normalize,
                       " -NEIGHBOUR ",is3_neighbour, 
                       " -METHOD "   ,is3_method,
                       " -SIG_1 "    ,is3_thVarFeature,
                       " -SIG_2 "    ,is3_thVarSpatial,
                       " -THRESHOLD ",is3_thSimilarity),
                intern = TRUE)
  
  # fill holes inside the crowns (simple approach)
  # TODO better segmentation
  if (majority_radius > 0){
    outname<- "sieve_pre_tree_crowns.sdat"
    ret <- system(paste0("gdal_sieve.py -8 ",
                         path_run,"tree_crowns.sdat ",
                         path_run,outname,
                         " -of SAGA"),
                  intern = TRUE)
    # apply majority filter for smoothing the extremly irregular crown boundaries 
    ret <- system(paste0(sagaCmd, " grid_filter 6 ",
                         " -INPUT "   ,path_run,"sieve_pre_tree_crowns.sgrd",
                         " -RESULT "  ,path_run,"tree_crowns.sgrd",
                         " -MODE 0",
                         " -RADIUS "  ,majority_radius,
                         " -THRESHOLD 0.0 "),
                  intern = TRUE)
    }
  
  
  # convert filtered crown clumps to shape format 
  ret <- system(paste0(sagaCmd, " shapes_grid 6 ",
                       " -GRID "     ,path_run,"tree_crowns.sgrd",
                       " -POLYGONS " ,path_run,"tree_crowns.shp",
                       " -CLASS_ALL 1" ,
                       " -CLASS_ID 1.0",
                       " -SPLIT 1"),
                intern = TRUE)
  
  
  tree_crowns <- rgdal::readOGR(path_run,"tree_crowns", verbose = FALSE)
  
  options(warn=0)
  cat("segmentation finsihed...\n")
  return( tree_crowns)
} 
