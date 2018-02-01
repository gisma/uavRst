if (!isGeneric('fa_crown_segmentation')) {
  setGeneric('fa_crown_segmentation', function(x, ...)
    standardGeneric('fa_crown_segmentation'))
}

#'@name fa_crown_segmentation
#'@title Tree segmentation based on a CHM
#'
#'@description
#' Tree segmentation based on a CHM
#'
#'@author Chris Reudenbach
#'
#'@param seeds  spatial raster object
#'@param is3_leafsize    default is 8,
#'@param is3_normalize   default is 1,
#'@param is3_neighbour   default is 0,
#'@param is3_method      default is 0,
#'@param is3_sig1        default is  0.05,
#'@param is3_sig2        default is  0.05,
#'@param is3_threshold   default is  0.0001,
#'@param is3_seed_params default is c("GLI","HI","GRV") rgb image derived indices
#'@param seeding default  is TRUE switch if seeding is called
#'@param split default  is TRUE switch if splitting of the polygons is called

#'@return basically returns a  vector data sets with the tree crown geometries and a bunch of corresponding indices
#'
#'
#'@export fa_crown_segmentation
#'@examples
#'\dontrun{
#' # Tree segmentation based on a CHM
#'  fa_crown_segmentation(x = rasterobj,  "nameofSAGAFile")
#'}
#'
fa_crown_segmentation <- function(seeds = "seeds.sgrd",
                                   is3_leafsize    = 8,
                                   is3_normalize   = 1,
                                   is3_neighbour   = 0,
                                   is3_method      = 0,
                                   is3_sig1        = 0.05,
                                   is3_sig2        = 0.05,
                                   is3_threshold   = 0.00005,
                                   is3_seed_params = c("chm"),
                                   majority_radius = 5.000,
                                  seeding = TRUE,
                                  split = TRUE
                                   
)  {

  
  cat(":: run main segmentation...\n")
  # create correct param list s
  #is3_seed_params<-c("HI","GLI")
  
  param_list <- paste0(path_run,is3_seed_params,".sgrd;",collapse = "")

  # Start final segmentation algorithm as provided by SAGA's seeded Region Growing segmentation (imagery_segmentation 3)
  # TODO sensitivity analysis of the parameters
  ret <- system(paste0(sagaCmd, " imagery_segmentation 3 ",
                       " -SEEDS "    ,path_run,"seeds.sgrd",
                       " -FEATURES '", param_list,
                       "' -SEGMENTS ",path_run,"pre_tree_crowns.shp",
                       " -LEAFSIZE " ,is3_leafsize,
                       " -NORMALIZE ",is3_normalize,
                       " -NEIGHBOUR ",is3_neighbour, 
                       " -METHOD "   ,is3_method,
                       " -SIG_1 "    ,is3_sig1,
                       " -SIG_2 "    ,is3_sig2,
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
                       " -INPUT "   ,path_run,"fpre_tree_crowns.sgrd",
                       " -RESULT "  ,path_run,"tree_crowns.sgrd",
                       " -MODE 0",
                       " -RADIUS "  ,majority_radius,
                       " -THRESHOLD 0.0 "),
                intern = TRUE)
  
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
