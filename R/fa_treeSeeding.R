if (!isGeneric('fa_treeSeeding')) {
  setGeneric('fa_treeSeeding', function(x, ...)
    standardGeneric('fa_treeSeeding'))
}

#'@name fa_treeSeeding
#'@title Tree segmentation based on a CHM
#'
#'@description
#' Tree segmentation based on a CHM
#'
#'@author Chris Reudenbach
#'
#'@param x  spatial raster object
#'@param minTreealt default is 5 
#'@param crownMinArea    default is 3 minimum area of crown
#'@param crownMinArea    default is 225 maximum area of crown
#'@param is0_output      default is 0,     # 0=s seed value 1=segment id
#'@param is0_join        default is 2,     # 0=no join, 1=seed2saddle diff, 2=seed2seed diff
#'@param is0_thresh      default is 0.05,  # threshold for join difference in m
#'@param majority_radius default is 5.000
#'@param seeding default  is TRUE switch if seeding is called
#'@param split default  is TRUE switch if splitting of the polygons is called

#'@return basically returns a  vector data sets with the tree crown geometries and a bunch of corresponding indices
#'
#'
#'@export fa_treeSeeding
#'@examples
#'\dontrun{
#' # Tree segmentation based on a CHM
#'  fa_treeSeeding(x = rasterobj,  "nameofSAGAFile")
#'}
#'
fa_treeSeeding <- function(x = NULL,
                                  minTreeAlt       = 10,
                                  crownMinArea     = 3,
                                  crownMaxArea     = 150,
                                  is0_output      = 1,     # 0= seed value 1=segment id
                                  is0_join        = 1,     # 0=no join, 1=seed2saddle diff, 2=seed2seed diff
                                  is0_thresh      = 0.10,  # threshold for join difference in m
                                  majority_radius = 5.000,
                                  seeding = TRUE,
                                  split = TRUE
                                  
)  {
  cat("\n:: start crown identification...\n")
  options(warn=-1)
  if (!exists(sagaCmd)) link2GI::linkSAGA()
  r2saga(x,"chm")
  if (seeding){
    cat(":: run pre-segmentation...\n")
    # first segment run is a simple watershed segmentation just for deriving more reliable seeds 
    # TODO improve different advanceds seed finding algorithms
    ret <- system(paste0(sagaCmd, " imagery_segmentation 0 ",
                         " -GRID "     ,path_run,"chm.sgrd",
                         " -SEGMENTS " ,path_run,"dummyCrownSegments.sgrd",
                         " -SEEDS "    ,path_run,"treeSeeds.shp",
                         " -OUTPUT "   ,is0_output, 
                         " -DOWN 1"    , 
                         " -JOIN "     ,is0_join,
                         " -THRESHOLD ",is0_thresh, 
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
    
    # tmp <- rgdal::readOGR(path_run,"dummyCrownSegment",verbose = FALSE)
    # tmp <- tmp[tmp$VALUE >= 0,]
    # tmp@data$area <- rgeos::gArea(tmp,byid = TRUE)
    # tmp <- tmp[tmp$area > crownMinArea,]
    # rgdal::writeOGR(obj    = tmp,
    #                 layer  = "dummyCrownSegment", 
    #                 driver = "ESRI Shapefile", 
    #                 dsn    = path_run, 
    #                 overwrite_layer = TRUE)
    # 
    # 
    cat(":: find max height position...\n")
    dummycrownsStat <- uavRst::xpolystat(c("chm"), spdf ="dummyCrownSegment.shp")
    # rgdal::writeOGR(obj    = polyStat,
    #                 layer  = "polyStat", 
    #                 driver = "ESRI Shapefile", 
    #                 dsn    = path_run, 
    #                 overwrite_layer = TRUE)
    trees_crowns <- fa_basicTreeCrownFilter(crownFn = dummycrownsStat,
                                            minTreeAlt = minTreeAlt,
                                            crownMinArea = crownMinArea,
                                            crownMaxArea = crownMaxArea,
                                            mintreeAltParam = "chmQ10"
    )
    rgdal::writeOGR(obj    = trees_crowns[[2]],
                    layer  = "dummyCrownSegment", 
                    driver = "ESRI Shapefile", 
                    dsn    = path_run, 
                    overwrite_layer = TRUE)
    
    cat(":: find max height position...\n")
    ts <-  poly_extract_maxpos(paste0(path_run,"chm.tif"),"dummyCrownSegment",poly_split = split)
    # create raw zero mask
    seeds <- ts[[1]] * x
    r2saga(seeds,"treeSeeds")
    # extract stats
    
    # reclass extracted seeds to minTreeAlt
    ret <- system(paste0(sagaCmd, "  grid_tools 15 ",
                         " -INPUT "  ,path_run,"treeSeeds.sgrd",
                         " -RESULT " ,path_run,"seeds.sgrd",
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
    # trees <- sf::st_read(paste0(path_run,"treeSeeds.shp"))
  }
  return(raster::raster(paste0(path_run,"seeds.sdat")))
}