if (!isGeneric('fa_findTreePosition')) {
  setGeneric('fa_findTreePosition', function(x, ...)
    standardGeneric('fa_findTreePosition'))
}

#'@name fa_findTreePosition
#'@title Find potential tree positions using a canopy height model
#'
#'@description
#' Find potential tree positions using a canopy height model using a iterative watershed algorithm 
#'
#'@author Chris Reudenbach
#'
#'@param chm  spatial raster object
#'@param minTreeAlt default is 5 
#'@param mintreeAltParam default is "chmQ20"
#'@param minCrownArea    default is 3 minimum area of crown
#'@param maxCrownArea    default is 225 maximum area of crown
#'@param output      default is 0,     # 0=s treePos value 1=segment id
#'@param join        default is 2,     # 0=no join, 1=treePos2saddle diff, 2=treePos2treePos diff
#'@param thresh      default is 0.05,  # threshold for join difference in m
#'@param split default  is TRUE switch if splitting of the polygons is called
#'

#'@return basically returns a  vector data sets with the tree crown geometries and a bunch of corresponding indices
#'
#'
#'@export fa_findTreePosition
#'@examples
#'\dontrun{
#' # Tree segmentation based on a CHM
#'  fa_findTreePosition(chm = rasterobj,  "nameofSAGAFile")
#'}
#'
fa_findTreePosition <- function(chm = NULL,
                                  minTreeAlt       = 10,
                                  minTreeAltParam  = "chmQ20",
                                  minCrownArea     = 3,
                                  maxCrownArea     = 150,
                                  output      = 1,     # 0= treePos value 1=segment id
                                  join        = 1,     # 0=no join, 1=treePos2saddle diff, 2=treePos2treePos diff
                                  thresh      = 0.10,  # threshold for join difference in m
                                  split = TRUE,
                                  giLinks = NULL
                                  
)  {
  cat("\n:: start crown identification...\n")
  options(warn=-1)
  
  if (is.null(giLinks)){
    giLinks <- linkBuilder()
  }
  
  gdal <- giLinks$gdal
  saga <- giLinks$saga
  sagaCmd<-saga$sagaCmd
  raster::writeRaster(chm,paste0("chm.sdat"),overwrite = TRUE,NAflag = 0)
  raster::writeRaster(chm,paste0("chm.tif"),overwrite = TRUE,NAflag = 0)
  #r2saga(chm,"chm")

    cat(":: run pre-segmentation...\n")
    # first segment run is a simple watershed segmentation just for deriving more reliable treePoss 
    # TODO improve different advanceds treePos finding algorithms
    ret <- system(paste0(sagaCmd, " imagery_segmentation 0 ",
                         " -GRID "     ,path_run,"chm.sgrd",
                         " -SEGMENTS " ,path_run,"dummyCrownSegments.sgrd",
                         " -SEEDS "    ,path_run,"treePos.shp",
                         " -OUTPUT "   ,output, 
                         " -DOWN 1"    , 
                         " -JOIN "     ,join,
                         " -THRESHOLD ",thresh, 
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
    
    # 
    cat(":: find max height position...\n")
    dummycrownsStat <- uavRst::xpolystat(c("chm"), spdf ="dummyCrownSegment.shp")

    trees_crowns <- fa_basicTreeCrownFilter(crownFn = dummycrownsStat,
                                            minTreeAlt = minTreeAlt,
                                            minCrownArea = minCrownArea,
                                            maxCrownArea = maxCrownArea,
                                            mintreeAltParam = minTreeAltParam 
    )
    rgdal::writeOGR(obj    = trees_crowns[[2]],
                    layer  = "dummyCrownSegment", 
                    driver = "ESRI Shapefile", 
                    dsn    = path_run, 
                    overwrite_layer = TRUE)
    
    cat(":: find max height position...\n")
    ts <-  extractMaxPosPoly(paste0(path_run,"chm.tif"),"dummyCrownSegment",poly_split = split)
    # create raw zero mask
    treePos <- ts[[1]] * chm
    raster::writeRaster(treePos,"treePos0.sdat",overwrite = TRUE,NAflag = 0)
    #r2saga(treePos,"treePos0")
    # extract stats
    
    # reclass extracted treePoss to minTreeAlt
    ret <- system(paste0(sagaCmd, "  grid_tools 15 ",
                         " -INPUT "  ,path_run,"treePos0.sgrd",
                         " -RESULT " ,path_run,"treePos.sgrd",
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
    # trees <- sf::st_read(paste0(path_run,"treePos.shp"))
 
  return(raster::raster(paste0(path_run,"treePos.sdat")))
}