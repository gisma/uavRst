if (!isGeneric('xpolystat')) {
  setGeneric('xpolystat', function(x, ...)
    standardGeneric('xpolystat'))
}

#'@name xpolystat
#'@title calculate statitiscs of polygon based raster extraction
#'
#'@description
#' calculate statitiscs of polygon based raster extraction
#'
#'@author Chris Reudenbach
#'
#'@param x  spatial raster object
#'@param minTreealt default is 5 

#'@return returns a spatialpolygon dataframe containing decriptive statistics
#'
#'
#'@export xpolystat
#'@examples
#'\dontrun{
#' # Tree segementation based on a CHM
#' polyStat <- xpolystat(c("chm","dah"),
#'                       spdf = "tree_crowns.shp")
#'}
#'
xpolystat <- function(x = NULL,
                           spdf = NULL,
                           count = 1,
                           min = 1,
                           max = 1,
                           sum = 1,
                           range = 1,
                           mean = 1,
                           var = 1, 
                           stddev = 1,
                           quantile = 10)   {
                                  
cat(":: run statistics...\n")
# calculate chm statistics for each crown 

  for (i in seq(1:length(x))) {
    cat(":: calculate ",x[i], " statistics\n")
    ret <-  system(paste0(sagaCmd, " shapes_grid 2 ",
                          " -GRIDS ",path_run,x[i],".sgrd",
                          " -POLYGONS ",path_run,spdf,
                          " -NAMING 1",
                          " -METHOD 2",
                          " -COUNT ", count,
                          " -MIN  ", min,
                          " -MAX ", max,
                          " -SUM ",sum,
                          " -RANGE ",range,
                          " -MEAN  ", mean,
                          " -VAR ",var,
                          " -STDDEV ",stddev,
                          " -QUANTILE ",quantile,
                          " -PARALLELIZED 1",
                          " -RESULT ",path_run,x[i],"Stat.shp"),
                   intern = TRUE)
    
    stat1 <- rgdal::readOGR(path_run,paste0(x[i],"Stat"), verbose = FALSE)
    names(stat1) <- gsub(names(stat1),pattern = "\\.",replacement = "")
    
    if (i == 1) {
      stat <- stat1
      
    } else  {
      stat@data <- cbind(stat@data,stat1@data[4:length(names(stat1))])
      #stat <- stat1
    }
  }
  rgdal::writeOGR(obj = stat,
                  layer = "polyStat",
                  driver = "ESRI Shapefile",
                  dsn = path_run,
                  overwrite_layer = TRUE)
  return(stat)
}