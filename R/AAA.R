.onLoad <- function(libname = find.package("uavRst"), pkgname = "uavRst"){
  
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1") 
    # get rid of NOTEs in R CMD check for "undefined global functions or variables"
    utils::globalVariables(
    c("path_data","path_fu","path_id",
      "path_OTB","path_output","path_run","path_tmp","sagaCmd","projRootDir","zipfn","k","v")    
    
    )
  
  invisible()
}

requireNamespace(c("osmar", "randomForest" ,"raster" ,"sp","sf","rgrass7","tools"))
