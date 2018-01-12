
## dem function
dem_grass_lidar <- function(path, 
                            inFN, 
                            outFN,
                            grass_lidar_method,
                            res){
  
  ground_raster <- execGRASS("r.in.lidar",
                             input = paste0(path, inFN),
                             output = outFN,
                             flags = c("e", "n", "v", "overwrite","o"),
                             resolution = gridsize,
                             method = grass_lidar_method,
                             class_filter = 2,
                             intern = TRUE,
                             ignore.stderr = TRUE)
  return(ground_raster)
}