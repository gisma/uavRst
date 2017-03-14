getSpatialLASInfo <- function(lasinfo,lasFN){
  
  #ret <- linkGRASS7(spatial_params = c(-180,-90,180,90,"+proj=longlat +datum=WGS84 +no_defs"))
  #ret <- rgrass7::execGRASS("r.in.lidar",
  #                          flags = c("p"),
  #                          input=paste0(lasFN),
  #                          intern=TRUE)
  ret <- system(paste0(lasinfo,
                       " -i ",lasFN,
                       " -no_check  -stdout"),intern = TRUE)
  
  spatial_params<- list() 
  
  tmp <- grep(pattern = "min x y z", ret, value = TRUE)
  tmp <- unlist(strsplit(tmp, ":"))
  tmp <- unlist(strsplit(stringr::str_trim(tmp[2]), " "))
  spatial_params[1] <- tmp[1]
  spatial_params[2] <- tmp[2]
  tmp <- grep(pattern = "max x y z", ret, value = TRUE)
  tmp <- unlist(strsplit(tmp, ":"))
  tmp <- unlist(strsplit(stringr::str_trim(tmp[2]), " "))
  spatial_params[3] <- tmp[1]
  spatial_params[4] <- tmp[2]
  #spatial_params[5] <- grep(pattern = "+proj", ret, value = TRUE)
  
  return(unlist(spatial_params))
}


