getSpatialLASInfo <- function(lasFN){
  
  ret <- linkGRASS7(spatial_params = c(-180,-90,180,90,"+proj=longlat +datum=WGS84 +no_defs"))
  ret <- rgrass7::execGRASS("r.in.lidar",
                            flags = c("p"),
                            input=paste0(lasFN),
                            intern=TRUE)
  spatial_params<- list() 
  tmp <- grep(pattern = "Min X Y", ret, value = TRUE)
  tmp <- unlist(strsplit(tmp, ":"))
  tmp <- unlist(strsplit(stringr::str_trim(tmp[2]), " "))
  spatial_params[1] <- tmp[1]
  spatial_params[2] <- tmp[2]
  tmp <- grep(pattern = "Max X Y", ret, value = TRUE)
  tmp <- unlist(strsplit(tmp, ":"))
  tmp <- unlist(strsplit(stringr::str_trim(tmp[2]), " "))
  spatial_params[3] <- tmp[1]
  spatial_params[4] <- tmp[2]
  spatial_params[5] <- grep(pattern = "+proj", ret, value = TRUE)
  
  return(unlist(spatial_params))
}


G2Tiff <- function (runDir=NULL,layer=NULL){
  
  rgrass7::execGRASS("r.out.gdal",
                     flags=c("c","overwrite","quiet"),
                     createopt="TFW=YES,COMPRESS=LZW",
                     input=layer,
                     output=paste0(runDir,"/",layer,".tif")
  )
}

Tiff2G <- function (runDir=NULL,layer=NULL){
  rgrass7::execGRASS('r.external',
                     flags=c('o',"overwrite","quiet"),
                     input=paste0(layer,".tif"),
                     output=layer,
                     band=1
  )
}

OGR2G <- function (runDir=NULL,layer=NULL){
  # import point locations to GRASS
  rgrass7::execGRASS('v.in.ogr',
                     flags=c('o',"overwrite","quiet"),
                     input=paste0(layer,".shp"),
                     output=layer
  )
}

G2OGR <- function (runDir=NULL,layer=NULL){
  rgrass7::execGRASS("v.out.ogr",
                     flags=c("overwrite","quiet"),
                     input=layer,
                     type="line",
                     output=paste0(layer,".shp")
  )
}

