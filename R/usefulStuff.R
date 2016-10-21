getMinMaxG <- function (layer=NULL){
  r.info<-rgrass7::execGRASS('r.info', flags=c("r","quiet"), map=layer,intern=TRUE)
  min <- as.numeric(unlist(strsplit(r.info[1], split='=', fixed=TRUE))[2])
  max <- as.numeric(unlist(strsplit(r.info[2], split='=', fixed=TRUE))[2])
  return(c(min,max))

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


accuCalc <- function (envGIS,dem="dem",cost="cost",currentP=NULL,lambda=0.5,memory=4000,dump=FALSE,walk=walk){
  cat ('calculate accu cost',unlist(currentP),"\n")
  rgrass7::execGRASS("r.cost",
                     flags=c("overwrite","quiet"),
                     parameters=list(input = cost,
                                     outdir="accudir",
                                     output="accu",
                                     start_coordinates = as.numeric(unlist(currentP)),
                                     memory=8000)
  )
  if (dump) {G2Tiff(runDir=envGIS$runDir,layer="accu")}

  if (walk) {
    cat ('calculate walk cost',unlist(currentP),"\n")
    rgrass7::execGRASS("r.walk",
                       flags=c("overwrite","quiet"),
                       elevation="dem",
                       friction="cost",
                       outdir="walkdir",
                       output="walk",
                       start_coordinates=as.numeric(unlist(currentP)),
                       lambda=0.5
    )
    if (dump) {G2Tiff(runDir=envGIS$runDir,layer="walk")}
  }
}

fix.encoding <- function(df, originalEncoding = "latin1",newEncoding = "utf8") {
  numCols <- ncol(df)
  for (col in 1:numCols) {
    df[, col]<-as.character(df[, col])
    Encoding(df[, col]) <- originalEncoding
    df[, col] <- iconv(
      df[, col],
      originalEncoding,
      newEncoding
    )
  }
  return(df)
}


### optional transformation to Albert equal area
### TODO make the central meridian dynamical
#   mosaicSRTM<- gdalwarp(paste0(rootDir, "/srtm/cMosaicSRTM.tif"),
#                         paste0(rootDir, "/srtm/cpMosaicSRTM.tif"),
#                         t_srs='+proj=aea +lat_1=15 +lat_2=65 +lat_0=30 +lon_0=95 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs',
#                         output_Raster = TRUE,
#                         overwrite= TRUE,
#                         verbose=TRUE
#   )