#calculate gdal derived DEM params
gdalDEMParam<- function(dem,item=NULL) {
  if (is.null(item)){
    items<-c("slope", "aspect","TRI","TPI","Roughness")
  }
  # s<-raster(fn)
  # y<-yres(s)
  # x<-xres(s)
  # gdalwarp(dem,'dem2.tif',te=paste(extent(s)[1],' ',extent(s)[3],' ',extent(s)[2],' ',extent(s)[4]), tr=paste(x,' ', y),overwrite = TRUE, multi = TRUE)

  for (item in items){
    gdaldem(item,"dem2.tif",paste0(item,".tif"))
  }
}

# fill holes
fillGaps<- function (folder,layer){
  cat(":: fill data gaps using gdal_fillnodata... \n")
  
  # fill data holes
  if (Sys.info()["sysname"] == "Windows"){
    ret <- system2(command = "gdal_fillnodata.py ",args = 
                     paste0(folder,"/",layer,".tif ",
                            folder,"/",layer,".tif "))
    
  } else {
    ret <- system(paste0("gdal_fillnodata.py ", folder,layer,".tif ",
                         folder,layer,".tif "),intern = TRUE)
  }
  
  # write filled data back to GRASS
  rgrass7::execGRASS('r.in.gdal',  flags=c('o',"overwrite"), input=paste0(folder,"/",layer,".tif"),  output=layer, band=1)
}


# creates names and ranges from a simple list for zrange cuts
makenames<-function(zr ) {
  class<-list()
  zrange<-list()
  for ( i in 1:(length(zr[[1]]))) {
    if (i == length(zr[[1]])){
      class[[i]]<-c(paste0('class',zr[[1]][1],zr[[1]][length(zr[[1]])]))
      zrange[[i]]<-c(zr[[1]][1], zr[[1]][length(zr[[1]])])  
    } else {
      class[[i]]<-c(paste0('class',zr[[1]][i],zr[[1]][i+1]))
      zrange[[i]]<-c(zr[[1]][i],zr[[1]][i+1])
    }
  }
  return(list(unlist(class),zrange))
}

extractTrainPixelValues<- function(imgStack=NULL,trainData=NULL,responseCol=NULL){
  #extract training Area pixel values
  dfTpv = data.frame(matrix(vector(), nrow = 0, ncol = length(names(imgStack)) + 1))   
  for (i in 1:length(unique(trainData[[responseCol]]))){
    category <- unique(trainData[[responseCol]])[i]
    cat("\n extracting cat: ",levels(category)[i]," no: ",i," of: ",length(unique(trainData[[responseCol]])))
    categorymap <- trainData[trainData[[responseCol]] == category,]
    dataSet <- raster::extract(imgStack, categorymap)
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfTpv <- rbind(dfTpv, df)
  }
  names(dfTpv)<-gsub(names(dfTpv),pattern = "\\.",replacement = "_")
  return(dfTpv)
}

# calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}