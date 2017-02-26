if (!isGeneric('basicClassification')) {
  setGeneric('basicClassification', function(x, ...)
    standardGeneric('basicClassification'))
}

basicClassification<-function(rasterLayer=c("b1","b2","b3","RI","CI","BI"),trainingfN){
  
  # put all raster in a brick
  img<-NULL
  
  files<-paste0(rasterLayer,".tif")
  img<- brick(lapply(files, raster))
  writeraster(img,filename = "brick.tif",overwrite=TRUE)
  # read training data
  trainData <- shapefile(trainingfN)
  sp::proj4string(trainData)<- CRS("+proj=longlat +datum=WGS84 +no_defs")
  # define response param
  responseCol <- "class"
  
  #extract training Area pixel values
  dfAll = data.frame(matrix(vector(), nrow = 0, ncol = length(names(img)) + 1))   
  for (i in 1:length(unique(trainData[[responseCol]]))){                          
    category <- unique(trainData[[responseCol]])[i]
    categorymap <- trainData[trainData[[responseCol]] == category,]
    dataSet <- extract(img, categorymap)
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfAll <- rbind(dfAll, df)
  }
  
  # split training and test data
  inTraining <- createDataPartition(dfAll$class, p = .75, list = FALSE)
  training <- dfAll[ inTraining,]
  testing  <- dfAll[-inTraining,]
  
  nsamples <- 10000
  sdfAll <- subset(taining[sample(1:nrow(training), nsamples), ])
  
  # model training  
  modFit_rf <- train(as.factor(class) ~ b1 + b2 + b3 + RI + CI + BI , 
                     method = "rf", 
                     trControl=trainControl(method="repeatedcv", 
                                            number=10, 
                                            repeats=5, 
                                            selectionFunction = "oneSE"),
                     data = sdfAll)
  # classify  
  beginCluster()
  preds_rf <- clusterR(img, raster::predict, args = list(model = modFit_rf))
  endCluster() 
  writeRaster(preds_rf,filename = "~/predict2.tif",overwrite=TRUE)
  
}

# calculate gdal derived DEM params
gdalDEMParam<- function(dem,fn,item=NULL) {
  if (is.null(item)){
    items<-c("slope", "aspect","TRI","TPI","Roughness")
  }
  s<-raster(fn)
  y<-yres(s)
  x<-xres(s)
  gdalwarp(dem,'dem2.tif',te=paste(extent(s)[1],' ',extent(s)[3],' ',extent(s)[2],' ',extent(s)[4]), tr=paste(x,' ', y),overwrite = TRUE, multi = TRUE)
  
  for (item in items){
    gdaldem(item,"dem2.tif",paste0(item,".tif"))
  }
}

# Split rgb
gdalsplit<-function(fn){
  directory<-dirname(fn)  
  for (i in seq(1:3)){
    gdal_translate(fn,paste0(directory,"/b",i,".tif"),b=i)
  }
}


# calculate orfeo radiometric indices
otbRadiometricIndices<- function(fn,module,list=c("Soil:RI","Soil:CI","Soil:BI")){
  module<-"otbcli_RadiometricIndices"
  directory<-dirname(fn)
  output<-paste0(directory, "/otbOut.tif")
  command<-module
  command<-paste(command, "-in ", fn)
  command<-paste(command, "-out ", output)
  command<-paste(command, "-ram 4096")
  command<-paste(command, "-channels.blue 1")
  command<-paste(command, "-channels.green 2")
  command<-paste(command, "-channels.red 3")
  command<-paste(command, "-list ",as.character(list))
  system(command)  
  i=1
  for (item in list){
    cat(item)
    gdal_translate(output, paste0(directory,"/",substr(item,6,8),".tif"), b = i)
    i<-i+1
  }
}

# calculate orfeo HaralickTextureExtraction
otbHaraTex<- function(fn,module="otbcli_HaralickTextureExtraction",param=c("haraTex","4096",2,2,1,1,0,255,8,"higher")){
  directory<-dirname(fn)
  output<-paste0(directory, "/",param[1],".tif")
  command<-module
  command<-paste(command, "-in ", fn)
  command<-paste(command, "-out ", output)
  command<-paste(command, "-ram ",param[2])
  command<-paste(command, "-parameters.xrad ",param[3])
  command<-paste(command, "-parameters.yrad ",param[4])
  command<-paste(command, "-parameters.xoff ",param[5])
  command<-paste(command, "-parameters.yoff ",param[6])
  command<-paste(command, "-parameters.min ",param[7])
  command<-paste(command, "-parameters.max ",param[8])
  command<-paste(command, "-parameters.nbbin ",param[9])
  command<-paste(command, "-texture ",param[10])
  system(command)  
}


