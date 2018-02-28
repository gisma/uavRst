
rs_basicClassify<-function(rasterLayer=c("b1","b2","b3","RI","CI","BI"),trainingfN){
  
  # put all raster in a brick
  img<-NULL
  
  files<-paste0(rasterLayer,".tif")
  img<- brick(lapply(files, raster))
  raster::writeRaster(img,filename = "brick.tif",overwrite=TRUE)
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
  sdfAll <- subset(training[sample(1:nrow(training), nsamples), ])
  
  # model training  
  modFit_rf <- caret::train(as.factor(class) ~ b1 + b2 + b3 + RI + CI + BI , 
                            method = "rf", 
                            trControl=caret::trainControl(method="repeatedcv", 
                                                          number=10, 
                                                          repeats=5, 
                                                          selectionFunction = "oneSE"),
                            data = sdfAll)
  # classify  
  beginCluster()
  preds_rf <- clusterR(img, raster::predict, args = list(model = modFit_rf))
  endCluster() 
  raster::writeRaster(preds_rf,filename = "~/predict2.tif",overwrite=TRUE)
  
}

