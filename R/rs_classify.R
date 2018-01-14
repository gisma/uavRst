if (!isGeneric('rs_basicClassify')) {
  setGeneric('rs_basicClassify', function(x, ...)
    standardGeneric('rs_basicClassify'))
}

rs_basicClassify<-function(rasterLayer=c("b1","b2","b3","RI","CI","BI"),trainingfN){
  
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

if (!isGeneric('extractTrainData')) {
  setGeneric('extractTrainData', function(x, ...)
    standardGeneric('extractTrainData'))
}

#'@name extractTrainData
#'@title extracts training data from a raster stack
#'
#'@description
#' extracts training data from a raster stack
#'
#'@author Chris Reudenbach
#'
#'@param rasterStack  default is \code{NULL} rasterstack wcontaining all image data
#'@param trainPlots default is \code{NULL}  sp object providing training geometries
#'@param ids default is \code{c(1,2)} classification ids 
#'@param idLabel default is \code{c("yes","no")} names of ids
#'@param trainDataFn default is \code{filepath(temp(),"trainingDF.RData")} Name of the extracted training data file
#'

#'@return extractTrainData returns a dataframe with all training data
#'
#'@export extractTrainData
#'@examples
#'\dontrun{
#'
#' trainingDF <- extractTrainData(rasterStack  = trainStack,
#'                                training     = training,
#'                                ids=c(1,2),
#'                                idLabel= c("green","nogreen")) 
#'}
#'


extractTrainData<-function(rasterStack  = NULL,
                           trainPlots     = NULL,
                           ids=c(1,2),
                           idLabel= c("green","nogreen"),
                           trainDataFn=filepath(temp(),"trainingDF.RData")) {
  
  cat("\n::: extract trainPlots data...\n")
  trainingDF =  data.frame()
  #extract trainPlots Area pixel values
  for (j in 1:length(rasterStack)) {
    cat("\n  :: extracting trainPlots data from image ",j," of ... ",length(rasterStack))
    load(paste0(path_id,"/bnames_",basename(rasterStack[[j]]@file@name),".RData"))
    categorymap<-rgeos::gUnionCascaded(trainPlots[[j]],id=trainPlots[[j]]@data$id)
    dataSet <- raster::extract(rasterStack[[j]], categorymap,df=TRUE)
    names(dataSet)<-append(c("ID"),bnames)
    #names(dataSet)<-(c("ID",seq (2:length((dataSet)))))
    ## add filename as category
    dataSet$FN= substr(names(rasterStack[[j]][[1]]),1,nchar(names(rasterStack[[j]][[1]]))-2)
    #names(dataSet)<- names
    dataSet[is.na(dataSet)] <- 0
    dataSet=dataSet[complete.cases(dataSet),]
    trainingDF<-rbind(trainingDF, dataSet)
  }
 # save(trainingDF, file = trainDataFn)
  ## reclassify data frame
  for (i in 1:length(ids)){
    trainingDF$ID[trainingDF$ID==i]<-idLabel[i]
  }
  trainingDF$ID <- as.factor(trainingDF$ID)
  
  ## save dataframe
  save(trainingDF, file = trainDataFn)
  return(trainingDF)
}

#' trains model according to trainingdata and image stacks
#' 
#' @param ids ids
#' @param position position
#' @param  imageFiles image files
#' @param out_prefix out prefix string
#' @param ext extension
#' @param path   output path
#' @param dropChars chars to drop
#' 
#' @export getCounts
#' @examples  
#' df1<-getCounts(position = position
#' imageFiles = imageFiles
#' dropChars = 8
#' pre=pre
#' ext=".tif")

getCounts<- function(ids=c(1,2),
                     position=NULL,
                     imageFiles = NULL,
                     buffersize=1.5,
                     out_prefix="classified_index_",
                     ext=".tif",
                     path = path_output,
                     dropChars=0) {
  
  buffers<-rgeos::gBuffer(position,width=buffersize) 
  ex<-data.frame()
  df<- lapply(seq(1:length(position)), function(i) {
    fn<-paste0(path,out_prefix,substr(position[i,]$tree,1,nchar(position[i,]$tree)-dropChars),ext)
    
    if (file.exists(fn)){
      ex <- as.data.frame(unlist(raster::extract(raster(fn) , position[i,]    , buffer=buffersize, df=TRUE)))
      
      idVal<-as.numeric(vector(length = length(ids)))
      for (j in 1:length(ids)){
        idVal[j] <- sum(ex[ex[2] == ids[j],  2])/j
        #nogreen <- table(ex[ex[2] == id2,  ])
      }}
    #  all=nogreen+green  
    #  gr=green/all
    #  no=nogreen/all
    
    return(c(idVal,basename(fn)))
    #return(c("green"=gr,"nogreen"=no,"all"=all, "plot"=basename(imageFiles[i])))
  }
  )
  result <- do.call("rbind", df)
  return(result)
}

#' classify images using raster predict
#' 
#' @param imageFiles imagestacke for classification
#' @param model classification model
#' @param  in_prefix in frefix  string
#' @param out_prefix out prefix string
#' @param bandNames band names 
#' 
#' @export predictRGB 
#' 
# predictRGB(imageFiles=imagestack,
#             model = model_final,
#             in_prefix = "index_",
#             out_prefix = "classified_",
#             bandNames = c("R","G","B","A","VARI","NDTI","TGI","GLI","NGRDI","GLAI","Mean","Variance","Skewness","Kurtosis")) 

predictRGB <- function(imageFiles=NULL,
                       model = NULL,
                       in_prefix = "index_",
                       out_prefix = "classified_",
                       bandNames = c("R","G","B","A","VARI","NDTI","TGI","GLI","NGRDI","GLAI","Mean","Variance","Skewness","Kurtosis")) {
  
  cat("\n::: start prediction aka classifikation...\n")
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  foreach(i = 1:length(imageFiles)) %dopar% {
    #for (i in 1:length(imageFiles)) {
    library(raster)  
    library(randomForest)
    library(caret)
    fn<-basename(imageFiles[i])
    path_out<-substr(dirname(imageFiles[i]),1,nchar(dirname(imageFiles[i]))-3)
    fnOut <- paste0(path_out,"output/",out_prefix,fn)
    
    img2predict<-raster::stack(imageFiles[i])
    names(img2predict)<-bandNames
    predictImg<- raster::predict(img2predict,
                                 model,
                                 progress= "text")
    raster::writeRaster(predictImg, filename = fnOut, overwrite = TRUE)
  }
  stopCluster(cl)
}

#' trains model according to trainingdata and image stacks
#' 
#' @param imageFiles imagestacke for classification
#' @param model classification model
#' @param  in_prefix in frefix  string
#' @param out_prefix out prefix string
#' @param bandNames band names 
#' trainingDF =NULL,
#' @param predictors   default is \code{c("R","G","B")}
#' @param response     default is \code{"ID"}
#' @param spaceVar     default is \code{"FN"}
#' @param names        default is \code{c("ID","R","G","B","A","FN")}
#' @param noLoc        default is \code{3}
#' @param cl_method    default is \code{"rf"}
#' @param metric_ffs   default is \code{"kappa"}
#' @param metric_caret default is \code{ "ROC"}
#' @param pVal         default is \code{ 0.5}
#' @param prefin       default is \code{"final_"}
#' @param preffs       default is \code{"ffs_"}
#' @param modelSaveName default is \code{"model.RData" }
#' 
#' @export trainModel
#' @examples  
#' result<-  trainModel(trainingDF =trainingDF,
#'                      predictors   = c("R","G","B","VARI","NDTI","TGI","GLI","NGRDI","GLAI","Mean","Variance","Skewness","Kurtosis"),
#'                      response     = "ID",
#'                      spaceVar     = "FN",
#'                      names = c("ID","R","G","B","A","VARI","NDTI","TGI","GLI","NGRDI","GLAI","Mean","Variance","Skewness","Kurtosis","FN"),
#'                      noLoc        = length(imageTrainFiles),
#'                      cl_method    = "rf",
#'                      metric_ffs   = "kappa",
#'                      metric_caret = "ROC",
#'                      pVal         = 0.5) 

trainModel<-function(   trainingDF =NULL,
                        predictors   = c("R","G","B"),
                        response     = "ID",
                        spaceVar     = "FN",
                        names        = c("ID","R","G","B","A","FN"),
                        noLoc        = 3,
                        cl_method    = "rf",
                        metric_ffs   = "Kappa",
                        metric_caret = "ROC",
                        pVal         = 0.5,
                        prefin       ="final_",
                        preffs       ="ffs_",
                        modelSaveName="model.RData" ) {
  
  # create subset according to pval
  trainIndex<-caret::createDataPartition(trainingDF$ID, p = pVal, list=FALSE)
  data_train <- trainingDF[ trainIndex,]
  data_test <- trainingDF[-trainIndex,]
  # create llo 
  spacefolds <- CAST::CreateSpacetimeFolds(x=data_train,
                                           spacevar = spaceVar,
                                           k=noLoc, # of CV
                                           seed=100)
  # define control values  
  ctrl <- caret::trainControl(method="cv",
                              savePredictions = TRUE,
                              verbose=TRUE,
                              index=spacefolds$index,
                              indexOut=spacefolds$indexOut,
                              returnResamp = "all")
  # make it paralel
  cl <- makeCluster(3)
  registerDoParallel(cl)  
  ffs_model <- ffs(data_train[,predictors],
                   data_train$ID,
                   method=cl_method,
                   metric=metric_ffs,
                   trControl = ctrl,
                   withinSE=TRUE, 
                   tuneGrid = expand.grid(mtry = 2)
  )
  save(ffs_model,file = paste0(path_result,preffs,saveModelName))
  
  
  predictors <- data_train[,names(ffs_model$trainingData)[-length(names(ffs_model$trainingData))]]
  
  model_final <- train(predictors,
                       data_train[,response],
                       method = cl_method,
                       metric=metric_caret,
                       #returnResamp = "all",
                       importance =TRUE,
                       tuneLength = length(predictors),
                       trControl = ctrl)
  stopCluster(cl)
  
  save(model_final,file = paste0(path_result,prefin,saveModelName) )
  
  return(list(model_ffs,model_final,perf,cstat))
}

makebNames <- function(rgbi    = c("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI"),
                       haratxt = "simple",
                       stat    = c("Mean","Variance", "Skewness", "Kurtosis")){
  rgbi<-append(c("red","green","blue","alpha"),rgbi)
  if(haratxt == "simple"){
    bnames <- c("Energy", "Entropy", "Correlation", 
                "Inverse_Difference_Moment", "Inertia", 
                "Cluster_Shade", "Cluster_Prominence",
                "Haralick_Correlation")
  } else if(haratxt == "advanced"){
    bnames <- c("Mean", "Variance", "Dissimilarity",
                "Sum_Average", 
                "Sum_Variance", "Sum_Entropy", 
                "Difference_of_Variances", 
                "Difference_of_Entropies", 
                "IC1", "IC2")
  } else if(haratxt == "higher"){
    bnames <- c("Short_Run_Emphasis", 
                "Long_Run_Emphasis", 
                "Grey-Level_Nonuniformity", 
                "Run_Length_Nonuniformity", 
                "Run_Percentage", 
                "Low_Grey-Level_Run_Emphasis", 
                "High_Grey-Level_Run_Emphasis", 
                "Short_Run_Low_Grey-Level_Emphasis", 
                "Short_Run_High_Grey-Level_Emphasis", 
                "Long_Run_Low_Grey-Level_Emphasis",
                "Long_Run_High_Grey-Level_Emphasis")
  } else if(haratxt == "all"){
    bnames <- c("Energy", "Entropy", "Correlation", 
                "Inverse_Difference_Moment", "Inertia", 
                "Cluster_Shade", "Cluster_Prominence",
                "Haralick_Correlation",
                "Mean", "Variance", "Dissimilarity",
                "Sum_Average", 
                "Sum_Variance", "Sum_Entropy", 
                "Difference_of_Variances", 
                "Difference_of_Entropies", 
                "IC1", "IC2",
                "Short_Run_Emphasis", 
                "Long_Run_Emphasis", 
                "Grey-Level_Nonuniformity", 
                "Run_Length_Nonuniformity", 
                "Run_Percentage", 
                "Low_Grey-Level_Run_Emphasis", 
                "High_Grey-Level_Run_Emphasis", 
                "Short_Run_Low_Grey-Level_Emphasis", 
                "Short_Run_High_Grey-Level_Emphasis", 
                "Long_Run_Low_Grey-Level_Emphasis",
                "Long_Run_High_Grey-Level_Emphasis")
  }
  
  return(append(rgbi,append(stat,bnames)))
  
  
}

