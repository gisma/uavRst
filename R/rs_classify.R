
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
  sdfAll <- subset(training[sample(1:nrow(training), nsamples), ])
  
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
#'@param trainDataFn default is \code{file.path(tempdir(),"trainingDF.RData")} Name of the extracted training data file
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
                           bnames = NULL,
                           imgFN) {
  require(crayon)
  catNote <- blue $ bold

  
  cat(catNote("\n:::: extract trainPlots data...\n"))
  trainingDF =  data.frame()
  # extract trainPlots Area pixel values
  # TODO https://gis.stackexchange.com/questions/253618/r-multicore-approach-to-extract-raster-values-using-spatial-points
  for (j in 1:length(rasterStack)) {
    cat("\n    extracting trainPlots data from image ",j," of ... ",length(rasterStack))
    
    categorymap<-rgeos::gUnionCascaded(trainPlots[[j]],id=trainPlots[[j]]@data$id)
    dataSet <- raster::extract(rasterStack[[j]], categorymap,df=TRUE)
    names(dataSet)<-append(c("ID"),bnames)
    ## add filename as lloc category
    #FNname<-substr(names(rasterStack[[j]][[1]]),1,nchar(names(rasterStack[[j]][[1]]))-2)
    dataSet$FN= imgFN[[j]]
    dataSet[is.na(dataSet)] <- 0
    dataSet=dataSet[stats::complete.cases(dataSet),]
    
    trainingDF<-rbind(trainingDF, dataSet)
    save(dataSet, file = paste0(path_output,"tmptrain_",j,".RData"))
  }
  
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
#' \dontrun{
#' df1<-getCounts(position = position,
#'               imageFiles = imageFiles,
#'               dropChars = 8,
#'               pre=pre,
#'               ext=".tif")
#'}

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
#' @examples
#' 
# predictRGB(imageFiles=imagestack,
#             model = model_final,
#             in_prefix = "index_",
#             out_prefix = "classified_",
#             bandNames = c("R","G","B","A","VARI","NDTI","TGI","GLI","NGRDI")) 

predictRGB <- function(imageFiles=NULL,
                       model = NULL,
                       in_prefix = "index_",
                       out_prefix = "classified_",
                       bandNames = c("R","G","B","A","VARI","NDTI","TGI","GLI","NGRDI","GLAI","Mean","Variance","Skewness","Kurtosis")) {
  
  cat("\n::: start prediction aka classifikation...\n")
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel	(cl)
  foreach::foreach(i = 1:length(imageFiles),po =path_output) %dopar% {
    #for (i in 1:length(imageFiles)) {
    require(raster)  
    require(randomForest)
    require(caret)
    #TODO rasterstack
    fn<-basename(imageFiles[i])
    fnOut <- paste0(po,out_prefix,fn)
    
    img2predict<-raster::stack(imageFiles[i])
    names(img2predict)<-bandNames
    predictImg<- raster::predict(img2predict,
                                 model,
                                 progress= "text")
    raster::writeRaster(predictImg, filename = fnOut, overwrite = TRUE)
  }
  parallel::stopCluster(cl)
}

#' forward feature selection based random forest model training 
#' @description ffsTrainModel is a wrapper function for a simple use of the forwatrd feature sselection approach 
#' of training random forest classification models. This validation is particulary suitable for
#' leave-location-out cross validations where variable selection
#' MUST be based on the performance of the model on the hold out station.
#' See \href{https://doi.org/10.1016/j.envsoft.2017.12.001}{Meyer et al. (2018)}
#' for further details.
#' This is in fact the case while using time space variable vegetation patterns for classification purposes. 
#' For the uav based RGB/NIR imagery it provides an optimized preconfiguration for the classification goals.
#' 
#' @param trainingDF    dataframe containing training data
#' @param runtest       logical default is false, if set a external validation will be performed
#' @param predictors    vector of predictor names as given by the header of the training data table
#' @param response      name of response variable as given by the header of the training data table
#' @param spaceVar      name of the spcetime splitting vatiable as given by the header of the training data table
#' @param names         all names of the dataframe header 
#' @param noLoc         number of locations to leave out usually nuber of dicrete trainings locations/images
#' @param metric        accuracy metrics for ffs for classification  default is \code{"kappa"}
#' @param pVal          used part of the training data  default is \code{ 0.5}
#' @param prefin        name pattern used for model default is \code{"final_"}
#' @param preffs        name pattern used for ffs default is \code{"ffs_"}
#' @param modelSaveName name pattern used for saving the model default is \code{"model.RData" }
#' @param nrclu         number of cluster to be used
#' 
#' @export ffsTrainModel
#' @examples  
#' #' \dontrun{
#' result<-  ffsTrainModel(trainingDF =trainingDF,
#'                      predictors   = c("R","G","B"),
#'                      response     = "ID",
#'                      spaceVar     = "FN",
#'                      names        = c("ID","R","G","B","A","FN"), 
#'                      noLoc        = length(imageTrainFiles),
#'                      metric_ffs   = "kappa",
#'                      pVal         = 0.5) 
#'                  }

ffsTrainModel<-function(   trainingDF   = NULL,
                        predictors   = c("R","G","B"),
                        response     = "ID",
                        spaceVar     = "FN",
                        names        = c("ID","R","G","B","A","FN"),
                        noLoc        = 3,
                        sumFunction  = "twoClassSummary",
                        pVal         = 0.5,
                        prefin       ="final_",
                        preffs       ="ffs_",
                        modelSaveName="model.RData" ,
                        runtest      = FALSE,
                        noClu = 3) {
  
  # if (tuneThreshold) summaryFunction = "fourStats"
  # if (!tuneThreshold) summaryFunction = "twoClassSummary"
  # if (type=="regression")summaryFunction ="defaultSummary"
  
  # create subset according to pval
  trainIndex<-caret::createDataPartition(trainingDF$ID, p = pVal, list=FALSE)
  data_train <- trainingDF[ trainIndex,]
  if (runtest) data_test <- trainingDF[-trainIndex,]
  # create llo 
  spacefolds <- CAST::CreateSpacetimeFolds(x        = data_train,
                                           spacevar = spaceVar,
                                           k        = noLoc, # number of CV
                                           seed     = 100)
  
  if (length(unique(eval(parse(text=paste("data_train$",response,sep = ""))))) > 2) metric = "Kappa" 
  else metric = "ROC" 
  
  # define control values for ROC (two categorical variables like green and no green) 
  if (metric=="ROC")
    ctrl <- caret::trainControl(method="cv",
                                savePredictions = TRUE,
                                verbose         = TRUE,
                                index           = spacefolds$index,
                                indexOut        = spacefolds$indexOut,
                                returnResamp    = "all",
                                classProbs      = TRUE,
                                summaryFunction = eval(parse(text=sumFunction)))
  # define control values for Kappa (more than two categorical variables like green and no green) 
  else if (metric=="Kappa")
    ctrl <- caret::trainControl(method          ="cv",
                                savePredictions = TRUE,
                                verbose         = TRUE,
                                index           = spacefolds$index,
                                indexOut        = spacefolds$indexOut,
                                returnResamp    = "all",
                                classProbs      = TRUE)
  
  # make it paralel
  cl <- parallel::makeCluster(noClu)
  doParallel::registerDoParallel(cl)  
  # run forward feature selection 
  ffs_model <- ffs(predictors = data_train[,predictors],
                   response   = eval(parse(text=paste("data_train$",response,sep = ""))),
                   method     = "rf",
                   metric     = metric,
                   trControl  = ctrl,
                   withinSE   = TRUE, 
                   tuneGrid   = expand.grid(mtry = 2)
  )
  
  # take resulting predictors 
  predictors <- data_train[,names(ffs_model$trainingData)[-length(names(ffs_model$trainingData))]]
  # and run final tuning 
  model_final <- train(predictors,
                       data_train[,response],
                       method = "rf",
                       metric=metric,
                       returnResamp = "all",
                       importance =TRUE,
                       tuneLength = length(predictors),
                       trControl = ctrl)
  parallel::stopCluster(cl)
  
  return(list(ffs_model,model_final))
}

#' create name vector corresponding to the training image stack 
#' 
#' @param rgbi default is  NA
#' @param haratxt band names 
#' @param  stat band names 
#' @param morpho band names 
#' @param edge band names 
#' 
#' @export makebNames 

makebNames <- function(rgbi    = NA,
                       haratxt = NA,
                       stat    = FALSE,
                       morpho  = NA,
                       edge    = NA ,
                       RGBtrans=NA){
  
  if (!is.na(rgbi[1])) bnames <- append(c("red","green","blue"),rgbi)
  if (!is.na(haratxt)) {
    if(haratxt == "simple"){
      bnames <- c("Energy", "Entropy", "Correlation", 
                  "Inverse_Difference_Moment", "Inertia", 
                  "Cluster_Shade", "Cluster_Prominence",
                  "Haralick_Correlation")
    } else if(haratxt == "advanced"){
      bnames <- c("Hara_Mean", "Hara_Variance", "Dissimilarity",
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
                  "Hara_Mean", "Hara_Variance", "Dissimilarity",
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
  }
  if (stat == TRUE)  {
    bnames    = c("Stat_Mean","Stat_Variance", "Skewness", "Kurtosis")
  } 
  
  if (!is.na(morpho))  {
    bnames    =  morpho
  } 
  
  if (!is.na(edge))  {
    bnames    =  edge
  } 
  if (!is.na(RGBtrans))  {
    bnames    =  RGBtrans
  } 
  return(bnames)
  
}

