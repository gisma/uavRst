
#'@name get_traindata
#'@title Extracts training data from a raster stack using vector data as a mask.
#'
#'@description
#' Extracts training data from a raster stack and returns a dataframe containing for each pixel all values.
#'
#'@author Chris Reudenbach
#'
#'@param rasterStack  an object of rasterstack*. containing image data to make prediction on
#'@param trainPlots   an object of SpatialPolygonDataFrame*. providing the training areas

#'@export get_traindata
#'@examples
#'\dontrun{
#'##- required packages
#'require(uavRst)

#'setwd(tempdir())
#'
#'##- get the tutorial data
#'utils::download.file("https://github.com/gisma/gismaData/raw/master/uavRst/data/tutorial_data.zip",
#'                      "tutorial_data.zip")
#'unzip("tutorial_data.zip",exdir =  ".")
#'
#'##- get the files
#'imageTrainStack <- list()
#'geomTrainStack <- list()
#'imageTrainFiles <- Sys.glob("rgb??.tif")
#'geomTrainFiles <- Sys.glob("rgb??.shp")
#'
#'##- create stacks from image and geometry files
#'imageTrainStack<-lapply(imageTrainFiles, FUN=raster::stack)
#'geomTrainStack  <- lapply(geomTrainFiles, FUN=raster::shapefile)
#'names(imageTrainStack[[1]])<-c("red","green","blue")
#'names(imageTrainStack[[2]])<-c("red","green","blue")
#'names(imageTrainStack[[3]])<-c("red","green","blue")
#'
#'##' finally extraxt the training data to a data frame
#'trainDF <- get_traindata(rasterStack  = imageTrainStack,
#'                         trainPlots = geomTrainStack)
#'
#'##- have a look at the training data
#'head(trainDF)
#'}



get_traindata<-function(rasterStack  = NULL,
                        trainPlots   = NULL) {

  catNote <- crayon::blue $ bold


  cat(catNote("\n:::: extract trainPlots data...\n"))
  trainingDF =  data.frame()
  # extract trainPlots Area pixel values
  # TODO https://gis.stackexchange.com/questions/253618/r-multicore-approach-to-extract-raster-values-using-spatial-points
  for (j in 1:length(rasterStack)) {
    cat("\n    extracting trainPlots data from image ",j," of ... ",length(rasterStack),"\n")

    categorymap<-rgeos::gUnionCascaded(trainPlots[[j]],id=trainPlots[[j]]@data$id)
    categorymap<-sp::spTransform(categorymap,(rasterStack[[j]]@crs))
    dataSet <- raster::extract(rasterStack[[j]], categorymap,df=TRUE)
    #names(dataSet)<-append(c("ID"),names(rasterStack[[j]]))
    ## add filename as lloc category
    #FNname<-substr(names(rasterStack[[j]][[1]]),1,nchar(names(rasterStack[[j]][[1]]))-2)
    dataSet$FN <- rasterStack[[j]]@filename
    dataSet[is.na(dataSet)] <- 0
    dataSet=dataSet[stats::complete.cases(dataSet),]

    trainingDF<-rbind(trainingDF, dataSet)
    #save(dataSet, file = paste0(path_tmp,"tmptrain_",j,".RData"))

  }

  return(trainingDF)
}

#' counts pixel values according to their classes
#'
#' @param ids numeric. the ids used for the training
#' @param position sp. spatialpoint object containing the centre target positions
#' @param  imageFiles raster* image/classification file
#' @param outPrefix character. out prefix string
#' @param ext character. extension
#' @param path   character. output path
#' @param dropChars numeric. number of characters that should be dropped at the end of the filename
#' @param buffersize numeric. radius in meters around position
#'
#' @export get_counts
#' @examples
#'\dontrun{
#' ##- required packages
#'  require(uavRst)

#'
#' ##- project root folder
#'  setwd(tempdir())
#'
#'
#' ##- get the rgb image, chm and training data
#'  utils::download.file("https://github.com/gisma/gismaData/raw/master/uavRst/data/tutorial.zip",
#'                       "tutorial_data.zip")
#'  unzip(zipfile = "tutorial_data.zip" ,
#'          exdir = ".")

#'
#' # read data
#'  position <- raster::shapefile("position.shp")
#'  imageFiles <-Sys.glob(paths = paste0("rgb*","tif"))
#'  imageTrainStack<-lapply(imageFiles, FUN=raster::stack)
#'
#' ## get counts
#'  df1<-get_counts(position = position,
#'                       ids = c(100,200),
#'                imageFiles = imageFiles,
#'                 outPrefix = "",
#'                       ext = ".tif",
#'                      path = tempdir())
#' head(df1)
#'##+}


get_counts<- function(ids=c(1,2),
                      position=NULL,
                      imageFiles = NULL,
                      buffersize=1.5,
                      outPrefix="classified_index_",
                      ext=".tif",
                      path = tempdir(),
                      dropChars=0) {

  buffers<-rgeos::gBuffer(position,width=buffersize)
  ex<-data.frame()
  df<- lapply(seq(1:length(position)), function(i) {
    fn<- file.path(R.utils::getAbsolutePath(path),paste0(outPrefix,substr(position[i,]$tree,1,nchar(position[i,]$tree)-dropChars),ext))

    if (file.exists(fn)){
      pos <-sp::spTransform(position[i,],raster::raster(fn)@crs)
      ex <- as.data.frame(unlist(raster::extract(raster::raster(fn) , pos    , buffer=buffersize, df=TRUE)))

      idVal<-as.numeric(vector(length = length(ids)))
      for (j in 1:length(ids)){
        idVal[j] <- sum(ex[ex[2] == ids[j],  2])/j
      }}


    return(c(idVal,basename(fn)))
    #return(c("green"=gr,"nogreen"=no,"all"=all, "plot"=basename(imageFiles[i])))
  }
  )
  result <- do.call("rbind", df)
  return(result)
}

#' classify images using raster predict
#'
#' @param imageFiles raster*. imagestack for classification purposes must contain the required bands as needed by the model.
#' @param model model. classification model
#' @param  inPrefix character. in frefix  string
#' @param outPrefix character. out prefix string
#' @param bandNames character. band names
#'
#' @export predict_rgb
#' @examples
#'\dontrun{
#' ##- required packages
#' require(uavRst)
#' require(link2GI)
#'
#' ##- project folder
#' projRootDir<-tempdir()
#'
#' ##-create subfolders pls notice the pathes are exported as global variables
#' paths<-link2GI::initProj(projRootDir = projRootDir,
#'                         projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                         global = TRUE,
#'                         path_prefix = "path_")

#' setwd(path_run)
#' unlink(paste0(path_run,"*"), force = TRUE)
#'
#' ##- get the tutorial data
#' utils::download.file("https://github.com/gisma/gismaData/raw/master/uavRst/data/ffs.zip",
#' paste0(path_run,"ffs.zip"))
#' unzip(zipfile =  paste0(path_run,"ffs.zip"), exdir = ".")
#'
#' ##- assign tutorial data
#' imageFile <- paste0(path_run,"predict.tif")
#' load(paste0(path_run,"tutorialbandNames.RData"))
#' tutorialModel<-readRDS(file = paste0(path_run,"tutorialmodel.rds"))
#'
#' ##- start the  prediction taking the non optimized model
#' ##- please note the output is saved in the subdirectory path_output
#' predict_rgb(imageFiles=imageFile,
#'             model = tutorialModel[[1]],
#'             bandNames = bandNames)
#'
#' ##- visualise the classification
#' raster::plot(raster::raster(paste0(path_output,"classified_predict.tif")))
#'##+}


predict_rgb <- function(imageFiles=NULL,
                        model = NULL,
                        inPrefix = "index_",
                        outPrefix = "classified_",
                        bandNames = NULL) {

  if (is.null(bandNames)) return(cat(getCrayon()[[1]]("\n you did not provide predictor names. \nTypically something like bandNames ie c('R','G','B')")))
  po = path_output
  i = 1:length(imageFiles)
  cat("\n::: start prediction aka classifikation...\n")

  #cl <- parallel::makeCluster(parallel::detectCores())
  #doParallel::registerDoParallel	(cl)
  #foreach::foreach(i,po) %dopar% {
    for (i in 1:length(imageFiles)) {
    requireNamespace("raster")
    #requireNamespace(randomForest)
    #require(caret)
    #TODO rasterstack
    fn<-basename(imageFiles[i])
    fnOut <- paste0(po,outPrefix,fn)

    img2predict<-raster::stack(imageFiles[i])
    names(img2predict)<-bandNames
    predictImg<- raster::predict(img2predict,
                                 model,
                                 progress= "text")
    raster::writeRaster(predictImg, filename = fnOut, overwrite = TRUE)
  }
  #parallel::stopCluster(cl)
}

#' Forward feature selection based on rf model
#' @description ffs_train is a wrapper function for a simple use of the forward feature selection approach
#' of training random forest classification models. This validation is particulary suitable for
#' leave-location-out cross validations where variable selection
#' MUST be based on the performance of the model on the hold out station.
#' See \href{https://www.sciencedirect.com/science/article/pii/S1364815217310976}{Meyer et al. (2018)}
#' for further details.
#' This is in fact the case while using time space variable vegetation patterns for classification purposes.
#' For the UAV based RGB/NIR imagery, it provides an optimized preconfiguration for the classification goals.
#'
#'@note The workflow of \code{uavRst} is intended to use the forward feature selection as decribed by \href{https://www.sciencedirect.com/science/article/pii/S1364815217310976}{Meyer et al. (2018)}.
#'This approach needs at least a pair of images that differ in time and/or space for a leave one location out validation mode. You may overcome this situation if you tile your image and provide for each tile seperate training data.
#'If you just want to classify a single image by a single training file use the normal procedure as provided by the \code{\link[caret]{trainControl}} function.


#'
#' @param trainingDF    dataframe. containing training data
#' @param runtest       logical. default is false, if set a external validation will be performed
#' @param predictors    character. vector of predictor names as given by the header of the training data table
#' @param response      character. name of response variable as given by the header of the training data table
#' @param spaceVar      character. name of the spacetime splitting vatiable as given by the header of the training data table
#' @param names         character. all names of the dataframe header
#' @param noLoc         numeric. number of locations to leave out usually number of discrete trainings locations/images
#' @param pVal          numeric. used part of the training data  default is \code{ 0.5}
#' @param prefin        character. name pattern used for model default is \code{"final_"}
#' @param preffs        character. name pattern used for ffs default is \code{"ffs_"}
#' @param modelSaveName character. name pattern used for saving the model default is \code{"model.RData" }
#' @param seed          numeric. number for seeding
#' @param noClu         numeric. number of cluster to be used
#' @param withinSE      locical.  compares the performance to models that use less variables (e.g. if a model using 5 variables is better than a model using 4 variables but still in the standard error of the 4-variable model, then the 4-variable model is rated as the better model).
#' @param mtry          numerical. Number of variable is randomly collected to be sampled at each split time
#' @param sumFunction   character. function to summarize default is "twoClassSummary"
#' @export ffs_train
#' @examples
#' \dontrun{
#' require(uavRst)
#'
#' ##- project folder
#' projRootDir<-tempdir()
#'
#' # create subfolders please mind that the pathes are exported as global variables
#' paths<-link2GI::initProj(projRootDir = projRootDir,
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")

#' setwd(path_run)
#' unlink(paste0(path_run,"*"), force = TRUE)
#'
#' ##- get the rgb image, chm and training data
#' utils::download.file("https://github.com/gisma/gismaData/raw/master/uavRst/data/ffs.zip",
#'                       paste0(path_run,"ffs.zip"))
#' unzip(zipfile = paste0(path_run,"ffs.zip"),exdir = ".")
#'
#' ##- get geometrical training data assuming that you have used before the calc_ext function
#' trainDF<-readRDS(paste0(path_run,"tutorial_trainDF.rds"))
#' load(paste0(path_run,"tutorialbandNames.RData"))
#'
#' ##- define the classes
#'  idNumber=c(1,2,3)
#'  idNames= c("green tree","yellow tree","no tree")
#' ##- add classes names
#'  for (i in 1:length(idNumber)){
#'    trainDF$ID[trainDF$ID==i]<-idNames[i]
#'  }
#' ##- convert to factor (required by rf)
#'  trainDF$ID <- as.factor(trainDF$ID)
#' ##- now prepare the predictor and response variable names
#' ##- get actual name list from the DF
#'  name<-names(trainDF)
#' ##- cut leading and tailing ID/FN
#'  predictNames<-name[3:length(name)-1]
#'
#' ##- call Training
#'  model <-  ffs_train(trainingDF= trainDF,
#'                      predictors= predictNames,
#'                      response  = "ID",
#'                      spaceVar  = "FN",
#'                      names     = name,
#'                      pVal      = 0.1,
#'                      noClu     = 1)
#'
#' ##- for classification/prediction go ahead with the predict_RGB function
#' ##+}

ffs_train<-function(   trainingDF   = NULL,
                       predictors   = c("R","G","B"),
                       response     = "ID",
                       spaceVar     = "FN",
                       names        = c("ID","R","G","B","A","FN"),
                       noLoc        = NULL,
                       sumFunction  = "twoClassSummary",
                       pVal         = 0.5,
                       prefin       ="final_",
                       preffs       ="ffs_",
                       modelSaveName="model.RData" ,
                       runtest      = FALSE,
                       seed         = 100,
                       withinSE     = TRUE,
                       mtry         = 2,
                       noClu = 1) {

  if (is.null(noLoc)) noLoc <- length(unique(trainingDF$FN))
  # create subset according to pval
  trainIndex<-caret::createDataPartition(trainingDF$ID, p = pVal, list=FALSE)
  data_train <- trainingDF[ trainIndex,]
  if (runtest) data_test <- trainingDF[-trainIndex,]
  # create llo
  spacefolds <- CAST::CreateSpacetimeFolds(x        = data_train,
                                           spacevar = spaceVar,
                                           k        = noLoc, # number of CV
                                           seed     = seed)

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
                                classProbs      = FALSE)

  # make it parallel
  cl <- parallel::makeCluster(noClu)
  doParallel::registerDoParallel(cl)
  # run forward feature selection
  ffs_model <- CAST::ffs(predictors = data_train[,predictors],
                   response   = eval(parse(text=paste("data_train$",response,sep = ""))),
                   method     = "rf",
                   metric     = metric,
                   trControl  = ctrl,
                   withinSE   = withinSE,
                   tuneGrid   = expand.grid(mtry = mtry)
  )

  # take resulting predictors
  predictors <- data_train[,names(ffs_model$trainingData)[-length(names(ffs_model$trainingData))]]
  # and run final tuning
  model_final <- caret::train(predictors,
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



#' Convenient function to preprocess synthetic raster bands from a given RGB image and/or DTM/DSM data.
#' @description
#' Convenient function to preprocess synthetic raster bands from a given RGB image and/or DTM/DSM data.
#' Currently for all chanels of the input images the following textures and indices can be calculated:\cr
#' rgb indices (17), color transformation (9), haralick (3 = 25 layers), statitics (4),edge (3),morpho (4), DEM Parameter (20).
#' All layers will be stacked (as an ENVI file). 
#' If wanted the raster values can be extracted to a data frames by overlaying corresponding vector data. NOTE: The vector data has to be named identically to the rasterfiles.  This is useful
#' for for classification training purposes and covers usually step 1 of the random forest based
#' classification of UAV derived visible imagery and point clouds.

#'@details
#'
#' (01) calc_ext() calculation of spectral indices, basic spatial statistics and textures and
#'               extracting of training values over all channels according to training data\cr\cr
#' (02) ffs_train() training using random forest and the forward feature selection method \cr
#'                      startTrain=TRUE\cr\cr
#' (03) calc_ext() with respect to the selected predictor variables you may calculate
#'               the requested channels for all rgb data that you want to predict.\cr\cr
#' (04) prediction startPredict=TRUE\cr\cr
#'
#'@note If the function is used for stand alone extraction of the training data please provide both, the imagestack containing the raster data plus the corresponding band names (usually saved as an Rdata file) and the corresponding shape file.
#'@note The workflow is intended to use the forward feature selection as decribed by \href{https://www.sciencedirect.com/science/article/pii/S1364815217310976}{Meyer et al. (2018)}.
#'This approach needs at least a pair of images that differ in time and/or space for a leave one location out validation mode. You may overcome this situation if you tile your image and provide for each tile seperate training data.
#'If you just want to classify a single image by a single training file use the normal procedure as provided by the \code{\link[caret]{trainControl}} function.


#' @param calculateBands    logical. switch for set on calculation of syntheic bands and indices default = TRUE
#' @param extractTrain      logical. switch for set on extract training data according to training geometries default = TRUE
#' @param patternImgFiles   character. mandantory string that exist in ech imagefile to be processes
#' @param patternIdx         character. code string that will concatenated to the filename to identify the index file
#' @param prefixRun      character. general prefix of all result files of the current run default = "tmp"
#' @param patterndemFiles       character. mandantory if DEM data is processes. prefix of current DEM default = "dem"
#' @param prefixTrainImg    character. potential string of characters that is used in the beginning of a shape file prefixTrainImg_SHAPEFILENAME_suffixTrainImg
#' @param suffixTrainImg    character. potential string of characters that is used in the beginning of a shape file prefixTrainImg_SHAPEFILENAME_suffixTrainImg
#' @param prefixTrainGeom    character. potential string of characters that is used in the beginning of a shape file prefixTrainGeom_SHAPEFILENAME_suffixTrainGeom
#' @param suffixTrainGeom   character. potential string of characters that is used in the beginning of a shape file prefixTrainGeom_SHAPEFILENAME_suffixTrainGeom
#' @param channels          character. channels to be choosed options are c("red", "green", "blue")  default =  c("red", "green", "blue")
#' @param hara              logical. switch for using  HaralickTextureExtraction, default = TRUE. \cr
#' @param haraType          character. hara options, default is c("simple"), other  options are "advanced"  "higher" "all". NOTE:  "higher" takes a LOT of time
#' @param stat              logical. switch for using statistic default = TRUE the stas are mean,variance, curtosis, skewness
#' @param pardem            logical. switch for calculating dem parameter, default = FALSE
#' @param demType           character. ("hillshade","slope", "aspect","TRI","TPI","Roughness")
#' @param edge              logical. switch for using edge filtering default = TRUE
#' @param edgeType          character. edge options, default is c("gradient","sobel","touzi") all options are c("gradient","sobel","touzi")
#' @param morpho            logical. switch for using morphological filtering default = TRUE
#' @param morphoType        character. morphological options, default is c("dilate","erode","opening","closing") all options are ("dilate","erode","opening","closing")
#' @param rgbi              logical. switch for using rgbi index calcualtions default = TRUE
#' @param indices           character. RGB indices, default is c("VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI") all options are c("VVI","VARI","NDTI","RI","SCI","BI","SI","HI","TGI","GLI","NGRDI","GRVI","GLAI","HUE","CI","SAT","SHP")
#' @param rgbTrans          logical. switch for using color space transforming default = TRUE
#' @param colorSpaces       character.  RGB colorspace transforming to default c("CIELab","CMY","Gray","HCL","HSB","HSI","Log","XYZ","YUV")
#' @param kernel            numeric. size of kernel for filtering and statistics, default is  3
#' @param morphoMethod  numeric. saga morphometric method
#' @param minScale  numeric. in scale for multi scale TPI
#' @param maxScale  numeric. max scale for multi scale TPI
#' @param numScale  numeric. number of scale for multi scale TPI
#' @param currentDataFolder  NULL folder to image (and shape) data
#' @param currentIdxFolder  NULL folder for saving the results
#' @param cleanRunDir  logical. TRUE logical switch for deleting the calculated tifs, default is TRUE
#' @param giLinks     list. GI tools cli paths
#' @examples
#' \dontrun{
#'
#' ##- required packages
#' require(uavRst)
#' require(link2GI)
#'
#' # create and check the links to the GI software
#' giLinks<-uavRst::linkAll()
#' if (giLinks$saga$exist & giLinks$otb$exist){
#'#'
#' ##- create and set folders
#' ##- please mind that the pathes are exported as global variables
#' paths<-link2GI::initProj(projRootDir = tempdir(),
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")
#'
#' ##- clean runtime folder
#' unlink(paste0(path_run,"*"), force = TRUE)
#'
#' ##- get the tutorial data
#' utils::download.file("https://github.com/gisma/gismaData/raw/master/uavRst/data/tutorial_data.zip",
#'                      paste0(path_run,"tutorial_data.zip"))
#' unzip(zipfile = paste0(path_run,"tutorial_data.zip"), exdir = R.utils::getAbsolutePath(path_run))
#'
#' ##- calculate some synthetic channels from the RGB image and the canopy height model
#' ##- then extract the from the corresponding training geometries the data values aka trainingdata
#' trainDF <- calc_ext(calculateBands    = TRUE,
#'                     extractTrain      = TRUE,
#'                     suffixTrainGeom   = "",
#'                     patternIdx        = "index",
#'                     patternImgFiles   = "rgb" ,
#'                     patterndemFiles   = "chm",
#'                     prefixRun         = "tutorial",
#'                     prefixTrainImg    = "",
#'                     rgbi              = TRUE,
#'                     indices           = c("TGI","CI"),
#'                     channels          = c("red"),
#'                     rgbTrans          = FALSE,
#'                     hara              = FALSE,
#'                     haraType          = c("simple"),
#'                     stat              = FALSE,
#'                     edge              = FALSE,
#'                     morpho            = FALSE,
#'                     pardem            = TRUE,
#'                     demType           = c("slope", "MTPI"),
#'                     kernel            = 3,
#'                     currentDataFolder = path_run,
#'                     currentIdxFolder  = path_run,
#'                     giLinks = giLinks)
#'
#' ##- show the result
#' head(trainDF)
#' # use ffs_train as next step for rf classification issues
#' }
#' ##+}


#' @export calc_ext


calc_ext<- function ( calculateBands    = FALSE,
                      extractTrain      = TRUE,
                      prefixRun         = "temp",
                      patterndemFiles   = "dem",
                      prefixTrainImg    = "",
                      prefixTrainGeom   = "",
                      suffixTrainImg    = "",
                      suffixTrainGeom   = "",
                      patternIdx        = "index",
                      patternImgFiles   = "",
                      channels          = c("red", "green", "blue"),
                      hara              = TRUE,
                      haraType          = c("simple","advanced","higher"),
                      stat              = TRUE,
                      edge              = TRUE,
                      edgeType          = c("gradient","sobel","touzi"),
                      morpho            = TRUE,
                      morphoType        = c("dilate","erode","opening","closing"),
                      rgbi              = TRUE,
                      indices           = c("VVI","VARI","NDTI","RI","SCI","BI",
                                            "SI","HI","TGI","GLI","NGRDI","GRVI",
                                            "GLAI","HUE","CI","SAT","SHP") ,
                      rgbTrans          = TRUE,
                      colorSpaces       = c("CIELab","CMY","Gray","HCL","HSB","HSI","Log","XYZ","YUV"),
                      pardem            = TRUE,
                      demType           = c("hillshade","slope", "aspect","TRI","TPI","Roughness",
                                            "SLOPE","ASPECT", "C_GENE","C_PROF","C_PLAN","C_TANG",
                                            "C_LONG","C_CROS","C_MINI","C_MAXI","C_TOTA","C_ROTO","MTPI"),
                      morphoMethod = 6,
                      minScale = 1,
                      maxScale = 8,
                      numScale = 2,
                      kernel            = 3,
                      currentDataFolder = NULL,
                      currentIdxFolder  = NULL,
                      cleanRunDir        = TRUE,
                      giLinks = NULL){
  if (!exists("path_run")) path_run = tempdir()
  if (!rgbi) rgbTrans <- hara <- stat <- edge <- morpho <- FALSE
  if (is.null(giLinks)){
    giLinks <- linkAll()
  }

  gdal <- giLinks$gdal
  saga <- giLinks$saga
  otb <- giLinks$otb
  sagaCmd<-saga$sagaCmd
  path_OTB <- otb$pathOTB

  catHead <- getCrayon()[[4]]
  catErr  <- getCrayon()[[2]]
  catNote <- getCrayon()[[1]]
  catOk   <- getCrayon()[[3]]


  # if (nchar(prefixRun)>0)   prefixRun<-  paste0(prefixRun,"_")
  # if (nchar(patterndemFiles)>0)   patterndemFiles <-  paste0(patterndemFiles,"_")
  # if (nchar(patternImgFiles)>0)   patternImgFiles <-  paste0(patternImgFiles,"_")
  # if (nchar(prefixTrainImg)>0)   prefixTrainImg <-  paste0(prefixTrainImg,"_")
  # if (nchar(patternIdx)>0)   patternIdx <-  paste0(patternIdx,"_")
  # if (nchar(suffixTrainGeom)>0)   suffixTrainGeom <-  paste0("_",suffixTrainGeom)
  # if (nchar(suffixTrainImg)>0)   suffixTrainImg <-  paste0("_",suffixTrainImg)
  #
  currentDataFolder<- currentDataFolder #paste0(path_data_training)
  currentIdxFolder<- currentIdxFolder # paste0(path_data_training_idx)

  if (((stat == TRUE) || (hara == TRUE) || (edge == TRUE) || (morpho == TRUE)) & !otb$exist == "") stop("OTB missing - please check")

  ### ----- start preprocessing ---------------------------------------------------
  if (calculateBands) {
    cat(catHead("\n--- calculate synthetic channels ---\n"))

    # create list of image files to be processed
    # NOTE all subfolder below c("data/","output/","run/","fun","idx") have to created individually

    #imageFiles <- list.files(pattern=paste0("^",prefixRun,"*","tif"), path=currentDataFolder, full.names=TRUE)
    imageFiles <-Sys.glob(paths = paste0(currentDataFolder,patternImgFiles,"*","tif"))
    demFiles <- Sys.glob(paths = paste0(currentDataFolder,patterndemFiles,"*","tif"))

    # create a counter for all input files to be processed
    counter<- max(length(demFiles),length(imageFiles))

    ### calculate indices and base stat export it to tif
    # create list vars
    bandNames <- flist <- dellist <- list()

    for (i in 1:counter){
      bandNames <- list()
      # if calc pardem
      if (pardem){

        #cat(catNote(":::: processing dem... ",demType,"\n"))
        flist<-append(flist, Sys.glob(demFiles[i]))
        dellist <- append(dellist, file.path(R.utils::getAbsolutePath(path_run),"dem2.tif"))
        bandNames <-append(bandNames,"dem")
        morpho_dem(dem = demFiles[i],
                   item = demType,
                   morphoMethod = morphoMethod,
                   minScale = minScale,
                   maxScale = maxScale,
                   numScale = numScale,
                   giLinks = giLinks)
        flist<-append(flist, Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(demType,".tif"))))
        dellist <- append(dellist, Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(demType,".*"))))
        for (item in demType)
          bandNames <-append(bandNames,make_bandnames(dem = item))

      }


      # for all images do



      if (rgbi){
        cat(catNote(":::: processing indices of...",basename(imageFiles[i]),"\n"))
        r<-raster::stack(imageFiles[i])
        # calculate and stack r,g,b and requested indices
        rgb_rgbi<-raster::stack(r[[1:3]],uavRst::rgb_indices(r[[1]],r[[2]],r[[3]],indices))
        bandNames <- append(bandNames,make_bandnames(rgbi = indices))
        names(rgb_rgbi)<-append(c("red","green","blue"),indices)
        cat(catOk("\n     save ...",paste0(path_run,"rgbi_",basename(imageFiles[i])),"\n"))
        raster::writeRaster(rgb_rgbi,
                            file.path(R.utils::getAbsolutePath(path_run),paste0("rgbi_",basename(imageFiles[i]))),
                            progress = "text",
                            overwrite=TRUE)

        flist<-append(flist, file.path(R.utils::getAbsolutePath(path_run),paste0("rgbi_",basename(imageFiles[i]))))
        dellist <- append(dellist, file.path(R.utils::getAbsolutePath(path_run),paste0("rgbi_",basename(imageFiles[i]))))
      }
      # if RGB transform
      if (rgbTrans){

        cat(catNote(":::: processing color transformation...\n"))
        uavRst::colorspace(input = imageFiles[i],
                           colorspace = colorSpaces)
        rgbTranslist<-list()
        jj=1
        for (colMod in colorSpaces) {
          rgbTranslist[[jj]]<-file.path(R.utils::getAbsolutePath(path_run),paste0(colMod,"_",basename(imageFiles[i])))
          jj<-jj+1
        }
        rt<- lapply(rgbTranslist, FUN=raster::stack)
        for (jj in 1:length(rt)) {
          raster::extent(rt[[jj]])<-raster::extent(r)
          raster::projection(rt[[jj]]) <- raster::crs(raster::projection(r))
          cat(catOk(":::: save... ",paste0(colorSpaces[jj],"_",basename(imageFiles[i])),"\n"))
          raster::writeRaster(raster::stack(rt[[jj]][[1:(raster::nlayers(rt[[jj]])-1)]]),
                              file.path(R.utils::getAbsolutePath(path_run),paste0(colorSpaces[jj],"_ref",basename(imageFiles[i]))),
                              overwrite=TRUE,
                              options="INTERLEAVE=BAND",
                              progress="text")
          bandNames <-append(bandNames,make_bandnames(rgbTrans = colorSpaces[jj]))
          flist<-append(flist, file.path(R.utils::getAbsolutePath(path_run),paste0(colorSpaces[jj],"_ref",basename(imageFiles[i]))))
          dellist <- append(dellist, file.path(R.utils::getAbsolutePath(path_run),paste0(colorSpaces[jj],"_ref",basename(imageFiles[i]))))
          dellist <- append(dellist, file.path(R.utils::getAbsolutePath(path_run),paste0(colorSpaces[jj],basename(imageFiles[i]))))
        }
        #file.remove(paste0(path_run,unlist(rgbTranslist)))
        #r<-raster::stack(imageFiles[i])

        #bandNames <-append(bandNames,make_bandnames(rgbTrans = colorSpaces))

      }
      if (rgbi){
        # assign bandnumber according to name
        cat("\n")
        for (filterBand in channels){
          if (filterBand=="red") bandNr <- 1
          if (filterBand=="green") bandNr <- 2
          if (filterBand=="blue") bandNr <- 3
          # export single channel for synthetic band calculation
          # if (filterBand!="") {
          cat(catNote(":::: write temporary channel...",paste0(filterBand,"_",basename(imageFiles[i])),"\n"))
          raster::writeRaster(rgb_rgbi[[bandNr]],
                              file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"_",basename(imageFiles[i]))),
                              progress = "text",
                              overwrite=TRUE)
          fbFN<-file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"_",basename(imageFiles[i])))
         # filterband
        if (stat){
          cat(catNote(":::: processing stats...",fbFN,"\n"))
          otb_stat(input = fbFN,
                   out = file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"stat_",basename(imageFiles[i]))),
                   ram = "4096",
                   radius =  kernel,
                   giLinks=giLinks)
          flist<-append(flist,Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"stat_*"))))
          dellist <-append(dellist,Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"stat_*"))))
          bandNames <-append(bandNames,paste0(make_bandnames(stat = TRUE),"_",filterBand))
        }
        # if calc edge
        if (edge){
          for (edges in edgeType){
            cat(catNote(":::: processing edge... ",edges,"\n"))
            uavRst::otbtex_edge(input = fbFN,
                                out = file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,edges,basename(imageFiles[i]))),
                                filter = edges,
                                giLinks=giLinks)

            flist<-append(flist,Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,edges,"*"))))
            dellist<-append(dellist,Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,edges,"*"))))
            bandNames <-append(bandNames,make_bandnames(edge = paste0(edges,"_",filterBand)))
          }
        }

        # if calc morpho
        if (morpho){
          for (morphos in morphoType){
            cat(catNote(":::: processing morpho... ",morphos,"\n"))
            uavRst::otbtex_gray(input = fbFN,
                                out = file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,morphos,basename(imageFiles[i]))),
                                filter = morphos,
                                giLinks=giLinks)
            flist<-append(flist,Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,morphos,"*"))))
            dellist<-append(dellist,Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,morphos,"*"))))
            bandNames <-append(bandNames,make_bandnames(edge = paste0(morphos,"_",filterBand)))
          }
        }
        # if calc haralick
        if (hara){
          for (type in haraType){
            cat(catNote(":::: processing haralick... ",type,"\n"))
            otbtex_hara(x = fbFN,
                        output_name=file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"hara_",basename(imageFiles[i]))),
                        texture = type,
                        giLinks=giLinks)
            flist<-append(flist,Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"hara_*"))))
            dellist<-append(dellist,Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"hara_*"))))
            bandNames <-append(bandNames,paste0(make_bandnames(bandNames = type),"_",filterBand))
          }
        }
          # delete single channel for synthetic channel calculation
          file.remove(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"_",basename(imageFiles[i]))))
        }
      }# end of single channnel calculation

      # create an alltogether stack
      if (rgbi)  tmpFN<-paste0(substr(basename(imageFiles[i]),1,nchar(basename(imageFiles[i]))-4))
      else if (length(demFiles)>= i)  tmpFN<-paste0(substr(basename(demFiles[i]),1,nchar(basename(demFiles[i]))-4))
      else return(cat(catErr("\nhopefully done\n You are mixing RGB an DEM input files. You may do this but only if they are of the same extent etc. and if each image file has a corresponding dem file\n NOTE the dem filename MUST have a prefix default is 'dem_'.")))
      cat(catOk("     save ...",paste0(patternIdx, tmpFN),"\n"))
      # r<-raster::brick(raster::stack(flist)) qgis cannot read heder
      r<-raster::stack(paste0(flist))
      if (raster::nlayers(r)!=length(bandNames)) stop("\n Number of names and layers differ...\n most common case is a broken cleanup of the runtime directory!")
      names(r)<-bandNames
      # write file to envi
      raster::writeRaster(r,
                          paste0(currentIdxFolder,"/", patternIdx,tmpFN),
                          format="ENVI",
                          progress ="text",
                          #options="COMPRESS=LZW",
                          overwrite=TRUE)
      #raster::hdr(r, filename = paste0(currentIdxFolder,"/", patternIdx,tmpFN), format = "ENVI") qgis cannot read heder

      # cleanup runtime files lists...
      if (cleanRunDir) {
        cat(catNote(":::: removing temp files...\n"))
        res<-file.remove(unlist(dellist))
      }
      flist <- dellist <- list()

    }

    # save bandname list we need it only once
    save(bandNames,file = paste0(currentIdxFolder,prefixRun,"bandNames.RData"))


    cat(catHead("\n--- calculation of synthetic channels finished ---\n"))
  }
  # ----- start extraction ---------------------------------------------------
  if (extractTrain){
    cat(catHead("\n--- extract training data ---\n"))
    load(paste0(currentIdxFolder,prefixRun,"bandNames.RData"))
    # get image and geometry data for training purposes
    imageTrainStack <- list()
    imageTrainFiles <- list.files(pattern="[.]envi$", path=currentIdxFolder, full.names=TRUE)
    tmp  <- basename(list.files(pattern="[.]envi$", path=currentIdxFolder, full.names=TRUE))
    for (j in 1:length(imageTrainFiles) )  {
      rs<-raster::stack(imageTrainFiles[[j]])
      rs@filename <-tmp[j]
      names(rs)<-bandNames
      imageTrainStack <- append(imageTrainStack,rs)
      }
    tmp<- gsub(patternIdx,prefixTrainGeom,tmp)
    tmp<- gsub(suffixTrainImg,suffixTrainGeom,tmp)
    geomTrainFiles <- gsub(".envi",".shp",tmp)
    geomTrainFiles <- paste0(currentDataFolder,geomTrainFiles)
    #imageTrainStack<-lapply(imageTrainFiles, FUN=raster::stack)
    if (file.exists(raster::extension(geomTrainFiles[[1]], ".shp")))
      geomTrainStack  <- lapply(geomTrainFiles, FUN=raster::shapefile)
    else
      return(cat(catErr("\nTraining files are not existing please check suffix or prefix strings")))
    # extract clean and format training data
    for (i in 1: length(imageTrainStack))
    imageTrainStack[[i]]@crs<-geomTrainStack[[i]]@proj4string

    trainDF <- uavRst::get_traindata(rasterStack  = imageTrainStack,
                                     trainPlots = geomTrainStack)

    names(trainDF)<-append("ID",append(bandNames,"FN"))

    # create a new dataframe with prefixRun
    assign(paste0(prefixRun,"_trainDF"), trainDF)
    # save it
    saveRDS(eval(parse(text=paste0(prefixRun,"_trainDF"))), paste0(currentIdxFolder,prefixRun,"_trainDF",".rds"))
    #read it into another name
    #DF<-readRDS(paste0(currentIdxFolder,prefixRun,"_trainDF",".rds"))
    cat(catHead("\n--- training data extraction finished ---\n"))

    return(list(trainDF,bandNames))
  }
}


cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- stats::cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
