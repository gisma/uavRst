if (!isGeneric('extractTrainData')) {
  setGeneric('extractTrainData', function(x, ...)
    standardGeneric('extractTrainData'))
}

#'@name extractTrainData
#'@title extracts training data from a raster stack
#'
#'@description
#' extracts training data from a raster stack.return extractTrainData returns a dataframe with all training data
#'
#'@author Chris Reudenbach
#'
#'@param rasterStack  default is \code{NULL} rasterstack wcontaining all image data
#'@param trainPlots default is \code{NULL}  sp object providing training geometries
#'@param imgFN default is \code{file.path(tempdir(),"trainingDF.RData")} Name of the extracted training data file
#'@param bnames names of the bands
#'@import crayon

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
#' @param buffersize size in meters around position
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
#' @param imageFiles imagestacke for classification
#' @param model classification model
#' @param  in_prefix in frefix  string
#' @param out_prefix out prefix string
#' @param bandNames band names 
#' 
#' @export predictRGB 
#' @examples 
#' \dontrun{
#' predictRGB(imageFiles=imagestack,
#'             model = model_final,
#'             in_prefix = "index_",
#'             out_prefix = "classified_",
#'             bandNames = c("R","G","B","A","VARI","NDTI","TGI","GLI","NGRDI")) 
#'}

predictRGB <- function(imageFiles=NULL,
                       model = NULL,
                       in_prefix = "index_",
                       out_prefix = "classified_",
                       bandNames = c("R","G","B","A","VARI","NDTI","TGI","GLI","NGRDI","GLAI","Mean","Variance","Skewness","Kurtosis")) {
  po = path_output
  i = 1:length(imageFiles)
  cat("\n::: start prediction aka classifikation...\n")
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel	(cl)
  foreach::foreach(i,po) %dopar% {
    #for (i in 1:length(imageFiles)) {
    requireNamespace(raster)  
    #requireNamespace(randomForest)
    #require(caret)
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
#' @param pVal          used part of the training data  default is \code{ 0.5}
#' @param prefin        name pattern used for model default is \code{"final_"}
#' @param preffs        name pattern used for ffs default is \code{"ffs_"}
#' @param modelSaveName name pattern used for saving the model default is \code{"model.RData" }
#' @param seed          number for seeding
#' @param noClu         number of cluster to be used
#' @param sumFunction sumfunction
#' @export ffsTrainModel
#' @examples  
#' \dontrun{
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
                        noLoc        = length(unique(trainingDF$FN)),
                        sumFunction  = "twoClassSummary",
                        pVal         = 0.5,
                        prefin       ="final_",
                        preffs       ="ffs_",
                        modelSaveName="model.RData" ,
                        runtest      = FALSE,
                        seed         = 100,
                        noClu = 3) {

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

#' create name vector corresponding to the training image stack 
#' 
#' @param rgbi default is  NA
#' @param bandNames band names 
#' @param  stat band names 
#' @param morpho band names 
#' @param edge band names 
#' @param RGBtrans RGBtrans
#' 
#' @export makebNames 

makebNames <- function(rgbi    = NA,
                       bandNames = NA,
                       stat    = FALSE,
                       morpho  = NA,
                       edge    = NA ,
                       RGBtrans=NA){
  
  if (!is.na(rgbi[1])) bnames <- append(c("red","green","blue"),rgbi)
  if (!is.na(bandNames)) {
    if(bandNames == "simple"){
      bnames <- c("Energy", "Entropy", "Correlation", 
                  "Inverse_Difference_Moment", "Inertia", 
                  "Cluster_Shade", "Cluster_Prominence",
                  "Haralick_Correlation")
    } else if(bandNames == "advanced"){
      bnames <- c("Hara_Mean", "Hara_Variance", "Dissimilarity",
                  "Sum_Average", 
                  "Sum_Variance", "Sum_Entropy", 
                  "Difference_of_Variances", 
                  "Difference_of_Entropies", 
                  "IC1", "IC2")
    } else if(bandNames == "higher"){
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
    } else if(bandNames == "all"){
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
    bnames    =  bnames <- c(paste0(RGBtrans,"_b1"),paste0(RGBtrans,"_b2"),paste0(RGBtrans,"_b3"))
  } 
  return(bnames)
  
}


#' wrapper to preprocess synthetic raster bands from a given RGB and optionally 
#' extracting values according to training vector data
#' @description 
#' The calcex function covers step 1 of the  usecaseRGBClassify workflow for a random forest based classification of visible imagery.
#' The worflow is divided in 4 steps and can be controlled by using the  processing switches of the example (inst/examples/useCaseRGBclassify.R) script :\cr
#'   \itemize{
#'\item \code{startCalcex  = TRUE} start extraction. NOTE: you may set more switsches in the call of calcex
#'\item \code{startTrain  = TRUE} start training. NOTE: you may set more switsches in the call of calcex
#'\item \code{startPredict  = TRUE} start prediction. NOTE: you may set more switsches in the call of calcex
#'}
#' (01) calcex() calculation of spectral indices, basic spatial statistics and textures and
#'               extracting of training values over all channels according to training data\cr\cr
#' (02) ffsTrainModel() training using random forest and the forward feature selection method \cr
#'                      startTrain=TRUE\cr\cr
#' (03) calcex() with respect to the selected predictor variables you may calculate 
#'               the requested channels for all rgb data that you want to predict.\cr\cr
#' (04) prediction startPredict=TRUE\cr\cr
#' (05) for a basic analysis and results extraction have a look at useCaseRGB_analyze.R (highly preliminary!) \cr\cr
#' 
#' @param useTrainData      logical switch for choosing training data (which needs much more/all possible channels) or classification data the necessary ones # useTrainData switch to decide if using training images or classification data (FALSE) default = TRUE
#' @param calculateBands    logical switch for set on calculation of syntheic bands and indices default = TRUE
#' @param extractTrain      logical switch for set on extract training data according to training geometries default = TRUE
#' @param prefixrunFN       prefix of current run default = "train"  
#' @param suffixTrainGeom   suffix of training shape files e.g. index_2017_05_11_RGB_DEFS18_08_TrainingArea.shp default = "TrainingArea"  
#' @param prefixTrainGeom   prefix of training image files e.g. index_2017_05_11_RGB_DEFS18_08_OrthoMosaic.tif default = "index_"  
#' @param channels          optional channels to be choosed options are c("red", "green", "blue")  default =  c("red", "green", "blue")
#' @param hara              logical switch for using  HaralickTextureExtraction default = TRUE \cr
#' for a review of a lot of feature extraction algorithms look at:\href{http://homepages.dcc.ufmg.br/~william/papers/paper_2012_JEI.pdf}{Williams et al, 2012}\cr
#' glcm<-> haralick c("mean"  advanced1, "variance" advanced2 , "homogeneity"simple4, "contrast" simple5, "dissimilarity"advanced2, "entropy" simple2,"second_moment"simple4, "correlation" simple3)
#' using stats will cover mean and variance while dissimilarity is highly correlated to  Homogeneity data. For a nice introduction look at: \href{http://www.fp.ucalgary.ca/mhallbey/more_informaton.htm}{Hallbey}

#' @param haraType          hara options default is c("simple"), other  options are "advanced"  "higher" "all". NOTE:  "higher" takes a LOT of time
#' @param stat              logical switch for using statistic default = TRUE the stas are mean,variance, curtosis, skewness
#' @param edge              logical switch for using edge filtering default = TRUE
#' @param edgeType          edge options default is c("gradient","sobel","touzi") all options are c("gradient","sobel","touzi")
#' @param morpho            logical switch for using morphological filtering default = TRUE
#' @param morphoType        morphological options default is c("dilate","erode","opening","closing") all options are ("dilate","erode","opening","closing")
#' @param indices           RGB indices default is c("VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI") all options are c("VVI","VARI","NDTI","RI","SCI","BI","SI","HI","TGI","GLI","NGRDI","GRVI","GLAI","HUE","CI","SAT","SHP") 
#' @param RGBTrans          logical switch for using color space transforming default = TRUE
#' @param colorSpaces        RGB colorspace transforming to default c("CIELab","CMY","Gray","HCL","HSB","HSI","Log","XYZ","YUV")
#' @param kernel            size of kernel for filtering and statistics default is  3
#' @param currentDataFolder  NULL folder to image (and shape) data
#' @param currentIdxFolder  NULL folder for saving the results
#' @param cleanTiffs  TRUE logical switch for deleting the calculated tifs default is TRUE
#' @param giLinks        list. of GI tools cli pathes  
#' @examples 
#' \dontrun{
#' require(uavRst)
#'devtools::install_github("gisma/link2GI", ref = "master")
#'
#'#---> define environment and settings
#'# define project folder
#'projRootDir <- "~/temp7/GRASS7"
#'
#'# create project structure and export global pathes
#'link2GI::initProj(projRootDir = projRootDir,
#'                  projFolders = c("data/",
#'                                  "data/training/",
#'                                  "data/training/idx/",
#'                                  "output/",
#'                                  "run/",
#'                                  "fun/") )
#'# set working directory
#'setwd(path_run)
#'res <- calcex( useTrainData      = TRUE, 
#'               calculateBands    = TRUE, 
#'               extractTrain      = TRUE, 
#'               prefixrunFN       = "traddel",
#'               suffixTrainGeom   = "TrainingArea",
#'               prefixTrainGeom   = "index_", 
#'               indices           = c("VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI") , 
#'               channels          = c("red", "green", "blue"),  
#'               hara              = FALSE,
#'               haraType          = c("simple","advanced","higher"),   
#'               stat              = TRUE, 
#'               edge              = TRUE, 
#'               edgeType          = c("gradient","sobel","touzi"), 
#'               morpho            = TRUE, 
#'               morphoType        = c("dilate","erode","opening","closing"), 
#'               kernel            = 3, 
#'               currentDataFolder = path_data_training,
#'               currentIdxFolder  = path_data_training_idx)
#'}
#' @import crayon               
#' @export calcex


calcex<- function ( useTrainData      = TRUE,
                    calculateBands    = FALSE,
                    extractTrain      = TRUE,
                    prefixrunFN       = "train",
                    suffixTrainGeom   = "TrainingArea",
                    prefixTrainGeom   = "index_",
                    channels          = c("red", "green", "blue"),
                    hara              = TRUE,
                    haraType          = c("simple","advanced","higher"),
                    stat              = TRUE, 
                    edge              = TRUE, 
                    edgeType          = c("gradient","sobel","touzi"),
                    morpho            = TRUE, 
                    morphoType        = c("dilate","erode","opening","closing"),
                    indices           = c("VVI","VARI","NDTI","RI","SCI","BI",
                                          "SI","HI","TGI","GLI","NGRDI","GRVI",
                                          "GLAI","HUE","CI","SAT","SHP") , 
                    RGBTrans          = TRUE,
                    colorSpaces       = c("CIELab","CMY","Gray","HCL","HSB","HSI","Log","XYZ","YUV"),
                    kernel            = 3, 
                    currentDataFolder = NULL,
                    currentIdxFolder  = NULL,
                    cleanTiffs        = TRUE,
                    giLinks = NULL){
  
  catHead  <- black $ bgGreen
  catErr <- red $ bold
  catNote <- blue $ bold
  catOk <- green $ bold
  
  
  if (useTrainData) {
    currentDataFolder<- currentDataFolder #paste0(path_data_training)
    currentIdxFolder<- currentIdxFolder # paste0(path_data_training_idx)
  }
  if (!useTrainData) {
    currentDataFolder<- currentDataFolder #paste0(path_data)
    currentIdxFolder<- currentIdxFolder #paste0(path_data_idx)
  }
  
  # link GDAL and OTB
  gdal <- link2GI::linkGDAL()
  link<-link2GI::linkOTB()
  path_OTB <-link$pathOTB
  if ((stat == TRUE || hara == TRUE || edge == TRUE || morpho == TRUE) & path_OTB == "") stop("OTB missing - please check")
  
  ### ----- start preprocessing ---------------------------------------------------
  if (calculateBands) {
    cat(catHead("\n     ---------------- preprocess input data ------------------                \n"))
    
    # create list of image files to be processed 
    # NOTE all subfolder below c("data/","output/","run/","fun","idx") have to created individually
    imageFiles <- list.files(pattern="[.]tif$", path=currentDataFolder, full.names=TRUE)
    
    # stack the ortho images
    #rgb<- lapply(imageFiles, FUN=raster::raster)
    
    
    ### calculate indices and base stat export it to tif
    # create list vars
    flist<-list()
    
    # for all images do
    for (i in 1:length(imageFiles)){
      cat(catNote(":::: processing indices of...",basename(imageFiles[i]),"\n"))
      r<-raster::stack(imageFiles[i])
      # calculate and stack r,g,b and requested indices
      rgb_rgbi<-raster::stack(r[[1:3]],uavRst::rgbIndices(r[[1]],r[[2]],r[[3]],indices))
      bnames <- uavRst::makebNames(rgbi = indices)
      names(rgb_rgbi)<-bnames 
      cat(catOk("\n     save ...",paste0("rgbi_",basename(imageFiles[i])),"\n"))
      raster::writeRaster(rgb_rgbi,
                          paste0("rgbi_",basename(imageFiles[i])),
                          progress = "text",                        
                          overwrite=TRUE)
      flist<-paste0("rgbi_",basename(imageFiles[i]))
      # if RGB transform
      if (RGBTrans){
        
        cat(catNote(":::: processing color transformation...\n"))
        uavRst::imageMagickconvert(input = imageFiles[i],
                                   colorspace = colorSpaces)
        rgbtranslist<-list()
        jj=1
        for (colMod in colorSpaces) {
          rgbtranslist[[jj]]<-paste0(colMod,"_",basename(imageFiles[i]))
          jj<-jj+1
        }
        rt<- lapply(rgbtranslist, FUN=raster::stack)
        for (jj in 1:length(rt)) {
          extent(rt[[jj]])<-extent(r)
          projection(rt[[jj]]) <- CRS(projection(r))
          cat(catOk(":::: save... ",colorSpaces[jj],"_",basename(imageFiles[i]),"\n"))
          raster::writeRaster(stack(rt[[jj]][[1:3]]),
                              paste0(colorSpaces[jj],"_ref",basename(imageFiles[i])),
                              overwrite=TRUE,
                              options="INTERLEAVE=BAND",
                              progress="text")
          bnames <-append(bnames,makebNames(RGBtrans = colorSpaces[jj]))
          flist<-append(flist, paste0(colorSpaces[jj],"_ref",basename(imageFiles[i])))
        }
        file.remove(unlist(rgbtranslist))
        #r<-raster::stack(imageFiles[i])
        
        #bnames <-append(bnames,makebNames(RGBtrans = colorSpaces))
        
      }
      
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
                            paste0(filterBand,"_",basename(imageFiles[i])), 
                            progress = "text",
                            overwrite=TRUE)
        fbFN<-paste0(filterBand,"_",basename(imageFiles[i]))
        # } else {
        #   fbFN<- imageFiles[i]
        #   filterBand<-list("red","green","blue")
        # }
        # if calc statistcis 
        if (stat){
          cat(catNote(":::: processing stats...",fbFN,"\n"))
          otbLocalStat(input = fbFN,
                       out = paste0(filterBand,"stat_",basename(imageFiles[i])),
                       ram = "4096",
                       radius =  kernel)
          flist<-append(flist,Sys.glob(paste0("*",paste0(filterBand,"stat_",basename(imageFiles[i])),"*")))
          bnames <-append(bnames,paste0(makebNames(stat = TRUE),"_",filterBand))
        }
        # if calc edge
        if (edge){
          for (edges in edgeType){
            cat(catNote(":::: processing edge... ",edges,"\n"))
            uavRst::otbEdge(input = fbFN,
                            out = paste0(filterBand,edges,basename(imageFiles[i])),
                            filter = edges)
            
            flist<-append(flist,Sys.glob(paste0("*",paste0(filterBand,edges,basename(imageFiles[i])),"*")))
            bnames <-append(bnames,makebNames(edge = paste0(edges,"_",filterBand)))
          }
        }    
        # if calc morpho
        if (morpho){
          for (morphos in morphoType){
            cat(catNote(":::: processing morpho... ",morphos,"\n"))
            uavRst::otbGrayMorpho(input = fbFN,
                                  out = paste0(filterBand,morphos,basename(imageFiles[i])),
                                  filter = morphos)
            flist<-append(flist,Sys.glob(paste0("*",paste0(filterBand,morphos,basename(imageFiles[i])),"*")))
            bnames <-append(bnames,makebNames(edge = paste0(morphos,"_",filterBand)))
          }    
        }
        # if calc haralick
        if (hara){
          for (type in haraType){
            cat(catNote(":::: processing haralick... ",type,"\n"))
            uavRst::otbTexturesHaralick(x = fbFN,
                                        output_name=paste0(filterBand,"hara_",basename(imageFiles[i])),
                                        texture = type)
            flist<-append(flist,Sys.glob(paste0("*",paste0(filterBand,"hara_",basename(imageFiles[i])),"*")))
            bnames <-append(bnames,paste0(makebNames(bandNames = type),"_",filterBand))
          }
        }
        # delete single channel for synthetic channel calculation
        file.remove(paste0(filterBand,"_",basename(imageFiles[i])))
      }
      # get the rest in a list
      #flist<-(Sys.glob(paste0("*",basename(imageFiles[i]),"*")))
      # end of single channnel calculation
      
      # create an alltogether stack
      tmpFN<-paste0(substr(basename(imageFiles[i]),1,nchar(basename(imageFiles[i]))-4))
      cat(catOk("     save ...",prefixTrainGeom, tmpFN,"\n"))
      # r<-raster::brick(raster::stack(flist)) qgis cannot read heder
      r<-(raster::stack(flist))
      if (raster::nlayers(r)!=length(bnames)) stop("\n Number of names and layers differ...\n most common case is a broken cleanup of the runtime directory!")
      names(r)<-bnames
      # write file to envi
      raster::writeRaster(r,
                          paste0(currentIdxFolder,"/", prefixTrainGeom,tmpFN),
                          format="ENVI", 
                          progress ="text",
                          #options="COMPRESS=LZW",
                          overwrite=TRUE)
      #raster::hdr(r, filename = paste0(currentIdxFolder,"/", prefixTrainGeom,tmpFN), format = "ENVI") qgis cannot read heder
      
      # cleanup runtime files lists...
      if (cleanTiffs) {
        cat(catNote(":::: removing temp files...\n"))
        file.remove(flist)
      }
      flist<-list()
    }
    
    # save bandname list we need it only once
    save(bnames,file = paste0(currentIdxFolder,"bandNames_",prefixrunFN,".RData"))
    
    
    cat(catHead("\n     ---------------- finished preprocessing RGB data ------------------                \n"))
  }
  # ----- start extraction ---------------------------------------------------
  if (extractTrain){
    cat(catHead("\n     --------------- start extract processing ------------------                \n"))
    load(paste0(currentIdxFolder,"bandNames_",prefixrunFN,".RData"))
    # get image and geometry data for training purposes
    imageTrainFiles <- list.files(pattern="[.]envi$", path=currentIdxFolder, full.names=TRUE)
    tmp  <- basename(list.files(pattern="[.]envi$", path=currentIdxFolder, full.names=TRUE))
    geomTrainFiles<-paste0(currentDataFolder,substr(tmp,nchar(prefixTrainGeom)+1,nchar(tmp)-(nchar(suffixTrainGeom)+4)),suffixTrainGeom,".shp")
    
    imageTrainStack<-lapply(imageTrainFiles, FUN=raster::stack)
    geomTrainStack  <- lapply(geomTrainFiles, FUN=raster::shapefile)
    
    # extract clean and format training data
    
    trainDF <- uavRst::extractTrainData(rasterStack  = imageTrainStack,
                                        trainPlots = geomTrainStack,
                                        bnames = bnames,
                                        imageTrainFiles
                                        
    )
    # create a new dataframe with prefixrunFN
    assign(paste0(prefixrunFN,"_trainDF"), trainDF,envir = )
    # save it 
    saveRDS(eval(parse(text=paste0(prefixrunFN,"_trainDF"))), paste0(currentIdxFolder,prefixrunFN,"_trainDF",".rds"))
    #read it into another name 
    #DF<-readRDS(paste0(currentIdxFolder,prefixrunFN,"_trainDF",".rds"))  
    cat(catHead("\n     --------------- stop start extract processing ------------------                \n"))
    return(trainDF)
  }
}
