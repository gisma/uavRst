if (!isGeneric('get_traindata')) {
  setGeneric('get_traindata', function(x, ...)
    standardGeneric('get_traindata'))
}

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
#'@param imgFN        character.  default is \code{file.path(tempdir(),"trainingDF.RData")} Name of the extracted training data file
#'@param bandNames       character. names of the bands
#'@import crayon
#'@export get_traindata
#'@examples
#'\dontrun{
#'
#' trainingDF <- get_traindata(rasterStack  = trainStack,
#'                                training     = training,
#'                                ids=c(1,2),
#'                                idLabel= c("green","nogreen"))
#'}

get_traindata<-function(rasterStack  = NULL,
                           trainPlots     = NULL,
                           bandNames = NULL,
                           imgFN) {

  catNote <- crayon::blue $ bold


  cat(catNote("\n:::: extract trainPlots data...\n"))
  trainingDF =  data.frame()
  # extract trainPlots Area pixel values
  # TODO https://gis.stackexchange.com/questions/253618/r-multicore-approach-to-extract-raster-values-using-spatial-points
  for (j in 1:length(rasterStack)) {
    cat("\n    extracting trainPlots data from image ",j," of ... ",length(rasterStack))

    categorymap<-rgeos::gUnionCascaded(trainPlots[[j]],id=trainPlots[[j]]@data$id)
    dataSet <- raster::extract(rasterStack[[j]], categorymap,df=TRUE)
    names(dataSet)<-append(c("ID"),bandNames)
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

#' counts pixel values according to their classes
#'
#' @param ids numeric. the ids used for the training 
#' @param position sp. spatialpoint object containing the centre target positions  
#' @param  imageFiles raster* image/classification file 
#' @param out_prefix character. out prefix string
#' @param ext character. extension
#' @param path   character. output path
#' @param dropChars numeric. number of characters that should be dropped at the end of the filename
#' @param buffersize numeric. radius in meters around position 
#'
#' @export get_counts
#' @examples
#' \dontrun{
#' df1<-get_counts(position = position,
#'               imageFiles = imageFiles,
#'               dropChars = 8,
#'               pre=pre,
#'               ext=".tif")
#'}

get_counts<- function(ids=c(1,2),
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
#' @param imageFiles raster*. imagestack for classification purposes must contain the required bands as needed by the model.
#' @param model model. classification model
#' @param  in_prefix character. in frefix  string
#' @param out_prefix character. out prefix string
#' @param bandNames character. band names
#'
#' @export predict_rgb
#' @examples
#' \dontrun{
#' predict_rgb(imageFiles=imagestack,
#'             model = model_final,
#'             in_prefix = "index_",
#'             out_prefix = "classified_",
#'             bandNames = c("R","G","B","A","VARI","NDTI","TGI","GLI","NGRDI"))
#'}

predict_rgb <- function(imageFiles=NULL,
                       model = NULL,
                       in_prefix = "index_",
                       out_prefix = "classified_",
                       bandNames = NULL) {
  
  if (is.null(bandNames)) return(cat(crayon()[[1]]("\n you did not provide predictor names. \nTypically something like bandNames ie c('R','G','B')")))
  po = path_output
  i = 1:length(imageFiles)
  cat("\n::: start prediction aka classifikation...\n")
  cl <- parallel::makeCluster(parallel::detectCores())
  doParallel::registerDoParallel	(cl)
  foreach::foreach(i,po) %dopar% {
    #for (i in 1:length(imageFiles)) {
    requireNamespace("raster")
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

#' #TOFIX @titel Forward feature selection based random forest model training
#' @description ffs_train is a wrapper function for a simple use of the forward feature selection approach
#' of training random forest classification models. This validation is particulary suitable for
#' leave-location-out cross validations where variable selection
#' MUST be based on the performance of the model on the hold out station.
#' See \href{https://www.sciencedirect.com/science/article/pii/S1364815217310976}{Meyer et al. (2018)}
#' for further details.
#' This is in fact the case while using time space variable vegetation patterns for classification purposes.
#' For the UAV based RGB/NIR imagery, it provides an optimized preconfiguration for the classification goals.
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
#' @param sumFunction   character. function to summarize default is "twoClassSummary"
#' @export ffs_train
#' @examples
#' \dontrun{
#' result<-  ffs_train(trainingDF = trainingDF,
#'                      predictors   = c("R","G","B"),
#'                      response     = "ID",
#'                      spaceVar     = "FN",
#'                      names        = c("ID","R","G","B","A","FN"),
#'                      noLoc        = length(imageTrainFiles),
#'                      metric_ffs   = "kappa",
#'                      pVal         = 0.5)
#'                  }

ffs_train<-function(   trainingDF   = NULL,
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

  # make it parallel
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



#' #TOFIX @titel Convenient function to preprocess synthetic raster bands from a given RGB and optionally
#' extract the raster values on base of vector data for training purposes.
#' @description
#' The calc_ext function covers step 1 of the  usecaseRGBClassify workflow for a random forest based classification of visible imagery.

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


#' @param calculateBands    logical. switch for set on calculation of syntheic bands and indices default = TRUE
#' @param extractTrain      logical. switch for set on extract training data according to training geometries default = TRUE
#' @param patternImgFiles   character. mandantory string that exist in ech imagefile to be processes
#' @param patternIdx         character. code string that will concatenated to the filename to identify the index file
#' @param prefixrunFN       character. general prefix of all result files of the current run default = "tmp"
#' @param prefixdemFN       character. mandantory if DEM data is processes. prefix of current DEM default = "dem"
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
#' @param RGBTrans          logical. switch for using color space transforming default = TRUE
#' @param colorSpaces       character.  RGB colorspace transforming to default c("CIELab","CMY","Gray","HCL","HSB","HSI","Log","XYZ","YUV")
#' @param kernel            numeric. size of kernel for filtering and statistics, default is  3
#' @param morpho_method  numeric. saga morphometric method 
#' @param min_scale  mnumeric. in scale for multi scale TPI
#' @param max_scale  numeric. max scale for multi scale TPI
#' @param num_scale  numeric. number of scale for multi scale TPI
#' @param currentDataFolder  NULL folder to image (and shape) data
#' @param currentIdxFolder  NULL folder for saving the results
#' @param cleanTiffs  logical. TRUE logical switch for deleting the calculated tifs, default is TRUE
#' @param giLinks     list. GI tools cli paths
#' @examples
#' \dontrun{
#' require(uavRst)
#' devtools::install_github("gisma/link2GI", ref = "master")
#' 
#'#---> define environment and settings
#' projRootDir <- "~/temp7/GRASS7"
#'
#'# create project structure and export global pathes
#' projRootDir<-tmpDir()
#' setwd(paste0(projRootDir,"run"))
#' link2GI::initProj(projRootDir = projRootDir,
#'                  projFolders = c("data/","data/training/","data/training/idx/",
#'                                  "output/","run/","fun/"),
#'                                  global = TRUE,
#'                                  path_prefix = "path_")
#'                                  
#'res <- calc_ext(calculateBands    = TRUE,
#'                extractTrain      = TRUE,
#'                prefixrunFN       = "traddel",
#'                suffixTrainGeom   = "TrainingArea",
#'                patternIdx   = "index",
#'                indices           = c("VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI") ,
#'                channels          = c("red", "green", "blue"),
#'                hara              = FALSE,
#'                haraType          = c("simple","advanced","higher"),
#'                stat              = TRUE,
#'                edge              = TRUE,
#'                edgeType          = c("gradient","sobel","touzi"),
#'                morpho            = TRUE,
#'                morphoType        = c("dilate","erode","opening","closing"),
#'                kernel            = 3,
#'                currentDataFolder = path_data_training,
#'                currentIdxFolder  = path_data_training_idx)
#'}
#' @import crayon
#' @export calc_ext


calc_ext<- function ( calculateBands    = FALSE,
                    extractTrain      = TRUE,
                    prefixrunFN       = "temp",
                    prefixdemFN       = "dem",
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
                    RGBTrans          = TRUE,
                    colorSpaces       = c("CIELab","CMY","Gray","HCL","HSB","HSI","Log","XYZ","YUV"),
                    pardem            = TRUE,
                    demType           = c("hillshade","slope", "aspect","TRI","TPI","Roughness",
                                          "SLOPE","ASPECT", "C_GENE","C_PROF","C_PLAN","C_TANG",
                                          "C_LONG","C_CROS","C_MINI","C_MAXI","C_TOTA","C_ROTO","MTPI"),  
                    morpho_method = 6,
                    min_scale = 1,
                    max_scale = 8,
                    num_scale = 2,
                    kernel            = 3,
                    currentDataFolder = NULL,
                    currentIdxFolder  = NULL,
                    cleanTiffs        = TRUE,
                    giLinks = NULL){

  if (!rgbi) RGBTrans <- hara <- stat <- edge <- morpho <- FALSE
  if (is.null(giLinks)){
    giLinks <- get_gi()
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

  if (nchar(prefixrunFN)>0)   prefixrunFN <-  paste0(prefixrunFN,"_")
  if (nchar(prefixdemFN)>0)   prefixdemFN <-  paste0(prefixdemFN,"_")
  if (nchar(prefixTrainImg)>0)   prefixTrainImg <-  paste0(prefixTrainImg,"_")
  if (nchar(patternIdx)>0)   patternIdx <-  paste0(patternIdx,"_")
  if (nchar(suffixTrainGeom)>0)   suffixTrainGeom <-  paste0("_",suffixTrainGeom)
  if (nchar(suffixTrainImg)>0)   suffixTrainImg <-  paste0("_",suffixTrainImg)
  
  currentDataFolder<- currentDataFolder #paste0(path_data_training)
  currentIdxFolder<- currentIdxFolder # paste0(path_data_training_idx)

  if ((stat == TRUE || hara == TRUE || edge == TRUE || morpho == TRUE) & path_OTB == "") stop("OTB missing - please check")

  ### ----- start preprocessing ---------------------------------------------------
  if (calculateBands) {
    cat(catHead("\n     ---------------- preprocess input data ------------------                \n"))

    # create list of image files to be processed
    # NOTE all subfolder below c("data/","output/","run/","fun","idx") have to created individually

    #imageFiles <- list.files(pattern=paste0("^",prefixrunFN,"*","tif"), path=currentDataFolder, full.names=TRUE)
    imageFiles <-Sys.glob(path=paste0(currentDataFolder,patternImgFiles,"*","tif"))
    demFiles <- Sys.glob(path=paste0(currentDataFolder,prefixdemFN,"*","tif"))
    counter<- max(length(demFiles),length(imageFiles))
    # stack the ortho images
    ### calculate indices and base stat export it to tif
    # create list vars
    flist<-list()

      bandNames<-NULL
      for (i in 1:length(counter)){
        # if calc pardem 
        if (pardem){

        #cat(catNote(":::: processing dem... ",demType,"\n"))
        morpho_dem(dem = demFiles[i], 
                   item = demType,
                   morpho_method = morpho_method,
                   min_scale = min_scale,
                   max_scale = max_scale,
                   num_scale = num_scale,
                   giLinks = giLinks)
        flist<-append(flist,paste0(demType,".tif"))
        for (item in demType) 
          bandNames <-append(bandNames,make_bandnames(dem = item))
        
      } 
    }

    # for all images do
    for (i in 1:length(imageFiles)){
      if (rgbi){
      cat(catNote(":::: processing indices of...",basename(imageFiles[i]),"\n"))
      r<-raster::stack(imageFiles[i])
      # calculate and stack r,g,b and requested indices
      rgb_rgbi<-raster::stack(r[[1:3]],uavRst::rgb_indices(r[[1]],r[[2]],r[[3]],indices))
      bandNames <- uavRst::make_bandnames(rgbi = indices)
      names(rgb_rgbi)<-bandNames
      cat(catOk("\n     save ...",paste0("rgbi_",basename(imageFiles[i])),"\n"))
      raster::writeRaster(rgb_rgbi,
                          paste0("rgbi_",basename(imageFiles[i])),
                          progress = "text",
                          overwrite=TRUE)
      flist<-paste0("rgbi_",basename(imageFiles[i])) 
      }
      # if RGB transform
      if (RGBTrans){

        cat(catNote(":::: processing color transformation...\n"))
        uavRst::colorspace(input = imageFiles[i],
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
          bandNames <-append(bandNames,make_bandnames(RGBtrans = colorSpaces[jj]))
          flist<-append(flist, paste0(colorSpaces[jj],"_ref",basename(imageFiles[i])))
        }
        file.remove(unlist(rgbtranslist))
        #r<-raster::stack(imageFiles[i])

        #bandNames <-append(bandNames,make_bandnames(RGBtrans = colorSpaces))

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
                            paste0(filterBand,"_",basename(imageFiles[i])),
                            progress = "text",
                            overwrite=TRUE)
        fbFN<-paste0(filterBand,"_",basename(imageFiles[i]))
}
        if (stat){
          cat(catNote(":::: processing stats...",fbFN,"\n"))
          otb_stat(input = fbFN,
                       out = paste0(filterBand,"stat_",basename(imageFiles[i])),
                       ram = "4096",
                       radius =  kernel,
                       giLinks=giLinks)
          flist<-append(flist,Sys.glob(paste0("*",paste0(filterBand,"stat_",basename(imageFiles[i])),"*")))
          bandNames <-append(bandNames,paste0(make_bandnames(stat = TRUE),"_",filterBand))
        }
        # if calc edge
        if (edge){
          for (edges in edgeType){
            cat(catNote(":::: processing edge... ",edges,"\n"))
            uavRst::otbtex_edge(input = fbFN,
                            out = paste0(filterBand,edges,basename(imageFiles[i])),
                            filter = edges,
                            giLinks=giLinks)

            flist<-append(flist,Sys.glob(paste0("*",paste0(filterBand,edges,basename(imageFiles[i])),"*")))
            bandNames <-append(bandNames,make_bandnames(edge = paste0(edges,"_",filterBand)))
          }
        }
        
        # if calc morpho
        if (morpho){
          for (morphos in morphoType){
            cat(catNote(":::: processing morpho... ",morphos,"\n"))
            uavRst::otbtex_gray(input = fbFN,
                                  out = paste0(filterBand,morphos,basename(imageFiles[i])),
                                  filter = morphos,
                                  giLinks=giLinks)
            flist<-append(flist,Sys.glob(paste0("*",paste0(filterBand,morphos,basename(imageFiles[i])),"*")))
            bandNames <-append(bandNames,make_bandnames(edge = paste0(morphos,"_",filterBand)))
          }
        }
        # if calc haralick
        if (hara){
          for (type in haraType){
            cat(catNote(":::: processing haralick... ",type,"\n"))
            uavRst::otbtex_hara(x = fbFN,
                                        output_name=paste0(filterBand,"hara_",basename(imageFiles[i])),
                                        texture = type,
                                        giLinks=giLinks)
            flist<-append(flist,Sys.glob(paste0("*",paste0(filterBand,"hara_",basename(imageFiles[i])),"*")))
            bandNames <-append(bandNames,paste0(make_bandnames(bandNames = type),"_",filterBand))
          }
        }
        # delete single channel for synthetic channel calculation
        file.remove(paste0(filterBand,"_",basename(imageFiles[i])))
      }
      # get the rest in a list
      #flist<-(Sys.glob(paste0("*",basename(imageFiles[i]),"*")))
      # end of single channnel calculation

      # create an alltogether stack
      if (rgbi)  tmpFN<-paste0(substr(basename(imageFiles[i]),1,nchar(basename(imageFiles[i]))-4))
      else if (length(demFiles)>= i)  tmpFN<-paste0(substr(basename(demFiles[i]),1,nchar(basename(demFiles[i]))-4))
      else return(cat(catErr("\nhopefully done\n You are mixing RGB an DEM input files. You may do this but only if they are of the same extent etc. and if each image file has a corresponding dem file\n NOTE the dem filename MUST have a prefix default is 'dem_'.")))
      cat(catOk("     save ...",paste0(patternIdx, tmpFN),"\n"))
      # r<-raster::brick(raster::stack(flist)) qgis cannot read heder
      r<-raster::stack(paste0(path_run,flist))
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
      if (cleanTiffs) {
        cat(catNote(":::: removing temp files...\n"))
        res<-file.remove(unlist(flist))
      }
      flist<-list()
    }

    # save bandname list we need it only once
    save(bandNames,file = paste0(currentIdxFolder,"bandNames_",prefixrunFN,".RData"))


    cat(catHead("\n     ---------------- finished preprocessing RGB data ------------------                \n"))
  }
  # ----- start extraction ---------------------------------------------------
  if (extractTrain){
    cat(catHead("\n     --------------- start extract processing ------------------                \n"))
    load(paste0(currentIdxFolder,"bandNames_",prefixrunFN,".RData"))
    # get image and geometry data for training purposes
    imageTrainFiles <- list.files(pattern="[.]envi$", path=currentIdxFolder, full.names=TRUE)
    tmp  <- basename(list.files(pattern="[.]envi$", path=currentIdxFolder, full.names=TRUE))
    tmp<- gsub(patternIdx,prefixTrainGeom,tmp)
    tmp<- gsub(suffixTrainImg,suffixTrainGeom,tmp)
    geomTrainFiles <- gsub(".envi",".shp",tmp)
    geomTrainFiles <- paste0(currentDataFolder,geomTrainFiles)
    imageTrainStack<-lapply(imageTrainFiles, FUN=raster::stack)
    if (file.exists(extension(geomTrainFiles, ".shp")))
    geomTrainStack  <- lapply(geomTrainFiles, FUN=raster::shapefile)
    else 
      return(cat(catErr("\nTraining files are not existing please check suffix or prefix strings")))
    # extract clean and format training data

    trainDF <- uavRst::get_traindata(rasterStack  = imageTrainStack,
                                        trainPlots = geomTrainStack,
                                        bandNames = bandNames,
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
