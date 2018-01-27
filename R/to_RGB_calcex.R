#' calculation of spectral indices, basic spatial statistics and textures and
#'      optionally extracting pix values over all channels according to vector data
#' @description 
#' The usecaseRGB functions are providing a common workflow for a random forest based classification of visible imagery.
#' The worflow is divided in 4 steps:\cr\cr
#' (01) calculation of spectral indices, basic spatial statistics and textures and
#'      extracting of training values over all channels according to training data
#'      (calcex)\cr\cr
#' (02) model training using random forest and the forward feature selection method
#'      (02_useCaseRGB_train.R)\cr\cr
#' (03) calculation spectral indices, basic spatial statistics and textures for 
#'      all rgb data according to the model requests (01_useCaseRGB_calcex.R)\cr\cr
#' (04) prediction (02_useCaseRGB_predict.R)\cr\cr
#' (05) basic analysis and results extraction (04_useCaseRGB_analyze.R)\cr\cr
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
#' @param indices           RGB indices default is c("VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI") all options are c("VARI","NDTI","TGI","GLI","NGRDI","GLAI") 
#' @param kernel            size of kernel for filtering and statistics default is  3
#' @param currentDataFolder  NULL folder to image (and shape) data
#' @param  currentidxFolder  NULL folder for saving the results
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
#'                  projFolders = c("data/","data/training/","data/training/idx/","data/training/idx/","output/","output/index/","run/","fun/") )
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
#'               haraType          = c("simple"),   
#'               stat              = TRUE, 
#'               edge              = TRUE, 
#'               edgeType          = c("gradient","sobel","touzi"), 
#'               morpho            = TRUE, 
#'               morphoType        = c("dilate","erode","opening","closing"), 
#'               kernel            = 3, 
#'               currentDataFolder = path_data_training,
#'               currentIdxFolder  = path_data_training_idx)
#'}
#'               
#' @export calcex
  

calcex<- function ( useTrainData      = TRUE,
                    calculateBands    = TRUE,
                    extractTrain      = TRUE,
                    prefixrunFN       = "train",
                    suffixTrainGeom   = "TrainingArea",
                    prefixTrainGeom   = "index_",
                    channels          = c("red", "green", "blue"),
                    hara              = TRUE,
                    haraType          = c("simple"),
                    stat              = TRUE, 
                    edge              = TRUE, 
                    edgeType          = c("gradient","sobel","touzi"),
                    morpho            = TRUE, 
                    morphoType        = c("dilate","erode","opening","closing"),
                    indices           = c("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI") , 
                    kernel            = 3, 
                    currentDataFolder = NULL,
                    currentIdxFolder  = NULL){
  
if (useTrainData) {
  currentDataFolder<- currentDataFolder #paste0(path_data_training)
  currentIdxFolder<- currentIdxFolder # paste0(path_data_training_idx)
}
if (!useTrainData) {
  currentDataFolder<- currentDataFolder #paste0(path_data)
  currentIdxFolder<- currentIdxFolder #paste0(path_data_idx)
}

# link GDAL and OTB
gdal <- link2GI::linkgdalUtils()
link2GI::linkOTB()

if ((stat == TRUE || hara == TRUE || edge == TRUE || morpho == TRUE) & path_OTB == "") stop("OTB missing - please check")

### ----- start preprocessing ---------------------------------------------------
if (calculateBands) {
  cat("\n::: preprocess input data...\n")
  
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
    cat(":::: processing indices of...",basename(imageFiles[i]),"\n")
    r<-raster::stack(imageFiles[i])
    # calculate and stack r,g,b and requested indices
    rgb_rgbi<-raster::stack(r[[1:3]],uavRst::rgbIndices(r[[1]],r[[2]],r[[3]],indices))
    bnames <- uavRst::makebNames(rgbi = indices)
    names(rgb_rgbi)<-bnames 
    cat("\n     save ...",paste0("rgbi_",basename(imageFiles[i])),"\n")
    raster::writeRaster(rgb_rgbi,
                        paste0("rgbi_",basename(imageFiles[i])),
                        progress = "text",                        
                        overwrite=TRUE)
    
    # assign bandnumber according to name
    cat("\n")
    for (filterBand in channels){
      if (filterBand=="red") bandNr <- 1
      if (filterBand=="green") bandNr <- 2
      if (filterBand=="blue") bandNr <- 3
      # export single channel for synthetic band calculation
      # if (filterBand!="") {
      cat("     save ...",paste0(filterBand,"_",basename(imageFiles[i])),"\n")
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
        cat(":::: processing stats...",fbFN,"\n")
        otbLocalStat(input = fbFN,
                     out = paste0(filterBand,"stat_",basename(imageFiles[i])),
                     ram = "4096",
                     radius =  kernel)
        bnames <-append(bnames,paste0(makebNames(stat = TRUE),"_",filterBand))
      }
      # if calc edge
      if (edge){
        for (edges in edgeType){
          cat(":::: processing edge... ",edges,"\n")
          uavRst::otbEdge(input = fbFN,
                          out = paste0(filterBand,edges,basename(imageFiles[i])),
                          filter = edges)
          bnames <-append(bnames,makebNames(edge = paste0(edges,"_",filterBand)))
        }
      }    
      # if calc morpho
      if (morpho){
        for (morphos in morphoType){
          cat(":::: processing morpho... ",morphos,"\n")
          uavRst::otbGrayMorpho(input = fbFN,
                                out = paste0(filterBand,morphos,basename(imageFiles[i])),
                                filter = morphos)
          bnames <-append(bnames,makebNames(edge = paste0(morphos,"_",filterBand)))
        }    
      }
      # if calc haralick
      if (hara){
        for (haras in haraType){
          cat(":::: processing haralick... ",haras,"\n")
          uavRst::otbTexturesHaralick(x = fbFN,
                                      output_name=paste0(filterBand,"hara_",basename(imageFiles[i])),
                                      texture = haras)
          bnames <-append(bnames,paste0(makebNames(hara = haras),"_",filterBand))
        }
      }
      # delete single channel for synthetic channel calculation
      file.remove(paste0(filterBand,"_",basename(imageFiles[i])))
    }
    # get the rest in a list
    flist<-(Sys.glob(paste0("*",basename(imageFiles[i]),"*")))
    # end of single channnel calculation
    
    # create an alltogether stack
    r<-raster::stack(flist)
    if (raster::nlayers(r)!=length(bnames)) stop("\n Number of names and layers differ...\n most common case is a broken cleanup of the runtime directory!")
    names(r)<-bnames
    
    # extract filename    
    tmpFN<-paste0(substr(basename(imageFiles[i]),1,nchar(basename(imageFiles[i]))-4))
    cat("     save ...",prefixTrainGeom, tmpFN,"\n")

    # write file to envi
    raster::writeRaster(r,
                        paste0(currentIdxFolder,"/", prefixTrainGeom,tmpFN),
                        format="ENVI", 
                        progress ="text",
                        overwrite=TRUE)

    # cleanup runtime files lists...
    cat(":::: removing temp files...\n")
    file.remove(flist)
    flist<-list()
  }
  
  # save bandname list we need it only once
  save(bnames,file = paste0(currentIdxFolder,"bandNames_",prefixrunFN,".RData"))
  
  cat(":::: finished preprocessing RGB data...\n")
}
# ----- start extraction ---------------------------------------------------
if (extractTrain){
  # get image and geometry data for training purposes
  imageTrainFiles <- list.files(pattern="[.]envi$", path=currentIdxFolder, full.names=TRUE)
  tmp  <- basename(list.files(pattern="[.]envi$", path=currentIdxFolder, full.names=TRUE))
  geomTrainFiles<-paste0(currentDataFolder,substr(tmp,nchar(prefixTrainGeom)+1,nchar(tmp)-(nchar(suffixTrainGeom)+4)),suffixTrainGeom,".shp")
  
  imageTrainStack<-lapply(imageTrainFiles, FUN=raster::stack)
  geomTrainStack  <- lapply(geomTrainFiles, FUN=raster::shapefile)
  
  # extract clean and format training data
  
  trainDF <- uavRst::extractTrainData(rasterStack  = imageTrainStack,
                                      trainPlots = geomTrainStack,
                                      load(paste0(currentIdxFolder,"bandNames_",prefixrunFN,".RData")),
                                      rasFN
  )
  # create a new dataframe with prefixrunFN
  assign(paste0(prefixrunFN,"_trainDF"), trainDF,envir = )
  # save it 
  saveRDS(eval(parse(text=paste0(prefixrunFN,"_trainDF"))), paste0(currentIdxFolder,prefixrunFN,"_trainDF",".rds"))
  #read it into another name 
  #DF<-readRDS(paste0(currentIdxFolder,prefixrunFN,"_trainDF",".rds"))  
  cat(":::: extraction...finsihed \n")
  return(trainDF)
}
}