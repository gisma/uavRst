# 01_useCaseRGB_calcex.R
#
# The usecaseRGB 01-04 scripts are providing a common workflow for a random forest based classification of visible imagery.
# The worflow is divided in 4 steps:
# (01) calculation of spectral indices, basic spatial statistics and textures and
#      extracting of training values over all channels according to training data
#      (01_useCaseRGB_calcex.R)
# (02) model training using random forest and the forward feature selection method
#      (02_useCaseRGB_train.R)
# (03) calculation spectral indices, basic spatial statistics and textures for 
#      all rgb data according to the model requests (01_useCaseRGB_calcex.R)
# (04) prediction (02_useCaseRGB_predict.R)
# (05) basic analysis and results extraction (04_useCaseRGB_analyze.R)

#devtools::install_github("gisma/uavRst", ref = "master")
require(uavRst)
devtools::install_github("gisma/link2GI", ref = "master")
# require(link2GI)
# require(CAST)
# require(raster)
# require(foreach)
# require(doParallel)

rm(list =ls())
#.rs.restartR()

# useTrainData switch to decide if using training images or classification data (FALSE) 
# usually training data will need more/all channels and classification data the necessary ones
useTrainData <- TRUE
# calculate syntheic bands and indices
calculateBands <- TRUE
# extract training data according to training geometries
extractTrain <- TRUE

# prefix of current run
prefixrunFN<-"traddel"

# suffix of training shape files e.g. index_2017_05_11_RGB_DEFS18_08_TrainingArea.shp
suffixTrainGeom <-"TrainingArea"

# suffix of training image files e.g. index_2017_05_11_RGB_DEFS18_08_OrthoMosaic.tif
prefixTrainGeom <- "index_"

#channels options c("red", "green", "blue")
channels<-c("red")

# switch for using  HaralickTextureExtraction 
# for a review of a lot of feature extraction algorithms look at:
# http://homepages.dcc.ufmg.br/~william/papers/paper_2012_JEI.pdf
# glcm<->haralick c("mean"  advanced1, "variance" advanced2 , "homogeneity"simple4, "contrast" simple5, "dissimilarity"advanced2, "entropy" simple2,"second_moment"simple4, "correlation" simple3)
# using stats will cover mean and variance while dissimilarity is highly correllated to  Homogeneity
# => stat + simple ~ glcm 
# good overview at: http://www.fp.ucalgary.ca/mhallbey/more_informaton.htm
hara=TRUE
# options are "all" "simple" "advanced"  "higher"
# "higher" takes a LOT of time
haraType=c("simple","higher")
# statistic: (mean,variance, curtosis, skewness)
stat=TRUE
# Edge filtering
edge=TRUE
# options are c("gradient","sobel","touzi")
edgeType=c("gradient","sobel","touzi")
# morpho filtering
morpho=TRUE
# options are ("dilate","erode","opening","closing")
morphoType=c("dilate","erode","opening","closing")
# indices: options are ("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI")
#c("VARI","NDTI","TGI","GLI","NGRDI","GLAI")
indices <- c("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI") 

# kernelsize
kernel<- 3

#---> define environment and settings
# define project folder
projRootDir <- "~/temp7/GRASS7"

# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","data/training/","data/training/idx/","data/training/idx/","output/","output/index/","run/","fun/") )

# set working directory
setwd(path_run)

if (useTrainData) {
  currentDataFolder<- paste0(path_data_training)
  currentIdxFolder<- paste0(path_data_training_idx)
}
if (!useTrainData) {
  currentDataFolder<- paste0(path_data)
  currentIdxFolder<- paste0(path_data_idx)
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
imageFiles <- list.files(pattern="[.]tif$", path=paste0(currentDataFolder), full.names=TRUE)

# stack the ortho images
rgb<- lapply(imageFiles, FUN=raster::stack)


### calculate indices and base stat export it to tif
# create list vars
rgb_all<- flist<-rasFN<-list()

# for all images do
for (i in 1:length(rgb)){
  cat(":::: processing indices of...",basename(imageFiles[i]),"\n")
  # calculates indices
  rgb_rgbi<-raster::stack(rgb[[i]],uavRst::rgbIndices(rgb[[i]][[1]],rgb[[i]][[2]],rgb[[i]][[3]],indices))
  bnames <-makebNames(rgbi = indices)
  # assign bandnumber according to name
  cat("\n")
  for (filterBand in channels){
    if (filterBand=="red") bandNr <- 1
    if (filterBand=="green") bandNr <- 2
    if (filterBand=="blue") bandNr <- 3
    # export single channel for synthetic band calculation
    # if (filterBand!="") {
       raster::writeRaster(rgb_rgbi[[bandNr]],paste0(filterBand,"_",basename(imageFiles[i])),overwrite=TRUE)
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
    flist<-append(flist, Sys.glob(paste0("*",basename(imageFiles[i]),"*")))
   # end of single channnel calculation
  
  # create an alltogether stack
  rgb_all[[i]]<-raster::stack(rgb_rgbi,raster::stack(unlist(flist)))
  if (raster::nlayers(rgb_all[[i]])!=length(bnames)) stop("\n Number of names and layers differ...\n most common case is a broken cleanup of the runtime directory!")
  #names(rgb_all[[i]])<-bnames
  
  rasFN[[i]]<-paste0(substr(basename(imageFiles[i]),1,nchar(basename(imageFiles[i]))-4))
  cat("      saving all bands as: ",prefixTrainGeom, rasFN[[i]],"\n")
  # stop if too much bands for geotiff format
  #if (raster::nlayers(rgb_all[[i]]) > 256) stop(paste0("\n", raster::nlayers(rgb_all) ,"calculated...  Geotiffs may have 256... reduce your synthetic channels"))
  # write file to envi
  
  raster::writeRaster(rgb_all[[i]],
                      paste0(currentIdxFolder,"/", prefixTrainGeom,rasFN[[i]]),
                      format="ENVI", 
                      progress ="text",
                      overwrite=TRUE)
  
  
  
  # cleanup runtime files lists...
  file.remove(unlist(flist))
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
  load(paste0(currentIdxFolder,"bandNames_",prefixrunFN,".RData"))
  #imageTrainStack <- lapply(imageTrainFiles, FUN=raster::stack)
  imageTrainStack<-lapply(imageTrainFiles, FUN=raster::stack)
  geomTrainStack  <- lapply(geomTrainFiles, FUN=raster::shapefile)
  
  # extract clean and format training data
  
  trainDF <- uavRst::extractTrainData(rasterStack  = imageTrainStack,
                                      trainPlots = geomTrainStack,
                                      bnames,
                                      rasFN
  )
  # create a new dataframe with prefixrunFN
  assign(paste0(prefixrunFN,"_trainDF"), trainDF,envir = )
  # save it 
  saveRDS(eval(parse(text=paste0(prefixrunFN,"_trainDF"))), paste0(currentIdxFolder,prefixrunFN,"_trainDF",".rds"))
  #read it into another name 
  DF<-readRDS(paste0(currentIdxFolder,prefixrunFN,"_trainDF",".rds"))  
  cat(":::: extraction...finsihed \n")
}