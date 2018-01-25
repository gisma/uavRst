# The usecaseRGBclassify 1-5 scripts are providing a common workflow for a random forest based classification of visible imagery.
# They can be used both - independly or in a chain 
# The worflow is divided in 5 steps:
# (01) calculation of spectral indices , basic spatial statistics and textures to generate  a set of predictor variables 
# (02) extracting of training values over all channels according to training data
# (03) perform training using random forest and the forward feature selection method
# (04) prediction
# (05) basic analysis and results extraction

#devtools::install_github("gisma/uavRst", ref = "master")
# require(uavRst)
devtools::install_github("gisma/link2GI", ref = "master")
# require(link2GI)
# require(CAST)
# require(raster)
# require(foreach)
# require(doParallel)

rm(list =ls())
#.rs.restartR()

# training data or classification data (FALSE) 
# usually training data will need more/all channels and classification data the necessary ones
train <- TRUE
extractTrain <- TRUE

# prefix for saved dataframe
prefixrunFN<-"traddel"
suffixTrainGeom <-"TrainingArea"
prefixTrainGeom <- "index_"
#channels options "red" "green" "blue"
channels<-c("green")

# switch for using  HaralickTextureExtraction 
# for a review of a lot of feature extraction algorithms look at:
# http://homepages.dcc.ufmg.br/~william/papers/paper_2012_JEI.pdf
# glcm<->haralick c("mean"advanced1, "variance" advanced2 , "homogeneity"simple4, "contrast" simple5, "dissimilarity"advanced2, "entropy" simple2,"second_moment"simple4, "correlation" simple3)
# NOTE IT TAKES A LOT OF TIME
hara=FALSE
# options are "all" "simple" "advanced"  "higher"
haratype=""
# statistic: (mean,variance, curtosis, skewness)
stat=TRUE
# Edge filtering
edge=TRUE
# options are "gradient" "sobel" "touzi"  
edgeType="touzi"
# morpho filtering
morpho=TRUE
# options are (dilate/erode/opening/closing)
morphoType="dilate"
# indices: options are ("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI")
#c("VARI","NDTI","TGI","GLI","NGRDI","GLAI")
indices <- c("GLAI") 

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

if (train) {
  currentDataFolder<- paste0(path_data_training)
  currentIdxFolder<- paste0(path_data_training_idx)
}
if (!train) {
  currentDataFolder<- paste0(path_data)
  currentIdxFolder<- paste0(path_data_idx)
}

# link GDAL and OTB
gdal <- link2GI::linkgdalUtils()
link2GI::linkOTB()

if ((stat == TRUE || hara == TRUE || edge == TRUE || morpho == TRUE) & path_OTB == "") stop("OTB missing - please check")

### ----- start preprocessing ---------------------------------------------------
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
  cat(":::: processing indices of...",basename(imageFiles[i]))
  # calculates indices
  rgb_rgbi<-raster::stack(rgb[[i]],uavRst::rgbIndices(rgb[[i]][[1]],rgb[[i]][[2]],rgb[[i]][[3]],indices))
  bnames <-makebNames(rgbi = indices)
  # assign bandnumber according to name
  for (filterBand in channels){
    if (filterBand=="red") bandNr <- 1
    if (filterBand=="green") bandNr <- 2
    if (filterBand=="blue") bandNr <- 3
    # export single channel for synthetic band calculation
    if (filterBand!="") {
      raster::writeRaster(rgb_rgbi[[bandNr]],paste0(filterBand,"_",basename(imageFiles[i])),overwrite=TRUE)
      fbFN<-paste0(filterBand,"_",basename(imageFiles[i]))
    } 
    # if calc statistcis 
    if (stat){
      cat("\n:::: processing stats...\n")
      otbLocalStat(input = fbFN,
                   out = paste0(filterBand,"stat_",basename(imageFiles[i])),
                   ram = "4096",
                   radius =  kernel)
      bnames <-append(bnames,paste0(makebNames(stat = TRUE),"_",filterBand))
    }
    # if calc edge
    if (edge){
      cat(":::: processing edge... ",edgeType,"\n")
      uavRst::otbEdge(input = fbFN,
                      out = paste0(filterBand,edgeType,basename(imageFiles[i])),
                      filter = edgeType)
      bnames <-append(bnames,makebNames(edge = paste0(edgeType,"_",filterBand)))
    }    
    # if calc morpho
    if (morpho){
      cat(":::: processing morpho... ",morphoType,"\n")
      uavRst::otbGrayMorpho(input = fbFN,
                            out = paste0(filterBand,morphoType,basename(imageFiles[i])),
                            filter = morphoType)
      bnames <-append(bnames,makebNames(edge = paste0(morphoType,"_",filterBand)))
    }    
    # if calc haralick
    if (hara){
      cat(":::: processing haralick... ",haratype,"\n")
      uavRst::otbTexturesHaralick(x = fbFN,
                                  output_name=paste0(filterBand,"hara_",basename(imageFiles[i])),
                                  texture = haratype)
      bnames <-append(bnames,paste0(makebNames(hara = haratype),"_",filterBand))
    }
    # delete single channel for synthetic channel calculation
    file.remove(paste0(filterBand,"_",basename(imageFiles[i])))
    # get the rest in a list
    flist<-append(flist, Sys.glob(paste0("*",basename(imageFiles[i]),"*")))
  } # end of single channnel calculation
  
  # create an alltogether stack
  rgb_all[[i]]<-raster::stack(rgb_rgbi,raster::stack(unlist(flist)))
  names(rgb_all[[i]])<-bnames
  
    rasFN[[i]]<-paste0(substr(basename(imageFiles[i]),1,nchar(basename(imageFiles[i]))-4))
    cat("      saving all bands as: ", rasFN[[i]],"\n")
    # stop if too much bands for geotiff format
    #if (raster::nlayers(rgb_all[[i]]) > 256) stop(paste0("\n", raster::nlayers(rgb_all) ,"calculated...  Geotiffs may have 256... reduce your synthetic channels"))
    # write file to envi
    
    raster::writeRaster(rgb_all[[i]],
                        paste0(currentIdxFolder,"/", prefixTrainGeom,rasFN[[i]]),
                        format="ENVI",
                        overwrite=TRUE)


  
  # cleanup runtime files lists...
  file.remove(unlist(flist))
  flist<-list()
}

# save bandname list we need it only once
save(bnames,file = paste0(currentIdxFolder,"bandNames_",prefixrunFN,".RData"))

cat(":::: finished preprocessing RGB data...\n")

if (extractTrain){
  cat(":::: start extraction... \n")
  # ----- start extraction ---------------------------------------------------
  # get image and geometry data for training purposes
  imageTrainFiles <- list.files(pattern="[.]envi$", path=currentIdxFolder, full.names=TRUE)
  tmp  <- basename(list.files(pattern="[.]envi$", path=currentIdxFolder, full.names=TRUE))
  geomTrainFiles<-paste0(currentDataFolder,substr(tmp,nchar(prefixTrainGeom)+1,nchar(tmp)-(nchar(suffixTrainGeom)+4)),suffixTrainGeom,".shp")
  load(paste0(currentIdxFolder,"bandNames_",prefixrunFN,".RData"))
  #imageTrainStack <- lapply(imageTrainFiles, FUN=raster::stack)
  imageTrainStack<-lapply(imageTrainFiles, FUN=raster::stack)
  geomTrainStack  <- lapply(geomTrainFiles, FUN=raster::shapefile)
  
  # extract clean and format training data
  
  trainingDF <- uavRst::extractTrainData(rasterStack  = imageTrainStack,
                                         trainPlots = geomTrainStack,
                                         bnames,
                                         rasFN
  )
  assign(paste0(prefixrunFN,"_trainDF"), trainingDF)
  save(eval(parse(text=paste0(prefixrunFN,"_trainDF"))), file = paste0(currentIdxFolder,prefixrunFN,"_trainDF",".RData"))
  
  cat(":::: extraction...finsihed \n")
}