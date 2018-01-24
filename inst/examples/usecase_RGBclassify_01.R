# The usecaseRGBclassify 1-5 scripts are providing a common workflow for a random forest based classification of visible imagery.
# They can be used both - independly or in a chain 
# The worflow is divided in 5 steps:
# (01) calculation of spectral indices , basic spatial statistics and textures to generate  a set of predictor variables 
# (02) extracting of training values over all channels according to training data
# (03) perform training using random forest and the forward feature selection method
# (04) prediction
# (05) basic analysis and results extraction

devtools::install_github("gisma/uavRst", ref = "master")
require(uavRst)
devtools::install_github("gisma/link2GI", ref = "master")
require(link2GI)
require(CAST)
require(raster)
require(foreach)
require(doParallel)

# # pre-processing of RGB UAV ortho imagery (1/2) - calculates RGB indices, statistics and haralick 
# switch to concatenate this script to the next one
chain <- TRUE 
# training data or classification data (FALSE) 
# usually training data will need more/all channels and classification data the necessary ones
train <- TRUE

#### packages
if (!chain) rm(list =ls())

# switch for using  HaralickTextureExtraction 
# for a review of a lot of feature extraction algorithms look at:
# http://homepages.dcc.ufmg.br/~william/papers/paper_2012_JEI.pdf
# glcm<->haralick c("mean"advanced1, "variance" advanced2 , "homogeneity"simple4, "contrast" simple5, "dissimilarity"advanced2, "entropy" simple2,"second_moment"simple4, "correlation" simple3)
# NOTE IT TAKES A LOT OF TIME
hara=TRUE
# options are "all" "simple" "advanced"  "higher"
haratype="simple"
# statistic: (mean,variance, curtosis, skewness)
stat=FALSE
#channels options "red" "green" "blue"
channels<-c("green")
# indices: options are ("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI")
#c("VARI","NDTI","TGI","GLI","NGRDI","GLAI")
indices <- c("GLAI") 


# kernelsize
kernel<- 3

#---> define environment and settings
# define project folder
projRootDir <- "~/temp7/GRASS7"
# define training data folder
trainDir <- "training"
# prefix for saved dataframe
prefixrunFN<-"traddel"
# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","output/","run/","fun","idx") )

# set working directory
setwd(path_run)


# link GDAL and OTB
gdal <- link2GI::linkgdalUtils()
otb<- link2GI::linkOTB()
if (stat == TRUE || hara == TRUE & otb == "") stop("OTB missing - please check")

### ----- start preprocessing ---------------------------------------------------
cat("\n::: preprocess input data...\n")

# create list of image files to be processed 
# NOTE all subfolder below c("data/","output/","run/","fun","idx") have to created individually
imageFiles <- list.files(pattern="[.]tif$", path=paste0(path_data,"all"), full.names=TRUE)

# stack the ortho images
rgb<- lapply(imageFiles, FUN=raster::stack)


### calculate indices and base stat export it to tif

# create list vars
rgb_all<- flist<-list()

# for all images do
for (i in 1:length(rgb)){
  # calculates indices
  rgb_rgbi<-raster::stack(rgb[[i]],uavRst::rgbIndices(rgb[[i]][[1]],rgb[[i]][[2]],rgb[[i]][[3]],indices))
  # assign bandnumber according to name
  for (filterBand in channels){
    if (filterBand=="red") bandNr <- 1
    if (filterBand=="green") bandNr <- 2
    if (filterBand=="blue") bandNr <- 3
    # export single channel for synthetic band calculation
    raster::writeRaster(rgb_rgbi[[bandNr]],paste0(filterBand,"_",basename(imageFiles[i])),overwrite=TRUE)
    # if calc statistcis 
    if (stat){
      cat("\n::: processing stats...\n")
      otbLocalStat(fn = paste0(filterBand,"_",basename(imageFiles[i])),param=c(paste0(filterBand,"stat_",basename(imageFiles[i])),"4096", kernel))
    }
    # if calc haralick
    if (hara){
      cat("\n::: processing haralick... ",haratype,"\n")
      uavRst::otbTexturesHaralick(x = paste0(filterBand,"_",basename(imageFiles[i])),output_name=paste0(filterBand,"hara_",basename(imageFiles[i])),texture = haratype)
    }
    
    # delete single channel for synthetic channel calculation
    file.remove(paste0(filterBand,"_",basename(imageFiles[i])))
    # get the rest in a list
    flist<-append(flist, Sys.glob(paste0("*","_",basename(imageFiles[i]),"*")))
  } # end of single channnel calculation

  # stack the results
  cat("\n::: saving all bands of ",imageFiles[i]," as ", paste0("index_",basename(imageFiles[i]),"\n"))
  
  # create an alltogether stack
  rgb_all<-raster::stack(rgb_rgbi,raster::stack(unlist(flist)))
  
  # stop if to much bands for geotiff
  if (raster::nlayers(rgb_all) > 256) stop(paste0("\n", raster::nlayers(rgb_all) ,"calculated...  Geotiffs may have 256... reduce your synthetic channels"))
  
  # create list of bandnames
  bnames  <- makebNames(rgbi = indices, stat = stat, haratxt = haratype)
  
  # create exportfilename according to training or classifing needs
  if (train){
    fn<-paste0(path_data,trainDir,"/index_",basename(imageFiles[i]))
    save(bnames,file = paste0(path_data,trainDir,"/bnames_index_",basename(imageFiles[i]),".RData"))
  } else {
    fn<-paste0(path_id,"/index_",basename(imageFiles[i]))
    # save bandname list
    save(bnames,file = paste0(path_id,"/bnames_index_",basename(imageFiles[i]),".RData"))
  }
  
  # write file to geotiff
  names(rgb_all)<-bnames
  raster::writeRaster(rgb_all,fn,overwrite=TRUE)
  
  # cleanup tempfiles lists...
  file.remove(unlist(flist))
  flist<-list()
}


cat("\n::: finished preprocessing RGB data...\n")

if (chain) source('~/dev/R/uavRst/examples/usecase_RGBclassify_02.R', echo=TRUE)
