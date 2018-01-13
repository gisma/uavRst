# pre-processing of RGB UAV ortho imagery
# calculates RGB indices. stitstics and haralick 
# ---- define global parameters -----------------------------------------------
#### packages
# require(link2GI)
# require(CAST)
# require(raster)
# require(foreach)
require(doParallel)

# switch for using  HaralickTextureExtraction performs very well for all kind of image data
# for a review of a lot of feature extraction algorithms look at:
# http://homepages.dcc.ufmg.br/~william/papers/paper_2012_JEI.pdf
# options are "all" "simple" "advanced"  "higher"
# NOTE IT TAKES A LOT OF TIME
hara=TRUE
haratype="advanced"

# switch if standard statistic is calculated (mean,variance, curtosis, skewness)
stat=TRUE

# selection of channels 
# options are "red" "green" "blue"
channels<-c("red","green")

# selection of indices 
# options are ("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI")
#c("VARI","NDTI","TGI","GLI","NGRDI","GLAI")
indices <- c("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI") 

kernel<- 3

# define project folder
projRootDir <- "~/temp7/GRASS7"
#predictDir <-"predict"
# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","output/","run/","fun","idx") )

# set working directory
setwd(path_run)

# link GDAL and SAGA
gdal <- link2GI::linkgdalUtils()
otb<- link2GI::linkOTB()

#source(paste(path_fu,"basicClassify.R",sep=.Platform$file.sep))

imageFiles <- list.files(pattern="[.]tif$", path=paste0(path_data,"all"), full.names=TRUE)



### ----- start preprocessing ---------------------------------------------------
cat("\n::: preprocess input data...\n")

### stack the ortho images
rgb<- lapply(imageFiles, FUN=raster::stack)


### calculate indices and base stat export it to tif
rgb_all<- flist<-list()
# cl <- makeCluster(detectCores()-1)
# registerDoParallel(cl)  
# out <- foreach(i = 1:length(rgb),indices = indices) %dopar% {
#   library(raster)  
#   library(uavRst)
  for (i in 1:length(rgb)){
  rgb_rgbi<-raster::stack(rgb[[i]],uavRst::rgbIndices(rgb[[i]][[1]],rgb[[i]][[2]],rgb[[i]][[3]],indices))
  
  for (filterBand in channels){
    if (filterBand=="red") bandNr <- 1
    if (filterBand=="green") bandNr <- 2
    if (filterBand=="blue") bandNr <- 3
    # export single channel for synthetic band calculation
    raster::writeRaster(rgb_rgbi[[bandNr]],paste0(filterBand,"_",basename(imageFiles[i])),overwrite=TRUE)
    
    if (stat){
      uavRst:::otbLocalStat(fn = paste0(filterBand,"_",basename(imageFiles[i])),param=c(paste0(filterBand,"stat_",basename(imageFiles[i])),"4096",kernel))
    }
    
    if (hara){
      uavRst::otbTexturesHaralick(x = paste0(filterBand,"_",basename(imageFiles[i])),output_name=paste0(filterBand,"hara_",basename(imageFiles[i])),texture = haratype)
    }
    # delete single channel for synthetic channel calculation
    # file.remove(paste0(filterBand,"_",basename(imageFiles[i])))
    # get the rest in a list
    flist<-append(flist, Sys.glob(paste0("*","_",basename(imageFiles[i]),"*")))
    
  }
  # stack the results
  rgb_all<-raster::stack(rgb_rgbi,raster::stack(flist))
  if (raster::nlayers(rgb_all) > 256) stop(paste0("\n", raster::nlayers(rgb_all) ,"calculated...  Geotiffs may have 256... reduce your synthetic channels"))
  
  # export as geotiff
  fn<-paste0(path_id,"/index_",basename(imageFiles[i]))
  raster::writeRaster(rgb_all,fn,overwrite=TRUE)
  # cleanup
  file.remove(unlist(flist))
  flist<-list()
}

#stopCluster(cl)
cat("\n::: finished preprocessing RGB data...\n")
