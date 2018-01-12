# pre-processing of RGB UAV ortho imagery
# calculates RGB indices. stitstics and haralick 
# ---- define global parameters -----------------------------------------------
#### packages
 require(doParallel)

hara=TRUE
haratype="all"
stat=TRUE
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

indices <- c("VARI","NDTI","TGI","GLI","NGRDI","GLAI") 

kernel<- 3

### ----- start preprocessing ---------------------------------------------------
cat("\n::: preprocess input data...\n")

### stack the ortho images
rgb<- lapply(imageFiles, FUN=raster::stack)


### calculate indices and base stat export it to tif
cat("::: calculate lStat... \n")
rgb_all<-list()
cl <- doParallel::makeCluster(detectCores())
registerDoParallel(cl)  
rgb_all <- foreach(i = 1:length(rgb)) %dopar% {
  rgb_all[[i]]<-raster::stack(rgb[[i]],uavRst::rgbIndices(rgb[[i]][[1]],rgb[[i]][[2]],rgb[[i]][[3]],indices))
  if (stat){
    raster::writeRaster(rgb_all[[i]][[2]],paste0("green_",basename(imageFiles[i])),overwrite=TRUE)
    uavRst:::otbLocalStat(fn = paste0("green_",basename(imageFiles[i])),param=c(paste0("greenstat_",basename(imageFiles[i])),"4096",kernel))
    rgb_all[[i]]<-raster::stack(rgb_all[[i]],raster::stack(paste0("greenstat_",basename(imageFiles[i]))))
  } else if (hara){
    harastack<-uavRst::otbTexturesHaralick(x = paste0("green_",basename(imageFiles[i])),output_name=paste0("greenstathara_",basename(imageFiles[i])),texture = haratype,return_raster = TRUE)
    rgb_all[[i]]<-raster::stack(rgb_all[[i]],harastack)
  }
  fn<-paste0(path_id,"/index_",basename(imageFiles[i]))
  raster::writeRaster(rgb_all[[i]],fn,overwrite=TRUE)
  file.remove(Sys.glob("*greenstathara_*"))
  file.remove(paste0("greenstat_",basename(imageFiles[i])))
  file.remove(paste0("green_",basename(imageFiles[i])))
}

stopCluster(cl)
cat("\n::: finished preprocessing training data...\n")
