# pre-processing of RGB UAV ortho imagery (2/2) -  extract pixel values as training data
# step to is only needed for extracting training data based 
# on digitized and classified training geometry data 
# if (!chain){rm(list =ls())}
#devtools::install_github("gisma/uavRst", ref = "master")
require(uavRst)
#devtools::install_github("gisma/link2GI", ref = "master")
require(link2GI)
require(CAST)
require(raster)
require(foreach)
require(doParallel)   

#---> define environment and settings
# define project folder
projRootDir <- "~/temp9/Projekt1"
# define training data folder
trainDir <- "training"
# prefix for saved dataframe
prefixrunFN<-"traddel"
# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","output/","run/","fun","idx") )

# set working directory
setwd(path_run)

#---> start processing

# ----- start extraction ---------------------------------------------------
# get image and geometry data for training purposes
imageTrainFiles <- list.files(pattern="[.]tif$", path=paste0(path_data,trainDir), full.names=TRUE)
geomTrainFiles  <- list.files(pattern="[.]shp$", path=paste0(path_data,trainDir), full.names=TRUE)
rdataTrainFiles  <- list.files(pattern="bnames", path=paste0(path_data,trainDir), full.names=TRUE)
imageTrainStack <- lapply(imageTrainFiles, FUN=raster::stack)
geomTrainStack  <- lapply(geomTrainFiles, FUN=raster::shapefile)

# extract clean and format training data

trainingDF <- uavRst::extractTrainData(rasterStack  = imageTrainStack,
                                       trainPlots = geomTrainStack,
                                       trainDataFn = rdataTrainFiles
                                       )

save(trainingDF, file = paste0(path_output,prefixrunFN,"_files_traindat_",".RData"))

cat("\n::: extraction...finsihed \n")


