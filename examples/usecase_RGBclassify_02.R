# extracting training data based on duigitized and classied geometries based on UAV ortho imagery
rm(list =ls())
require(link2GI)
require(CAST)
require(raster)
require(foreach)
require(doParallel)   
   # folder containing shapefiles and images files for training purposes
currentShptrainDir <- "training"
# prefix for saved dataframe
runname<-"test1"
# classes numbers
idNumber=c(1,2,3,4,5)
# classes names
idNames= c("green","nogreen","green","nogreen","nogreen")

# project folder
projRootDir <- "~/temp7/GRASS7"

# create project structure and export pathes as global variables
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","output/","run/","fun","idx") )

# set working directory
setwd(path_run)

# ----- start extraction ---------------------------------------------------
# get image and geometry data for training purposes
imageTrainFiles <- list.files(pattern="[.]tif$", path=paste0(path_data,currentShptrainDir), full.names=TRUE)
geomTrainFiles  <- list.files(pattern="[.]shp$", path=paste0(path_data,currentShptrainDir), full.names=TRUE)
rdataTrainFiles  <- list.files(pattern="bnames", path=paste0(path_data,currentShptrainDir), full.names=TRUE)
 imageTrainStack <- lapply(imageTrainFiles, FUN=raster::stack)
 geomTrainStack  <- lapply(geomTrainFiles, FUN=raster::shapefile)

# extract clean and format training data

trainingDF <- uavRst::extractTrainData(rasterStack  = imageTrainStack,
                                       trainPlots = geomTrainStack,
                                       ids=idNumber,
                                       idLabel= idNames,
                                       trainDataFn = rdataTrainFiles
                                       )

cat("\n::: extraction...finsihed \n")


