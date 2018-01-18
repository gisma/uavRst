# extracting training data based on duigitized and classied geometries based on UAV ortho imagery
if (!train){rm(list =ls())}
devtools::install_github("gisma/uavRst", ref = "master")
require(uavRst)
devtools::install_github("gisma/link2GI", ref = "master")
require(link2GI)require(CAST)
require(raster)
require(foreach)
require(doParallel)   

# prefix for saved dataframe
prefixrunFN<-"traddel"


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

trainDF <- uavRst::extractTrainData(rasterStack  = imageTrainStack,
                                       trainPlots = geomTrainStack,
                                       trainDataFn = rdataTrainFiles
                                       )

save(trainingDF, file = paste0(path_output,prefixrunFN,"_files_traindat_",".RData"))

cat("\n::: extraction...finsihed \n")


