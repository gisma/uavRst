# training and classifying for green leav classification from UAV ortho imagery
# ---- define global parameters -----------------------------------------------
#### packages

currentShptrainDir <- "training"
runname<-"test1"
proj <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
idNumber=c(1,2,3,4,5)
idNames= c("green","nogreen","green","nogreen","nogreen")

# define project folder
projRootDir <- "~/temp7/GRASS7"

# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","output/","run/","fun","idx") )

# set working directory
setwd(path_run)

# ----- start extraction ---------------------------------------------------
# get image data
imageTrainFiles <- list.files(pattern="[.]tif$", path=paste0(path_id,"/"), full.names=TRUE)
trainStack<-list()
for (i in 1:length(imageTrainFiles)) trainStack[[i]]<- raster::brick(imageTrainFiles[i])


# get training data
trainingFiles <- list.files(pattern="[.]shp$", path=paste0(path_data,currentShptrainDir), full.names=TRUE)
training <- lapply(trainingFiles, FUN=raster::shapefile)

# start training process
trainingDF <- uavRst::extractTrainData(rasterStack  = trainStack,
                               trainPlots = training,
                               ids=idNumber,
                               idLabel= idNames,
                               trainDataFn = paste0(path_output,runname,"_trainingDF.RData")
                               ) 

#saveRDS(trainingDF,path=paste0(path_output,runname,"_trainingDF.RData"))
  
cat(":: extraction...finsihed \n")


