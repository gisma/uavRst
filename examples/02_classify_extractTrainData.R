# training and classifying for green leav classification from UAV ortho imagery
# ---- define global parameters -----------------------------------------------
#### packages
# require(link2GI)
# require(CAST)
# require(raster)
# require(foreach)
# require(doParallel)
runname<-"test1"
# define project folder
projRootDir <- "~/temp7/GRASS7"
currentImgtrainDir <- "training"
# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","output/","run/","fun","idx") )

# set working directory
setwd(path_run)

# source(paste(path_fu,"trainModel.R",sep=.Platform$file.sep))
# source(paste(path_fu,"extractTrainData.R",sep=.Platform$file.sep))
# source(paste(path_fu,"classificationStats.R",sep=.Platform$file.sep))
# source(paste(path_fu,"classificationStats.R",sep=.Platform$file.sep))
# source(paste(path_fu,"latticeCombineGrid.R",sep=.Platform$file.sep))

# set vars
imageTrainFiles <- list.files(pattern="[.]tif$", path=paste0(path_data,currentImgtrainDir,"/"), full.names=TRUE)

indices <- c("VARI","NDTI","TGI","GLI","NGRDI","GLAI") 

kernel<- 3

# ----- start preprocessing ---------------------------------------------------


trainingFiles <- list.files(pattern="[.]shp$", path=paste0(path_data,"training"), full.names=TRUE)

proj <- "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
trainStack<-list()
for (i in 1:length(imageTrainFiles)) trainStack[[i]]<- stack(imageTrainFiles[i])

training <- lapply(trainingFiles, FUN=shapefile)

trainingDF <- extractTrainData(rasterStack  = trainStack,
                               trainPlots = training,
                                         ids=c(1,2),
                                         idLabel= c("green","nogreen")) 
saveRDS(trainingDF,path=paste0(path_output,runname,"_trainingDF.RData"))
  
cat(":: extraction...finsihed \n")


