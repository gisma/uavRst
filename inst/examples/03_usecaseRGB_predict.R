# predicting green leaves from UAV ortho imagery
# ---- define global parameters -----------------------------------------------
if (!chain) rm(list =ls())
#### packages
require(link2GI)
require(raster)
require(foreach)
require(doParallel)

# define project folder
projRootDir <- "~/temp7/GRASS7"
currentImgDir <- "../idx"
indices <- c("VARI","NDTI","TGI","GLI","NGRDI","GLAI") 
#trainModel<-"model_final.RData"
#load(trainModel)
# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","output/","run/","fun","idx") )

# set working directory
setwd(path_run)


#source(paste(path_fu,"predictRGB.R",sep=.Platform$file.sep))
#source(paste(path_fu,"getCounts.R",sep=.Platform$file.sep))

# set vars
imageFiles <- list.files(pattern="[.]tif$", path=paste0(path_id,"/"), full.names=TRUE)
bnameList <-  list.files(pattern="[.]RData$", path=paste0(path_id,"/"), full.names=TRUE)


predictRGB(imageFiles=imageFiles,
            model = model_final,
            in_prefix = "index_",
            out_prefix = "classified_",
            bandNames = bnameList) 

cat(":: ...finsihed \n")


