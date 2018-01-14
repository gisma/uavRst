# training and classifying for green leav classification from UAV ortho imagery
# ---- define global parameters -----------------------------------------------
#### packages
# require(link2GI)
require(CAST)
# require(raster)
 require(foreach)
 require(doParallel)
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


load(paste0(path_output,runname,"_trainingDF.RData"))
na<-names(trainingDF)
pred<-na[3:length(na)-1]
result<-  uavRst::trainModel(trainingDF =trainingDF,
             predictors   = pred,
             response     = "ID",
             spaceVar     = "FN",
             names        =  na,
             noLoc        = length(unique(trainingDF$FN)),
             cl_method    = "rf",
             metric_ffs   = "Kappa",
             metric_caret = "ROC",
             pVal         = 0.5) 


# load("model_final.RData")
model_final<-result[[2]]

perf <- model_final$pred[model_final$pred$mtry==model_final$bestTune$mtry,]
cstat<-classificationStats(perf$pred,perf$obs,plot = T) 
#summary(lm(as.numeric(as.character(perf$pred))~as.numeric(as.character(perf$obs))))
#plot(as.numeric(as.character(perf$pred))~as.numeric(as.character(perf$obs)))
cat(":: training...finsihed \n")


