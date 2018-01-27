# useCaseRGB.R
# Basic script to set up the environment for a typical classification project
# The usecaseRGB 01-04 scripts are providing a common workflow for a random forest based classification of visible imagery.
# The worflow is divided in 4 steps:
# (01) calcex() calculation of spectral indices, basic spatial statistics and textures and
#      extracting of training values over all channels according to training data
#      
# (02) trainRGBmodel() training using random forest and the forward feature selection method
#
# (03) calculation spectral indices, basic spatial statistics and textures for 
#      all rgb data according to the model requests (01_useCaseRGB_calcex.R)
# (04) prediction (02_useCaseRGB_predict.R)
# (05) basic analysis and results extraction (04_useCaseRGB_analyze.R)

#devtools::install_github("gisma/uavRst", ref = "master")
require(uavRst)
devtools::install_github("gisma/link2GI", ref = "master")

rm(list =ls())
#.rs.restartR()

startTrain=FALSE

#---> define environment and settings
# define project folder
projRootDir <- "~/temp7/GRASS7"

# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","data/training/","data/training/idx/","data/training/idx/","output/","output/index/","run/","fun/") )

# set working directory
setwd(path_run)

res <- calcex( useTrainData      = TRUE, 
               calculateBands    = TRUE, 
               extractTrain      = TRUE, 
               prefixrunFN       = "traddel",
               suffixTrainGeom   = "TrainingArea",
               prefixTrainGeom   = "index_", 
               indices           = c("VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI") , 
               channels          = c("red", "green", "blue"),  
               hara              = FALSE,
               haraType          = c("simple"),   
               stat              = TRUE, 
               edge              = TRUE, 
               edgeType          = c("gradient","sobel","touzi"), 
               morpho            = TRUE, 
               morphoType        = c("dilate","erode","opening","closing"), 
               kernel            = 3, 
               currentDataFolder = path_data_training,
               currentIdxFolder  = path_data_training_idx)

# ------------------  TRAIN

if (startTrain){
  #---> start processing
  # adapt dataframe for special needs
  # here GREEN LEAFS
  # NOTE ADAPT IT TO YOUR NEEDS
  # classes numbers
  idNumber=c(1,2,3,4,5)
  # classes names
  idNames= c("green","greenish","bud","nogreen","nogreen")
  # prefix of current run
  prefixrunFN<-"traddel"
  # load raw training dataframe
  trainDF<-readRDS(paste0(currentIdxFolder,prefixrunFN,"_trainDF",".rds"))  
  load(paste0(currentIdxFolder,"bandNames_",prefixrunFN,".RData"))
  names(trainDF)<-append("ID",append(bnames,"FN"))
  #keepsGreen <-c("ID","red","green","blue","VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI","FN")
  #trainDF<-trainDF[ , (names(trainDF) %in% keepsGreen)]
  # define classes
  for (i in 1:length(idNumber)){
    trainDF$ID[trainDF$ID==i]<-idNames[i]
  }
  trainDF$ID <- as.factor(trainDF$ID)
  # split names in predict and all var names 
  na<-names(trainDF)
  # split names in predict and all var names 
  na<-names(trainDF)
  pred<-na[3:length(na)-1]
  
  result<-  uavRst::trainModel(trainingDF = trainDF,
                               predictors   = pred,
                               response     = "ID",
                               spaceVar     = "FN",
                               names        =  na,
                               noLoc        =  5,
                               pVal         = 0.01,
                               noClu = 4)
  
  #system("kill -9 $(pidof R)")
  
  save(result[[1]], file = paste0(path_output,prefixrunFN,"_model_ffs",".RData"))
  save(result[[2]], file = paste0(path_output,prefixrunFN,"_model_final",".RData"))
  
  perf <- model_final$pred[model_final$pred$mtry==model_final$bestTune$mtry,]
  # scores for categorical 
  skills <- classificationStats(perf$pred,perf$obs, plot = T) 
  
  # linear model for numeric
  # summary(lm(as.numeric(as.character(perf$pred))~as.numeric(as.character(perf$obs))))
  # plot(as.numeric(as.character(perf$pred))~as.numeric(as.character(perf$obs)))
  
  cat(":: training...finsihed \n")
}