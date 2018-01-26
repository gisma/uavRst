# training of classification models based on predictors and response variables 
# as provided by the dataframe from step2

#### packages
if (!chain) rm(list =ls())
require(link2GI)
require(CAST)
require(raster)
require(foreach)
require(doParallel)

# below you will find classes names and classes IDs 
# NOTE ADAPT IT TO YOUR NEEDS
# classes numbers
idNumber=c(1,2,3,4,5)
# classes names
idNames= c("green","nogreen","nogreen","nogreen","nogreen")

# prefix for dataframe providing the training data
if (!chain){
trainFN<-paste0("/home/creu/temp7/GRASS7/output/traddel_traindat_.RData")
load(trainFN)
}

#---> define environment and settings
# define project folder
projRootDir <- "~/temp7/GRASS7"
trainDir <- "training"
# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","output/","run/","fun","idx") )

# set working directory
setwd(path_run)

#---> start processing


for (i in 1:length(idNumber)){
  trainingDF$ID[trainingDF$ID==i]<-idNames[i]
}
trainingDF$ID <- as.factor(trainingDF$ID)
# split names in predict and all var names 
na<-names(trainingDF)

# split names in predict and all var names 
na<-names(trainingDF)

pred<-na[3:length(na)-1]
# call training sequence
result<-  uavRst::trainModel(trainingDF = trainingDF,
                             predictors   = pred,
                             response     = "ID",
                             spaceVar     = "FN",
                             names        =  na,
                             noLoc        = length(unique(trainingDF$FN)),
                             cl_method    = "rf",
                             metric_ffs   = "Kappa",
                             metric_caret = "ROC",

                             pVal         = 0.05,
                             nrclu = 12)


# load("model_final.RData")
model_final<-result[[2]]

save(model_final, file = paste0(path_output,prefixrunFN,"_model_final",".RData"))

perf <- model_final$pred[model_final$pred$mtry==model_final$bestTune$mtry,]
#cstat<-classificationStats(perf$pred,perf$obs,plot = T) 
#summary(lm(as.numeric(as.character(perf$pred))~as.numeric(as.character(perf$obs))))
#plot(as.numeric(as.character(perf$pred))~as.numeric(as.character(perf$obs)))
cat(":: training...finsihed \n")


