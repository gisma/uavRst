# 02_useCaseRGB_train.R

# training of classification models based on predictors and response variables 
# as provided by the dataframe from step2

#### packages
rm(list =ls())
require(uavRst)

# below you will find classes names and classes IDs 
# NOTE ADAPT IT TO YOUR NEEDS
# classes numbers
idNumber=c(1,2,3,4,5)
# classes names
idNames= c("green","nogreen","nogreen","nogreen","nogreen")
# prefix of current run
prefixrunFN<-"traddel"

#---> define environment and settings
# define project folder
projRootDir <- "~/temp7/GRASS7"

# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","data/training/","data/training/idx/","data/training/idx/","output/","output/index/","run/","fun/") )

currentIdxFolder<- path_data_training_idx

# set working directory
setwd(path_run)

#---> start processing
# prefix for dataframe providing the training data
trainDF<-readRDS(paste0(currentIdxFolder,prefixrunFN,"_trainDF",".rds"))  
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
# call training sequence
result<-  uavRst::trainModel(trainingDF = trainDF,
                             predictors   = pred,
                             response     = "ID",
                             spaceVar     = "FN",
                             names        =  na,
                             noLoc        = length(unique(trainDF$FN)),
                             cl_method    = "rf",
                             metric_ffs   = "Kappa",
                             metric_caret = "ROC",
                             pVal         = 0.01,
                             nrclu = 12)


model_final<-result[[2]]

save(model_final, file = paste0(path_output,prefixrunFN,"_model_final",".RData"))

perf <- model_final$pred[model_final$pred$mtry==model_final$bestTune$mtry,]
#cstat<-classificationStats(perf$pred,perf$obs,plot = T) 
#summary(lm(as.numeric(as.character(perf$pred))~as.numeric(as.character(perf$obs))))
#plot(as.numeric(as.character(perf$pred))~as.numeric(as.character(perf$obs)))
cat(":: training...finsihed \n")


