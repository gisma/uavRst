# 02_useCaseRGB_train.R

# training of classification models based on predictors and response variables 
# as provided by the dataframe from step2

#### packages
#rm(list =ls())
require(uavRst)


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
# call training sequence

# call training function
result<-  uavRst::ffsTrainModel(trainingDF = trainDF,
                             predictors   = pred,
                             response     = "ID",
                             spaceVar     = "FN",
                             names        =  na,
                             noLoc        =  5,
                             pVal         = 0.01,
                             noClu = 4)

system("kill -9 $(pidof R)")

save(result[[1]], file = paste0(path_output,prefixrunFN,"_model_ffs",".RData"))
save(result[[2]], file = paste0(path_output,prefixrunFN,"_model_final",".RData"))

perf <- model_final$pred[model_final$pred$mtry==model_final$bestTune$mtry,]
# scores for categorical 
classificationStats(perf$pred,perf$obs, plot = T) 

# linear model for numeric
# summary(lm(as.numeric(as.character(perf$pred))~as.numeric(as.character(perf$obs))))
# plot(as.numeric(as.character(perf$pred))~as.numeric(as.character(perf$obs)))

cat(":: training...finsihed \n")


