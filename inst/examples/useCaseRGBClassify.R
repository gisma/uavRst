# useCaseRGBClassify.R
# -------------------------------------------------------------------------------------------------
#  Basic script to set up the environment for a typical classification project and
#  a common workflow for a random forest based classification of visible imagery.
#  The worflow can be stripped in 5 steps:

# (01) calcex() calculation of spectral indices, basic spatial statistics and textures and
#               extracting of training values over all channels according to training data

# (02) ffsTrainModel() training using random forest and the forward feature selection method 
#                      startTrain=TRUE

# (03) calcex() with respect to the selected predictor variables you may calculate 
#               the requested channels for all rgb data that you want to predict.

# (04) prediction startPredict=TRUE

# (05) basic analysis and results extraction very preliminary have a look at useCaseRGB_analyze.R

# -------------------------------------------------------------------------------------------------
# get the last version if not done so far
#devtools::install_github("gisma/uavRst", ref = "master")
require(uavRst)
# same with link2GI
devtools::install_github("gisma/link2GI", ref = "master")

# clean everything
rm(list =ls())

# set processing switches
startCalcex  = TRUE
startTrain   = TRUE
startPredict = TRUE

# simple and maybe sufficient give your training run  a unique name 
# to be integrated in results DF and file 
prefixrunFN       = "traddel"

# define project folder
projRootDir <- "~/temp7/GRASS7"

# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/",
                                  "data/training/",
                                  "data/training/idx/",
                                  "output/",
                                  "run/",
                                  "fun/") )

# set current data and results path default is training 
currentDataFolder = path_data_training
currentIdxFolder  = path_data_training_idx

# set working directory dirty but helpful
setwd(path_run)
# clean run dir
unlink(paste0(path_run,"*"), force = TRUE)
if (startCalcex){
  # start calculation of synthetic bands and extraction of the training data
  # note otions are commented due to the fact that the maximum is default
  # to restrict calculations uncomment and select by editng the param list
  res <- calcex( useTrainData      = TRUE, 
                 calculateBands    = TRUE, 
                 extractTrain      = TRUE, 
                 prefixrunFN       = prefixrunFN,
                 suffixTrainGeom   = "TrainingArea",
                 prefixTrainGeom   = "index_", 
                 indices           =  c("VVI"),#,"VARI","NDTI","RI","SCI","BI","SI","HI","TGI","GLI","NGRDI","GRVI","GLAI","HUE","CI","SAT","SHP"), 
                 RGBTrans          = TRUE,
                 colorSpaces       = c("CIELab","XYZ","YUV"),
                 channels          = c("red"),# "green", "blue"),  
                 hara              = TRUE,
                 haraType          = c("simple"), #,"advanced","higher"),   
                 stat              = TRUE, 
                 edge              = TRUE, 
                # edgeType          = c("gradient","sobel","touzi"), 
                 morpho            = TRUE, 
                # morphoType        = c("dilate","erode","opening","closing"), 
                 kernel            = 3, 
                 currentDataFolder = currentDataFolder,
                 currentIdxFolder  = currentIdxFolder)
}
# ------------------  TRAIN

if (startTrain){
  # here example is given for GREEN LEAVES
  
  # NOTE ADAPT IT TO YOUR NEEDS
  
  # classes IDs as given by the training vector files ID column
  idNumber=c(1,2,3,4,5)
  # rename them 
  idNames= c("green","greenish","greenish","nogreen","nogreen")
  
  # load raw training dataframe
  if (!(exists)("trainDF"))
  trainDF<-readRDS(paste0(currentIdxFolder,prefixrunFN,"_trainDF",".rds"))  
  if (!(exists)("bnames"))
  load(paste0(currentIdxFolder,"bandNames_",prefixrunFN,".RData"))
  # add leading Title "ID" and tailing title "FN"
  names(trainDF)<-append("ID",append(bnames,"FN"))
  
  # manipulate the data frame to you rneeds by dropping predictor variables
  #keepsGreen <-c("ID","red","green","blue","VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI","FN")
  #trainDF<-trainDF[ , (names(trainDF) %in% keepsGreen)]
  
  # now rename the classes
  for (i in 1:length(idNumber)){
    trainDF$ID[trainDF$ID==i]<-idNames[i]
  }
  trainDF$ID <- as.factor(trainDF$ID)
  
  # get actual name list from the DF
  na<-names(trainDF)
  # cut leading and tailing ID/FN
  predictNames<-na[3:length(na)-1]
  pVal<- 0.1
  # call Training 
  result<-  uavRst::ffsTrainModel(trainingDF = trainDF,
                                  predictors   = predictNames,
                                  response     = "ID",
                                  spaceVar     = "FN",
                                  names        =  na,
                                  noLoc        =  5,
                                  pVal         = pVal,
                                  noClu = 4)
  
  # if parallel process was interuppted and not finished correctly the resulting R sessions will be killed
  system("kill -9 $(pidof R)")
  

  saveRDS(result, file = paste0(path_output,prefixrunFN,"_",pVal,"_model_final",".rds"))
  model_final=result[[2]]
  perf <- model_final$pred[model_final$pred$mtry==model_final$bestTune$mtry,]
  # scores for categorical 
  skills <- classificationStats(perf$pred,perf$obs, plot = T) 
  #plot(skills)
  # linear model for numeric
  # summary(lm(as.numeric(as.character(perf$pred))~as.numeric(as.character(perf$obs))))
  # plot(as.numeric(as.character(perf$pred))~as.numeric(as.character(perf$obs)))
  
  cat(":: training...finsihed \n")
}

if (startPredict){
  # get images and bandnames
  imageFiles <- list.files(pattern="[.]tif$", path=currentDataFolder, full.names=TRUE)
  bnameList <-  list.files(pattern="[.]RData$", path=currentIdxFolder, full.names=TRUE)
  load(bnameList)
  load(file = paste0(path_output,prefixrunFN,"_model_final",".RData"))
  
  # start prediction
  predictRGB(imageFiles=imageFiles,
             model = model_final,
             in_prefix = "index_",
             out_prefix = "classified_",
             bandNames = bnames) 
  
  cat(":: ...finsihed \n")
  
}
