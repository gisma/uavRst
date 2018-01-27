# 01_useCaseRGB_calcex.R

# The usecaseRGB 01-04 scripts are providing a common workflow for a random forest based classification of visible imagery.
# The worflow is divided in 4 steps:
# (01) calculation of spectral indices, basic spatial statistics and textures and
#      extracting of training values over all channels according to training data
#      (01_useCaseRGB_calcex.R)
# (02) model training using random forest and the forward feature selection method
#      (02_useCaseRGB_train.R)
# (03) calculation spectral indices, basic spatial statistics and textures for 
#      all rgb data according to the model requests (01_useCaseRGB_calcex.R)
# (04) prediction (02_useCaseRGB_predict.R)
# (05) basic analysis and results extraction (04_useCaseRGB_analyze.R)

#devtools::install_github("gisma/uavRst", ref = "master")
require(uavRst)
devtools::install_github("gisma/link2GI", ref = "master")

rm(list =ls())
#.rs.restartR()


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