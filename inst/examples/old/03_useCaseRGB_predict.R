# predicting green leaves from UAV ortho imagery
# ---- define global parameters -----------------------------------------------

#### packages


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
#source(paste(path_fu,"predictRGB.R",sep=.Platform$file.sep))
#source(paste(path_fu,"getCounts.R",sep=.Platform$file.sep))

# set vars
imageFiles <- list.files(pattern="[.]tif$", path=path_data_training, full.names=TRUE)
bnameList <-  list.files(pattern="[.]RData$", path=path_data_training_idx, full.names=TRUE)
load(bnameList)
load(file = paste0(path_output,prefixrunFN,"_model_final",".RData"))
# TODO https://stackoverflow.com/questions/25388139/r-parallel-computing-and-zombie-processes

predictRGB(imageFiles=imageFiles,
            model = model_final,
            in_prefix = "index_",
            out_prefix = "classified_",
            bandNames = bnames) 

cat(":: ...finsihed \n")


