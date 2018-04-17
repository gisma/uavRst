# use cases for UAV data based crown segmentation
# ---- define global parameters -----------------------------------------------

# load package for linking  GI tools
require(link2GI)
require(uavRst)
require(corrplot)
require("PerformanceAnalytics")
# define project root folder
projRootDir <- "~/data/chile/working"

### web data
# url <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/477369_800_5631924_000_477469_800_5632024_000.las"
# res <- curl::curl_download(url, "~/proj/uav/thesis/finn/output/lasdata.las")
# uav_data<-paste0(path_output,"lasdata.las")

# proj subfolders
projFolders = c("data/","data/ref/","output/","run/","las/")

# export folders as global
global = TRUE
# with folder name plus following prefix
path_prefix = "path_"
# proj4 string of ALL data
proj4 = "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs"

# full tile 
#cutExtent <- extent
#cutExtent <- c(477392.0, 477462.0, 5631935.0, 5632005.0)
#ext<- raster::extent(as.numeric(cutExtent))

maxCrownArea = 50
minTreeAlt = 0.3
prefixRun         = "rgbImg"

# create project structure and export global pathes
paths<-link2GI::initProj(projRootDir = projRootDir,
                         projFolders = projFolders,
                         global = TRUE,
                         path_prefix = path_prefix)
#'
# referenz shape filename
#plot2<-raster::shapefile(paste0(path_data,"ref/plot_UTM.shp"))

currentDataFolder = path_run
currentIdxFolder  = path_run
# image data
rgbImgFn<-list()
rgbImgFn<-c(paste0(path_data,"eme04.tif"),paste0(path_data,"eme07.tif"),paste0(path_data,"eme10.tif"))
# lidar data  can be a foldr or a file
 las_data <- paste0(path_data,"eme_dense.las")
# las_data <- "~/proj/uav/thesis/finn/output/477369_800_5631924_000_477469_800_5632024_000.las"
# las_data <- "~/proj/uav/thesis/finn/data/sequoia/477369_800_5631924_000_477469_800_5632024_000.las"
# uav 2D point cloud
#las_data <- "~/proj/uav/thesis/finn/data/sequoia/uniwald_sequoia2.las"

# set some params
actual_grid_size<-0.1

# link all CLI stuff
giLinks<-get_gi()

# clean dirs
#unlink(paste0(path_run,"*"), force = TRUE)

# set raster options
raster::rasterOptions(tmpdir=path_run)

# set working directory
setwd(path_run)


# ----- calculate DSM DTM & CHM FROM UAV POINT CLOUDS-----------------------------------------------
#las_data<-"~/proj/uav/thesis/finn/output/477375_00_5631900_00_477475_00_5632000_00.las"


# read rgb ortho image file
for (j in 1:length(rgbImgFn)){
rgbImg <- raster::stack(rgbImgFn[j])
cutExtent <- raster::extent(rgbImg)


# create 2D pointcloud DSM
dsm <- pc2D_dsm(laspcFile = las_data,
                cutExtent = cutExtent,
                gisdbasePath = projRootDir,
                sampleMethod = "max",
                targetGridSize = 0.1,
                giLinks = giLinks)

# create 2D point cloud DTM
dtm <- pc2D_dtm(laspcFile = las_data,
                cutExtent = cutExtent,
                gisdbasePath = projRootDir,
                tension = 20 ,
                sampleGridSize = 1,
                method="min",
                splineThresGridSize= .1,
                targetGridSize = actual_grid_size,
                giLinks = giLinks)

# # create 3D DTM
# dtm2 <- pc3D_dtm(lasDir = las_data,
#                       gisdbasePath = projRootDir,
#                       thinGrid = 1.,
#                       splineNumber = 5 ,
#                       gridSize = actual_grid_size,
#                       giLinks = giLinks)

# take the resulting raster files
 dsmR <- dsm
 dtmR <- dtm


# crop dsm/dtm data to the image file *extent* NOT RESOLUTION
 dtm<- raster::crop(dtmR,rgbImg)
 dsm<- raster::crop(dsmR,rgbImg)
# 
# resample dtm and rgb to dsm 
 dsm <- raster::resample(dsm, dtm , method = 'bilinear')
 rgbR <- raster::resample(rgbImg, dsm , method = 'bilinear')

# calculate CHM
 chmR <- dsm - dtm

# reset negative values to 0
 chmR[chmR<0.25]<-0

# save chm
raster::writeRaster(chmR,paste0(path_run,"chm_",j,".tif"),overwrite=TRUE)

# save the resampled image file NOTE use UNIQUE suffix!
raster::writeRaster(rgbR,paste0(path_run,"syn_",j,".tif"),overwrite=TRUE)

}
# calculate synthetic channels for segmentation
res <- calc_ext(calculateBands    = F,
                extractTrain      = T,
                suffixTrainGeom   = "",
                patternIdx        = "index",
                patternImgFiles   = "syn" ,
                patterndemFiles   = "chm",
                prefixRun         = "rgbImg",
                prefixTrainImg    = "",
                rgbi              = T,
                indices           =  c("NDTI","RI","SCI","BI","SI","HI","TGI","GLI","NGRDI","GRVI","GLAI","HUE","CI","SAT","SHP"),
                rgbTrans          = T,
                hara              = T,
                haraType          = c("advanced"),
                stat              = T,
                edge              = T,
                morpho            = T,
                pardem            = T,
                kernel            = 9,
                currentDataFolder = path_run,
                currentIdxFolder  = path_run,
                giLinks = giLinks)

imageTrainStack <- list()
imageTrainFiles <- list.files(pattern="[.]envi$", path=currentIdxFolder, full.names=TRUE)
 

idNumber=c(1,2,3,4,5,6,7,8,9)
# rename them
idNames= c("darkgreen","lightgreen","darksoil","lightsoil","shadow","cacti","lightgray","darkgray","molliswhite")

# for (j in 1:length(imageTrainFiles)){
#   rs<-raster::stack(imageTrainFiles[[j]])
#   split2SAGA(fn=rs,
#              bandName=bandNames,
#              startBand= 1,
#              endBand = raster::nlayers(rs),
#              refFn=paste0(path_run,"syn_",j,".tif"),
#              returnRaster=FALSE)}

# manipulate the data frame to you rneeds by dropping predictor variables
#keepsGreen <-c("ID","red","green","blue","VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI","FN")
#trainDF<-trainDF[ , (names(trainDF) %in% keepsGreen)]

# load raw training dataframe
if (!(exists)("trainDF"))
  trainDF<-readRDS(paste0(currentIdxFolder,prefixRun ,"_trainDF",".rds"))
if (!(exists)("bandNames"))
  load(paste0(currentIdxFolder,prefixRun ,"bandNames",".RData"))
# add leading Title "ID" and tailing title "FN"
names(trainDF)<-append("ID",append(bandNames,"FN"))

tr<-trainDF
drops <- c("ID","FN")
tr<-tr[ , !(names(tr) %in% drops)]
res<-tr[complete.cases(tr), ]
res<-res[complete.cases(res), ]

p.mat <- cor.mtest(res,na.action="na.omit")

p2<-p.mat[,- caret::nearZeroVar(p.mat)]
res2<-res[,- caret::nearZeroVar(p.mat)]

res3<-res2[p2>0.5,]
res3[complete.cases(res3), ]
n<-names(res3) 


# manipulate the data frame to you rneeds by dropping predictor variables
n<-append("ID",append(n,"FN"))
keeps <- n

trainDF<-trainDF[ , (names(trainDF) %in% keeps)]
trainDF<-trainDF[complete.cases(trainDF), ]
#keepsGreen <-names(res) #c("ID","red","green","blue","VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI","C_TOTA","SLOPE","Roughness","TPI","TRI","dem","FN")
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
pVal<- 0.7
# call Training
result<-  uavRst::ffs_train(trainingDF = trainDF,
                            predictors   = predictNames,
                            response     = "ID",
                            spaceVar     = "FN",
                            names        =  na,
                            withinSE = FALSE,
                            #noLoc        =  5,
                            pVal         = pVal,
                            noClu = 4)
