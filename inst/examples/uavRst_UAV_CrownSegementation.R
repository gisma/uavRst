# use cases for UAV data based crown segmentation
# ---- define global parameters -----------------------------------------------

# load package for linking  GI tools
require(link2GI)
require(uavRst)

# define project root folder
projRootDir <- "~/proj/uav/thesis/finn"

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
proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "

# full tile 
#cutExtent <- c(477369.8, 477469.8, 5631924, 5632024)
cutExtent <- c(477392.0, 477462.0, 5631935.0, 5632005.0)
ext<- raster::extent(as.numeric(cutExtent))

maxCrownArea = 150
minTreeAlt = 2

# create project structure and export global pathes
paths<-link2GI::initProj(projRootDir = projRootDir,
                         projFolders = projFolders,
                         global = TRUE,
                         path_prefix = path_prefix)
#'
# referenz shape filename
plot2<-raster::shapefile(paste0(path_data,"ref/plot_UTM.shp"))

# image data
rgbImgFn<-paste0(path_data,"training/uniWald_sequoia-3-3.tif")
# lidar data  can be a foldr or a file
# las_data <- "~/proj/uav/thesis/finn/output/477119_673_5631675_040_477733_694_5632284_900.las"
# las_data <- "~/proj/uav/thesis/finn/output/477369_800_5631924_000_477469_800_5632024_000.las"
# las_data <- "~/proj/uav/thesis/finn/data/sequoia/477369_800_5631924_000_477469_800_5632024_000.las"
# uav 2D point cloud
las_data <- "~/proj/uav/thesis/finn/data/sequoia/uniwald_sequoia2.las"

# set some params
actual_grid_size<-0.1
splineNumber<-7
thingrid<- 4.

# link all CLI stuff
giLinks<-get_gi()

# clean dirs
unlink(paste0(path_run,"*"), force = TRUE)

# set raster options
raster::rasterOptions(tmpdir=path_run)

# set working directory
setwd(path_run)

# ----- calculate DSM DTM & CHM FROM UAV POINT CLOUDS-----------------------------------------------
#las_data<-"~/proj/uav/thesis/finn/output/477375_00_5631900_00_477475_00_5632000_00.las"

# create 2D pointcloud DSM
dsm <- pc2D_dsm(laspcFile = las_data,
                gisdbasePath = projRootDir,
                sampleMethod = "max",
                targetGridSize = actual_grid_size, 
                giLinks = giLinks)

# create 2D point cloud DTM
dtm <- pc2D_dtm(laspcFile = las_data,
                gisdbasePath = projRootDir,
                tension = 20 ,
                sampleGridSize = 25,
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

# read rgb ortho image file
rgbImg <- raster::stack(rgbImgFn)

# crop dsm/dtm data to the image file *extent* NOT RESOLUTION
dtm<- raster::crop(dtmR,rgbImg)
dsm<- raster::crop(dsmR,rgbImg)

# resample dtm and rgb to dsm 
dtm <- raster::resample(dtm, dsm , method = 'bilinear')
rgbR <- raster::resample(rgbImg, dsm , method = 'bilinear')

# calculate CHM
chmR <- dsm - dtm

# reset negative values to 0
chmR[chmR<0]<-0

# save chm
raster::writeRaster(chmR,paste0(path_run,"chm_3-3.tif"),overwrite=TRUE)

# save the resampled image file NOTE use UNIQUE suffix!
raster::writeRaster(rgbR,paste0(path_run,"syn_3-3.tif"),overwrite=TRUE)

# calculate synthetic channels for segmentation
res <- calc_ext(calculateBands    = T,
                extractTrain      = F,
                suffixTrainGeom   = "",
                patternIdx        = "index",
                patternImgFiles   = "syn" ,
                patterndemFiles   = "chm",
                prefixRun         = "rgbImg",
                prefixTrainImg    = "",
                rgbi              = T,
                indices           =  c("NDTI","RI","SCI","BI","SI","HI","TGI","GLI","NGRDI","GRVI","GLAI","HUE","CI","SAT","SHP"),
                RGBTrans          = F,
                hara              = T,
                haraType          = c("simple"),
                stat              = T,
                edge              = T,
                morpho            = T,
                pardem            = T,
                kernel            = 3,
                currentDataFolder = path_run,
                currentIdxFolder  = path_run,
                giLinks = giLinks)

# ----  start crown analysis ------------------------

### generic uavRST approach
# call seeding process
tPos <- uavRst::treepos(chmR,
                        minTreeAlt = minTreeAlt,
                        maxCrownArea = maxCrownArea,
                        join = 1,
                        thresh = 0.35,
                        giLinks = giLinks )

# we need treepos as segmentation seeding 
raster::writeRaster(tPos,"treepos.tif",overwrite = TRUE,NAflag = 0)

# now get synthetic image stack file
imageTrainFiles <- list.files(pattern="[.]envi$", path=path_run, full.names=TRUE)

# load the corresponding band names
load(paste0(path_run,"bandNames_rgbImg_.RData"))

# convert them to saga 
# NOTE this is an ugly workaround to force all files to the same extent
split2SAGA(imageTrainFiles,
           bandName=bandNames, 
           startBand=1,
           endBand=length(bandNames),
           refFn="chm.tif")

# call tree crown segmentation NOTE there are about 75 partly highly correlated channels 
# try to reduce and mention the thresholds
crowns <- chmseg_uav( treepos = tPos, 
                      segmentationBands = bandNames,
                      chm = chmR,
                      minTreeAlt = 3,
                      normalize = 0,
                      method = 0,
                      neighbour = 0,
                      majorityRadius = 3,
                      thVarFeature = 1.,
                      thVarSpatial = 1.,
                      thSimilarity = 0.000001,
                      giLinks = giLinks )


### Foresttools approach
crownsFT <- chmseg_FT(treepos = tPos,
                      chm = chmR,
                      minTreeAlt = minTreeAlt,
                      format = "polygons",
                      verbose = TRUE)


### rLiDAR approach
crownsRL <- chmseg_RL(chm=chmR,
                      treepos=tPos,
                      maxCrownArea=maxCrownArea,
                      exclusion=0.2)

### itcSeg approach
crownsITC<- chmseg_ITC(chm = chmR,
                       EPSG =3064,
                       movingWin = 3,
                       TRESHSeed = 0.45,
                       TRESHCrown = 0.55,
                       minTreeAlt = 2,
                       maxCrownArea = maxCrownArea)

print(mapview::mapview(crownsFT) +
        mapview::mapview(crownsRL) +
        mapview::mapview(crownsITC,zcol ="Height_m")+
        mapview::mapview(crowns,zcol="chmMAX") +
        mapview::mapview(chmR)
)



#--------  now treetop alternatives all of them are fast and reliable
# rlidar
tPosRL <- treepos_RL(chm =chmR, movingWin = 7, minTreeAlt = 2)

# lidR
tPosliR <- treepos_lidR(chm = chmR, movingWin = 7, minTreeAlt = 2)

# ForestTools
tPosFT <- treepos_ft(chm = chmR, minTreeAlt = 2, maxCrownArea = maxCrownArea)

# cut result is with reference
plot2<-sp::spTransform(plot2,CRSobj = raster::crs(proj4))
crownsITC<-sp::spTransform(crownsITC,CRSobj = raster::crs(proj4))
crowns<-sp::spTransform(crowns,CRSobj = raster::crs(proj4))
finalTreesITC<-rgeos::gIntersection(plot2,crownsITC,byid = TRUE)
finalTrees<-rgeos::gIntersection(plot2,crowns,byid = TRUE)
print(mapview::mapview(finalTreesITC) +
        mapview::mapview(finalTrees))


