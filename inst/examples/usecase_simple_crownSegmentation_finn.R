# use case for the estimation of basic crown segmentation
# from an uav derived point cloud data set
# NOTE the ortho image is obligatory

# ---- define global parameters -----------------------------------------------
# 
# load package for linking  GI tools
require(link2GI)
require(uavRst)

# define project settings, data folders etc
# define project folder
projRootDir <- "~/proj/uav/thesis/finn"
# lidar data folder
las_data_dir <- "~/proj/uav/thesis/finn/data/sequoia/"
# proj subfolders
projFolders = c("data/","data/ref/","output/","run/","las/")
# export folders as global
global = TRUE
# with folder name plus following prefix
path_prefix = "path_"
# proj4 string of ALL data
proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "
extent <- c(477393.,477460. ,5631938. , 5632003.)
ext<- raster::extent(as.numeric(extent))

maxCrownArea = 150
minTreeAlt = 2

# create project structure and export global pathes
paths<-link2GI::initProj(projRootDir = projRootDir,
                   projFolders = projFolders,
                   global = TRUE,
                   path_prefix = path_prefix)

# referenz shape filename
plot2<-raster::shapefile(paste0(path_data,"ref/plot_UTM.shp"))

# link all CLI stuff
giLinks<-uavRst:::linkBuilder()

# clean dirs

unlink(paste0(path_run,"*"), force = TRUE)
raster::rasterOptions(tmpdir=path_run) 

# set working directory
setwd(path_run)


# ----- calculate DSM DTM & CHM FROM UAV POINT CLOUDS-----------------------------------------------

# create DSM
dsm <- uavRst::fa_pc2DSM(lasDir = las_data_dir,
                         gisdbase_path = projRootDir,
                         otb_gauss_radius ="0.5",
                         grid_size = "0.5",
                         GRASSlocation = "dsm",
                         grass_lidar_method = "mean",
                         giLinks = giLinks)
# create DTM
dtm <- uavRst::fa_pc2DTM(lasDir = las_data_dir,
                         gisdbase_path = projRootDir,
                         thin_with_grid = "0.5",
                         level_max = "5" ,
                         grid_size = "0.5",
                         giLinks = giLinks)

# take the rsulting raster files
dsmR <- dsm[[1]]
dtmR <- dtm[[1]]

# crop them to the test area
dsmR<-raster::crop(dsmR,ext)
dtmR<-raster::crop(dtmR,ext)
#raster::plot(dsmR)
#raster::plot(dtmR)

# if not already done adjust dsm to dtm
dsmR <- raster::resample(dsmR, dtmR , method = 'bilinear')

# calculate CHM
chmR <- dsmR - dtmR
# reset negative values to 0
chmR[chmR<0]<-0
chmR1<-chmR
# inverse chm
#chmR<- (- 1 * chmR) + raster::maxValue(chmR)
saveRDS(dtmR,file = paste0(path_output,"dtmR.rds"))
saveRDS(dsmR,file = paste0(path_output,"dsmR.rds"))
saveRDS(chmR,file = paste0(path_output,"chmR.rds"))





# ----  start crown analysis ------------------------

### generic uavRST approach
# call seeding process
tPos <- uavRst::treePos(chmR,
                                minTreeAlt = minTreeAlt,
                                minCrownArea = 1,
                                maxCrownArea = maxCrownArea,
                                join = 1, 
                                thresh = 0.01,
                                giLinks = giLinks )
saveRDS(tPos,file = paste0(path_output,"treePos_iws.rds"))
# workaround for strange effects with SAGA 
# even if all params are identical it is dealing with different grid systems
tPos<-raster::resample(tPos, chmR , method = 'bilinear')
tPos[tPos<=0]<-0
# statically writing of the two minimum raster for segmentation
raster::writeRaster(tPos,"treePos.tif",overwrite = TRUE,NAflag = 0)
raster::writeRaster(chmR,"chm.sdat",overwrite = TRUE,NAflag = 0)

# call tree crown segmentation 

crowns <- uavRst::chmSegmentation( treePos = tPos,
                                           chm =chmR,
                                           minTreeAlt = 3,
                                           normalize = 0,
                                           method = 0,
                                           neighbour = 0,
                                           majority_radius = 8,
                                           thVarFeature = 1.,
                                           thVarSpatial = 1.,
                                           thSimilarity = 0.003,
                                           giLinks = giLinks )
 

### Foresttools approach
crownsFT <- uavRst::chmSegmentationFT(treePos = tPos, 
                        chm = chmR,
                        minTreeAlt = minTreeAlt,
                        format = "polygons",
                        verbose = TRUE)


### rLiDAR approach
crownsRL <- uavRst::chmSegmentationRL(chm=chmR, 
                                    treePos=tPos, 
                                    maxCrownArea=maxCrownArea, 
                                    exclusion=0.2)

### itcSeg approach
crownsITC<- uavRst::chmSegmentationITC(chm = chmR,
                        EPSG_code =3064,
                        movingWin = 3,
                        TRESHSeed = 0.45,
                        TRESHCrown = 0.55,
                        minTreeAlt = 2,
                        maxCrownArea = maxCrownArea)

chmSegmentationFU(lasDir = las_data_dir,
                  grid_size = c(1),
                  fusionPercentile    = 37,
                  movingWin          = 3,
                  focalStatFun = "mean",
                  proj4 = proj4, #"+init=epsg:25832",
                  path = getwd(),
                  fusionCmd = NULL,
                  extent = ext)

# view it
mapview::mapview(crownsFT) + 
mapview::mapview(crownsRL) + 
mapview::mapview(crownsITC,zcol ="Height_m") +
mapview::mapview(crowns,zcol="chmMAX") +
mapview::mapview(chmR)



#--------  now treetop alternatives all of them are fast and reliable
# rlidar
tPosRL <- treePosRL(chm =chmR, movingWin = 7, minTreeAlt = 2) 

# lidR
tPosliR <- treePoslidR(chm = chmR, movingWin = 7, minTreeAlt = 2)

# ForestTools
tPosFT <- treePosFT(chm = chmR, minTreeAlt = 2, maxCrownArea = maxCrownArea)

# cut result is with reference
plot2<-sp::spTransform(plot2,CRSobj = raster::crs(proj4))
crownsITC<-sp::spTransform(crownsITC,CRSobj = raster::crs(proj4))
crowns<-sp::spTransform(crowns,CRSobj = raster::crs(proj4))
finalTreesITC<-rgeos::gIntersection(plot2,crownsITC,byid = TRUE)
finalTrees<-rgeos::gIntersection(plot2,crowns,byid = TRUE)
mapview::mapview(finalTreesITC) +
mapview::mapview(finalTrees)  


