# use cases for UAV data based crown segmentation

# ---- define global parameters -----------------------------------------------
#
# load package for linking  GI tools
require(link2GI)

require(uavRst)

# define project settings, data folders etc
projRootDir <- "~/proj/uav/thesis/finn"

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
las_data <- "~/proj/uav/thesis/finn/output/477119_673_5631675_040_477733_694_5632284_900.las"
las_data <- "~/proj/uav/thesis/finn/output/477369_800_5631924_000_477469_800_5632024_000.las"
las_data <- "~/proj/uav/thesis/finn/data/sequoia/477369_800_5631924_000_477469_800_5632024_000.las"
las_data <- "~/proj/uav/thesis/finn/data/sequoia/uniwald_sequoia2.las"
actual_grid_size<-0.1
splineNumber<-7
thingrid<- 4.

# link all CLI stuff
giLinks<-get_gi()

# clean dirs

unlink(paste0(path_run,"*"), force = TRUE)
raster::rasterOptions(tmpdir=path_run)

# set working directory
setwd(path_run)

# ----- calculate DSM DTM & CHM FROM UAV POINT CLOUDS-----------------------------------------------
#las_data<-"~/proj/uav/thesis/finn/output/477375_00_5631900_00_477475_00_5632000_00.las"
# create DSM

dsm <- pc3D_dsm(lasDir = las_data,
                      gisdbase_path = projRootDir,
                      type_smooth = "no",
                      grid_size = actual_grid_size,
                      GRASSlocation = "dsm",
                      grass_lidar_method = "max",
                      #cutExtent = cutExtent,
                      giLinks = giLinks)

dtm <- pc2D_dtm(laspcFile = las_data,
               gisdbase_path = projRootDir,
               tension = 20 ,
               sampleGridSize = 25,
               targetGridSize = actual_grid_size,
               giLinks = giLinks)

# # create DTM
# dtm2 <- pc3D_dtm(lasDir = las_data,
#                       gisdbase_path = projRootDir,
#                       thin_with_grid = 1.,
#                       level_max = 5 ,
#                       grid_size = actual_grid_size,
#                    #   cutExtent = cutExtent,
#                       giLinks = giLinks)

# take the rsulting raster files
dsmR <- dsm[[1]]
dtmR <- dtm
#dtmR2<- raster::crop(dtmR,dsmR)
#dtmR <- raster::resample(dtmR2, dsmR , method = 'bilinear')
rgbImg <- raster::raster(rgbImgFn)
dtm2<- raster::crop(dtmR,rgbImg)
dsm2<- raster::crop(dsmR,rgbImg)
dtm2 <- raster::resample(dtm2, dsm2 , method = 'bilinear')
rgbR <- raster::resample(rgbImg, dsm2 , method = 'bilinear')

raster::writeRaster(rgbImg,paste0(path_output,"rgbImg.tif"),overwrite=TRUE)
             # if not already done adjust dsm to dtm


# calculate CHM
chmR <- dsm2 - dtm2

# reset negative values to 0
chmR[chmR<0]<-0
raster::writeRaster(chmR,paste0(path_output,"chm.tif"),overwrite=TRUE)
# inverse chm
#chmR<- (- 1 * chmR) + raster::maxValue(chmR)

# saveRDS(dtmR,file = paste0(path_output,"dtmR.rds")
# saveRDS(dsmR,file = paste0(path_output,"dsmR.rds"))
# saveRDS(chmR,file = paste0(path_output,"chmR.rds"))

# ----  start crown analysis ------------------------

### generic uavRST approach
# call seeding process
tPos <- uavRst::treepos(chmR,
                        minTreeAlt = minTreeAlt,
                        maxCrownArea = maxCrownArea,
                        join = 1,
                        thresh = 0.35,
                        giLinks = giLinks )
saveRDS(tPos,file = paste0(path_output,"treepos_iws.rds"))
# workaround for strange effects with SAGA
# even if all params are identical it is dealing with different grid systems
tPos<-raster::resample(tPos, chmR , method = 'bilinear')
tPos[tPos<=0]<-0
# statically writing of the two minimum raster for segmentation
raster::writeRaster(tPos,"treepos.tif",overwrite = TRUE,NAflag = 0)
raster::writeRaster(chmR,"chm.sdat",overwrite = TRUE,NAflag = 0)

# call tree crown segmentation

crowns <- chmseg_uav( treepos = tPos,
                      chm =chmR,
                      minTreeAlt = 3,
                      normalize = 0,
                      method = 0,
                      neighbour = 0,
                      majority_radius = 5,
                      thVarFeature = 1.,
                      thVarSpatial = 1.,
                      thSimilarity = 0.003,
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
                       EPSG_code =3064,
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


