# use case canopy height model (chm) based on a UAV derived point cloud data set
# ---- define global parameters -----------------------------------------------
#
# load package for linking  GI tools
require(link2GI)

require(uavRst)

# define project settings, data folders etc
# define project folder
projRootDir <- "~/proj/uav/thesis/finn"

# lidar data  can be a foldr or a file
las_data <- "~/proj/uav/thesis/finn/output/477375_000_5631900_000_477475_000_5632000_000.las"

# proj subfolders
projFolders = c("data/","data/ref/","output/","run/","las/")

# export folders as global
global = TRUE

# with folder name plus following prefix
path_prefix = "path_"

# proj4 string of ALL data
proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "
# cutExtent <- c(477393.,477460. ,5631938. , 5632003.) # old extent sharply cutted
cutExtent <- c(477375.,477475. ,5631900. , 5632000.)
ext<- raster::extent(as.numeric(cutExtent))

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
giLinks<-get_gi()

# clean dirs

unlink(paste0(path_run,"*"), force = TRUE)
raster::rasterOptions(tmpdir=path_run)

# set working directory
setwd(path_run)

# ----- calculate DSM DTM & CHM FROM UAV POINT CLOUDS-----------------------------------------------
#las_data<-"~/proj/uav/thesis/finn/output/477375_00_5631900_00_477475_00_5632000_00.las"
# create DSM
dsm <- uavRst::pc3D_dsm(lasDir = las_data,
                      gisdbasePath = projRootDir,
                      otb_gauss_radius ="4.1",
                      gridSize = "1.0",
                      GRASSlocation = "dsm",
                      grass_lidar_method = "max",
                      cutExtent = cutExtent,
                      giLinks = giLinks)
# create DTM
dtm <- uavRst::pc2D_dtm(lasDir = paste0(path_run,dsm[[4]]),
                      gisdbasePath = projRootDir,
                      thinGrid = "0.5",
                      splineNumber = "4" ,
                      gridSize = "0.25",
                      cutExtent = cutExtent,
                      giLinks = giLinks)

# take the rsulting raster files
dsmR <- dsm[[1]]
dtmR <- dtm[[1]]

# calculate CHM
chmR <- dsmR - dtmR

# reset negative values to 0
chmR[chmR<0]<-0

mapview::mapview(chmR)
