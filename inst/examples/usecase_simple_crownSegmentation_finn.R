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

ext<- raster::extent(477393.,477460. ,5631938. , 5632003.)


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

### generic uavRST appproach
# call seeding process
treePos <- uavRst::fa_findTreePosition(chmR,
                                minTreeAlt = 2,
                                minCrownArea = 1,
                                maxCrownArea = 100,
                                join = 1, 
                                thresh = 0.01,
                                giLinks = giLinks )
saveRDS(treePos,file = paste0(path_output,"treePos_iws.rds"))
# workaround for strange effects with SAGA 
# even if all params are identical it is dealing with different grid systems
tPos<-raster::resample(treePos, chmR , method = 'bilinear')
tPos[tPos<=0]<-0
# statically writing of the two minimum raster for segmentation
raster::writeRaster(tPos,"treePos.tif",overwrite = TRUE,NAflag = 0)
raster::writeRaster(chmR,"chm.sdat",overwrite = TRUE,NAflag = 0)

# call tree crown segmentation 

rawCrowns <- uavRst::fa_crownSegmentation( treePos = tPos,
                                           chm =chmR,
                                           minTreeAlt =3,
                                           normalize = 0,
                                           method = 0,
                                           neighbour = 1,
                                           majority_radius = 3,
                                           thVarFeature = 1.,
                                           thVarSpatial = 1.,
                                           thSimilarity = 0.002,
                                           giLinks = giLinks )
 

### Foresttools approach
rawCrownsFT <- fa_crownSegementationFT(treePos = tPos, 
                        chm = chmR,
                        minTreeAlt = 2,
                        format = "polygons",
                        verbose = TRUE)


mapview::mapview(rawCrownsFT) + mapview::mapview(rawCrowns) 

cat("::: run post-classification...\n")



# export geojson
sf::st_write(sf::st_as_sf(statRawCrowns), "crowns.geojson",delete_dsn=TRUE,driver="GeoJSON")

# simple filtering of crownareas based on tree height min max area and artifacts at the analysis/image borderline
trees_crowns <- uavRst::fa_basicTreeCrownFilter(crownFn = paste0(path_run,"crowns.geojson"),
                                                minTreeAlt = 5,
                                                minCrownArea = 5,
                                                maxCrownArea = 150,
                                                mintreeAltParam = "chmQ20" )

# view it
mapview::mapview(plot2) 
mapview::mapview(rawCrowns[[2]]) +
mapview::mapview(chmR)  

# cut result is with reference
plot2<-sp::spTransform(plot2,CRSobj = raster::crs(proj4))
trees_crowns[[2]]<-sp::spTransform(trees_crowns[[2]],CRSobj = raster::crs(proj4))
finalTrees<-rgeos::gIntersection(plot2,trees_crowns[[2]],byid = TRUE)
mapview::mapview(finalTrees)


