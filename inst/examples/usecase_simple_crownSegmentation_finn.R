# use case for the estimation of basic crown segmentation
# from an uav derived point cloud data set
# NOTE the ortho image is obligatory

# ---- define global parameters -----------------------------------------------
# 
# load package for linking  GI tools
require(link2GI)
require(uavRst)

# orthoimage filename
plot2<-raster::shapefile("/home/creu/lehre/msc/active/msc-2017/data/gis/input/ref/plot_UTM.shp")

# rgb indices 
#indices <- c("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI")   


# only post processing to avoid the point cloud to DSM/DEM operation
#calculate_chm <- FALSE

# just process a clipped area for testing
#only_postprocessing <- FALSE
#ext  <- raster::extent(498372,498472,5664417,5664513)
#ext  <- raster::extent(498300,498620,5664070,5664475)
#ext  <- raster::extent(498432,498545,5664204,5664302)
#ext  <- raster::extent(498404,498488,5664458,5664536)

#plot2
ext<- raster::extent(477393.,477460. ,5631938. , 5632003.)
# all sample trees
#ext  <- raster::extent(498310,498610,5664085,5664470)

# define project folder
projRootDir <- "~/proj/uav/thesis/finn"

# define uav point cloud data folder mapview(plot2)
las_data_dir <- "~/proj/uav/thesis/finn/data/sequoia/"
projFolders = c("data/","output/","run/","las/")
global = TRUE
path_prefix = "path_"
proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "
# create project structure and export global pathes
paths<-link2GI::initProj(projRootDir = projRootDir,
                   projFolders = projFolders,
                   global = TRUE,
                   path_prefix = path_prefix)

giLinks<-uavRst:::linkBuilder()

# clean dirs

unlink(paste0(path_run,"*"), force = TRUE)
raster::rasterOptions(tmpdir=path_run) 

# set working directory
setwd(path_run)

# link GDAL and SAGA
#gdal <- link2GI::linkGDAL()
#saga <- link2GI::linkSAGA()
#otb <- link2GI::linkOTB()
#makGlobalVar("path_OTB",otb$pathOTB)
# ----- calculate DSM DTM & CHM  ---------------------------------------------------
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

# inverse chm
#chmR<- (- 1 * chmR) + raster::maxValue(chmR)
#raster::plot(chmR)
#mapview::mapview(chmR)+plot2
# index for segmentation default is chm
# indices <- c("chm")
# for (item in indices){
  # gdalUtils::gdalwarp(paste0(path_run,item,".tif"), 
  #                     paste0(path_run,item,".sdat"), 
  #                     overwrite = TRUE,  
  #                     of = 'SAGA',
  #                     verbose = FALSE)
#   raster::writeRaster(chmR,paste0(path_run,item,".sdat"),overwrite = TRUE)
# }


# ----  start crown analysis ------------------------session(--------------------------------

# call seeding process
seeds <- uavRst::fa_treeSeeding(chmR,
                                minTreeAlt = 5,
                                crownMinArea = 3,
                                crownMaxArea = 125,
                                is0_join = 1, 
                                is0_thresh = 0.10 
                                
)
# workaround for strange effects with SAGA 
# even if all params are identical it is dealing with different grid systems
seeds <- raster::resample(seeds, chmR , method = 'bilinear')
seeds[seeds<=0]<-0
# statically writing of the two minimum raster for segmentation
raster::writeRaster(seeds,"seed.sdat",overwrite = TRUE,NAflag = 0)
raster::writeRaster(chmR,"chm.sdat",overwrite = TRUE)

# call tree crown segmentation 
rawCrowns <- uavRst::fa_crown_segmentation(
                                        majority_radius = 3,
                                        is3_thVarFeature = .09,
                                        is3_thVarSpatial = .09,
                                        is3_thSimilarity = 0.00001,
                                        giLinks = giLinks
)

cat("::: run post-classification...\n")

# extract chm stats by potential crown segments
 statRawCrowns <- uavRst::xpolystat(c("chm"),
                               spdf = rawCrowns)

# export geojson
sf::st_write(sf::st_as_sf(statRawCrowns), "crowns.geojson",delete_dsn=TRUE,driver="GeoJSON")

# simple filtering of crownareas based on tree height min max area and artifacts at the analysis/image borderline
trees_crowns <- uavRst::fa_basicTreeCrownFilter(crownFn = paste0(path_run,"crowns.geojson"),
                                                minTreeAlt = 5,
                                                crownMinArea = 5,
                                                crownMaxArea = 150,
                                                mintreeAltParam = "chmQ20"
)

# view it
mapview::mapview(plot2) +
mapview::mapview(trees_crowns[[2]]) +
mapview::mapview(chmR)  

# cut result is with reference
plot2<-sp::spTransform(plot2,CRSobj = raster::crs(proj4))
trees_crowns[[2]]<-sp::spTransform(trees_crowns[[2]],CRSobj = raster::crs(proj4))
finalTrees<-rgeos::gIntersection(plot2,trees_crowns[[2]],,byid = TRUE,)
mapview::mapview(finalTrees)


