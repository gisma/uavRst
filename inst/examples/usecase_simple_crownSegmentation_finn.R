# use case for the estimation of basic crown segmentation
# from an uav derived point cloud data set
# NOTE the ortho image is obligatory

# ---- define global parameters -----------------------------------------------
# 
# load package for linking  GI tools
require(link2GI)
require(uavRst)

# orthoimage filename
#orthImg <- "ortho_05.tif"
#load("/home/creu/lehre/msc/active/msc-2017/data/gis/output/0_5_10_15_20_50stat.RData")
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

# define uav point cloud data folder 
las_data_dir <- "/home/creu/Downloads"
projFolders = c("data/","output/","run/","las/")
global = TRUE
path_prefix = "path_"

# create project structure and export global pathes
paths<-link2GI::initProj(projRootDir = projRootDir,
                   projFolders = c("data/","output/","run/","las/"),
                   global = TRUE,
                   path_prefix = "path"
                     )

# clean dirs
if (!only_postprocessing) unlink(paste0(path_run,"*"), force = TRUE)
unlink(paste0(path_tmp,"*"), force = TRUE)
raster::rasterOptions(tmpdir=path_tmp) 

# set working directory
setwd(path_run)

# link GDAL and SAGA
#gdal <- link2GI::linkGDAL()
#saga <- link2GI::linkSAGA()
#otb <- link2GI::linkOTB()
#makGlobalVar("path_OTB",otb$pathOTB)
# ----- calculate DSM DTM & CHM  ---------------------------------------------------


  dsm <- uavRst::fa_pc2DSM(lasDir = las_data_dir,
                           gisdbase_path = projRootDir,
                           otb_gauss_radius ="0.5",
                           grid_size = "0.05",
                           GRASSlocation = "dsm",
                           grass_lidar_method = "range")



chmR<-dsm[[1]]
raster::writeRaster(chmR,"chm.tif",
                    overwrite = TRUE)
# index for segmentation default is chm
indices <- c("chm")
for (item in indices){
  gdalUtils::gdalwarp(paste0(path_run,item,".tif"), 
                      paste0(path_run,item,".sdat"), 
                      overwrite = TRUE,  
                      of = 'SAGA',
                      verbose = FALSE)
}


# ----  start crown analysis --------------------------------------------------------

# call seeding process
seeds <- uavRst::fa_treeSeeding(chmR,
                                minTreeAlt = 12.5,
                                crownMinArea = 3,
                                crownMaxArea = 225,
                                is0_join = 1, 
                                is0_thresh = 0.20, 
                                
)

# call tree crown segmentation 
rawCrowns <- uavRst::fa_crown_segmentation(seeds = seeds,
                                        majority_radius = 2.0,
                                        is3_thVarFeature = 0.5,
                                        is3_thVarSpatial = 0.5,
                                        is3_thSimilarity = 0.0005,
                                        is3_seed_params = indices,
)

cat("::: run post-classification...\n")
# extract stats
 statRawCrowns <- uavRst::xpolystat(c("chm"),
                               spdf = crowns)

# calculate metrics of the crown geometries
#crowns <- uavRst::fa_caMetrics(statRawCrowns)

# export geojson
sf::st_write(sf::st_as_sf(crowns), "crowns.geojson",delete_dsn=TRUE,driver="GeoJSON")
# rgdal::writeOGR(obj = statRawCrowns,
#                 layer = "statRawCrowns", 
#                 driver = "ESRI Shapefile", 
#                 dsn = path_run, 
#                 overwrite_layer = TRUE)
# simple filtering of crownareas based on tree height min max area and artifacts at the analysis/image borderline
trees_crowns <- uavRst::fa_basicTreeCrownFilter(crownFn = paste0(path_run,"crowns.geojson"),
                                                minTreeAlt = 5,
                                                crownMinArea = 5,
                                                crownMaxArea = 150,
                                                mintreeAltParam = "chmQ20"
)

# vie it
dsm<-mapview::mapview(chmR)
tc<-mapview::mapview(trees_crowns)
tc+dsm

# cut result is with reference
finalTrees<-rgeos::gIntersection(plot2,trees_crowns[[2]],,byid = TRUE,)
plot(finalTrees)


