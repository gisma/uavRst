# use case for the estimation of basic crown segmentation
# from an uav derived point cloud data set
# NOTE the ortho image is obligatory

# load package for linking  GI tools
require(link2GI)
require(uavRst)

# set minimum tree height
minTreeHeight <- 5

# name of orthoimage
 orthImg <- "rgb_75.tif"
   
# only post processing to avoid the point cloud to DSM/DEM operation
only_postprocessing <- FALSE

# just process a clipped area for testing
crop <- FALSE
ext  <- raster::extent(498372, 498472,  5664417 ,5664513)

# define project folder
filepath_base <- "~/temp6/GRASS7"

# define uav point cloud data folder 
las_data_dir <- "/home/creu/apps/LAStools/bin/input"

# create project structure and export global pathes
link2GI::initProj(projRootDir = "~/temp6/GRASS7",
                  projFolders = c("data/","output/","run/","las/") )

# set working directory
setwd(path_run)

# clean run dir
unlink(paste0(path_run,"*"), force = TRUE)

# link GDAL and SAGA
gdal <- link2GI::linkgdalUtils()
saga <- link2GI::linkSAGA()

if (!only_postprocessing) {
  # create DSM
  dsm <- uavRst::fa_pc2DSM(lasDir = las_data_dir,
                        gisdbase_path = filepath_base,
                        grid_size = "0.1")
  # create DTM
  dtm <- uavRst::fa_pc2DTM(lasDir = las_data_dir,
                        gisdbase_path = filepath_base,
                        thin_with_grid = "0.5",
                        level_max = "5" ,
                        grid_size = "0.1")
  dsmR <- dsm[[1]]
  dtmR <- dtm[[1]]
  
  # crop to clip
  if (crop){
    dtmR <- raster::crop(dtmR,ext)
    dsmR <- raster::crop(dsmR,ext)
  }
  
  # adjust dsm to dtm
  dsmR <- raster::resample(dsmR, dtmR, method = 'bilinear')
  
  # calculate CHM
  chmR <- dsmR - dtmR
  raster::writeRaster(chmR,paste0(path_output,"chm.tif"),
                      overwrite = TRUE)
}



if (crop) {
  cat("\n:: crop & adjust input data\n")
  if (only_postprocessing) chmR <- raster::raster(paste0(path_output,"chm.tif"))
  chmR <- raster::crop(chmR,ext)
  rgb <- raster::stack(paste0(path_data,orthImg))
  rgb <- raster::crop(rgb,ext)
  rgb <- raster::resample(rgb, chmR, method = 'bilinear')
  cat(":: calculate RGBI \n")
  indices <- c("VVI","VARI") #names(rgbI)
  rgbI <- uavRst::rs_rgbi(rgb)
  
  #converting them to SAGA
  i <- 1
  for (index in indices) {
    uavRst:::R2SAGA(rgbI[[i]],index)
    i <- i + 1
  }  
} else {
  cat("\n:: prepare and adjust ortho image\n")
  if (only_postprocessing) chmR <- raster::raster(paste0(path_output,"chm.tif"))
  rgb <- raster::stack(paste0(path_data,orthImg))
  rgb <- raster::resample(rgb, chmR, method = 'bilinear')
  cat(":: calculate RGBI \n")
  rgbI <- uavRst::rs_rgbi(rgb)
  #converting them to SAGA
  indices <- c("VVI","VARI")
  i <- 1
  for (index in indices) {
    uavRst:::R2SAGA(rgbI[[i]],index)
    i <- i + 1
  }
}

#apply minium tree height to canopy height model (chm) -----------------------
chmR[chmR < -minTreeHeight] <- minTreeHeight

if (only_postprocessing) {
  chmR <- raster::raster(paste0(path_output,"chmR.tif"))
}

# call tree crown segmentation 
microbenchmark::microbenchmark(crowns <- fa_tree_segementation(chmR),times=1,minTreeAlt=minTreeHeight)
