# use case for the estimation of basic tree/forest indices 
# from an uav derived point cloud data set

# load package for linking  GI tools
require(link2GI)
require(uavRst)

# only post
only_postprocessing <- TRUE

# define project folder
filepath_base <- "~/temp6/GRASS7"

# define uav point cloud data folder 
las_data_dir <- "/home/creu/apps/LAStools/bin/input"

# create project structure and export global pathes
link2GI::initProj(projRootDir = "~/temp6/GRASS7",
                  projFolders = c("data/","output/","run/","las/") )

# set working directory
setwd(path_run)

# just process a cut
crop <- TRUE
ext  <- raster::extent(498372, 498472,  5664417 ,5664513)

gdal <- link2GI::linkgdalUtils()
saga <- link2GI::linkSAGA()

if (!only_postprocessing) {
  # create DSM
  dsm <- uavRst::pc2DSM(lasDir = las_data_dir,
                        gisdbase_path = filepath_base,
                        grid_size = "0.1")
  # create DTM
  dtm <- uavRst::pc2DTM(lasDir = las_data_dir,
                        gisdbase_path = filepath_base,
                        thin_with_grid = "0.5",
                        level_max = "5" ,
                        grid_size = "0.1")
  dsmR <- dsm[[1]]
  dtmR <- dtm[[1]]
  
  # crop to clip
  dtmR <- raster::crop(dtmR,ext)
  dsmR <- raster::crop(dsmR,ext)
  # adjust dsm to dtm
  dsmR <- resample(dsmR, dtmR, method = 'bilinear')
  # calculate CHM
  chmR <- dsmR - dtmR
  raster::writeRaster(chmR,paste0(path_output,"chm_crop.tif"),
                      overwrite = TRUE)
  
}


if (crop) {
  cat(":: cropping input data\n")
  chmR <- raster::raster(paste0(path_output,"chm_crop.tif"))
  chmR <- raster::crop(chmR,ext)
  rgb <- raster::stack(paste0(path_data,"rgb_75.tif"))
  rgb <- raster::crop(rgb,ext)
  rgb <- raster::resample(rgb, chmR, method = 'bilinear')
  rgbI <- uavRst::rgbi(rgb)
  cat(":: calculationg indices\n")
  indices <- c("VVI","VARI") #names(rgbI)
  i <- 1
  for (index in indices) {
    raster::writeRaster(rgbI[[i]],paste0(path_run,index,".tif"),overwrite = TRUE)  
    gdalUtils::gdalwarp(paste0(path_run,index,".tif"), 
                        paste0(path_run,index,".sdat"), 
                        overwrite = TRUE,  
                        of = 'SAGA',
                        verbose = FALSE) #  calculate and convert inverse canopy height model (iChm)
    i <- i + 1
  }
  
  #raster::writeRaster(chmR,paste0(path_output,"rgb75.tif"),
  #                    overwrite = TRUE)
} else {
  # adjust dsm to dtm
  dsmR <- resample(dsmR, dtmR, method = 'bilinear')
  # calculate CHM
  chmR <- dsmR - dtmR
  raster::writeRaster(chmR,paste0(path_output,"chm_orig.tif"),
                      overwrite = TRUE)
  rgb <- raster::raster(paste0(path_data,"rgb_75.tif"))
  rgb <- resample(rgb, chmR, method = 'bilinear')
}



# the ratio of the above ground points to the total points is from 0 to 1 where 
# 0.0 represents no canopy and 1.0 very dense canopy
#pTot <- pcagR + pcgrR
#hFdensity <- pcagR / pTot
# raster::writeRaster(hFdensity,paste0(path_run,"hFdensity.tif"),overwrite = TRUE)
# gdalUtils::gdalwarp(paste0(path_run,"hFdensity.tif"), 
#                     paste0(path_run,"hFdensity.sdat"), 
#                     overwrite = TRUE,  
#                     of = 'SAGA',
#                     verbose = FALSE) 

# ----- calculate canopy height model (chm) -----------------------

chmR[chmR < -minTreeAlt] <- minTreeAlt
raster::writeRaster(chmR,paste0(path_run,"chm.tif"),
                    overwrite = TRUE)
# convert to SAGA
gdalUtils::gdalwarp(paste0(path_run,"chm.tif"), 
                    paste0(path_run,"chm.sdat"), 
                    overwrite = TRUE,  
                    of = 'SAGA',
                    verbose = FALSE) #  calculate and convert inverse canopy height model (iChm)


crowns <- foa_tree_segementation(chmFn =  paste0(path_run,"chm.tif"))
