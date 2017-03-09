# use case for the estimation of basic crown segmentation
# from an uav derived point cloud data set
# NOTE the ortho image is obligatory

# load package for linking  GI tools
require(link2GI)
require(uavRst)

# name of orthoimage
orthImg <- "ortho_05.tif"

# rgb indices 
indices <- c("GLI","HI")   

# only post processing to avoid the point cloud to DSM/DEM operation
only_postprocessing <- TRUE

# just process a clipped area for testing
crop <- TRUE
#ext  <- raster::extent(498372, 498472,  5664417 ,5664513)
#ext  <- raster::extent(498300,498620,5664070,5664475)
# bestry
ext  <- raster::extent(498432,498545,5664204,5664302)
#ext  <- raster::extent(498404,498488,5664458,5664536)

# define project folder
projRootDir <- "~/temp6/GRASS7"

# define uav point cloud data folder 
las_data_dir <- "/home/creu/apps/LAStools/bin/input"

# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","output/","run/","las/") )

# set working directory
setwd(path_run)

# clean run dir
unlink(paste0(path_run,"*"), force = TRUE)

# link GDAL and SAGA
gdal <- link2GI::linkgdalUtils()
saga <- link2GI::linkSAGA()

# CREATE dtm & dsm
if (!only_postprocessing) {
  # create DSM
  dsm <- uavRst::fa_pc2DSM(lasDir = las_data_dir,
                           gisdbase_path = projRootDir,
                           grid_size = "0.05")
  # create DTM
  dtm <- uavRst::fa_pc2DTM(lasDir = las_data_dir,
                           gisdbase_path = projRootDir,
                           thin_with_grid = "0.5",
                           level_max = "5" ,
                           grid_size = "0.05")
  dsmR <- dsm[[1]]
  dtmR <- dtm[[1]]
  
  # adjust dsm to dtm
  dsmR <- raster::resample(dsmR, dtmR, method = 'bilinear')
  
  # calculate CHM
  chmR <- dsmR - dtmR
  raster::writeRaster(chmR,paste0(path_output,"chm.tif"),
                      overwrite = TRUE)
} # !only_postprocessing

# now post processing and/or crop 
if (crop) {
  cat("\n:: crop & adjust input data\n")
  if (only_postprocessing) {
    chmR <- raster::raster(paste0(path_output,"chm.tif"))
    rgb <- raster::raster(paste0(path_output,"ortho.tif"))
    dsm <- raster::raster(paste0(path_output,"dsm.tif"))
    dah <- raster::raster(paste0(path_output,"dah.tif"))
    
  } 
  chmR <- raster::crop(chmR,ext)
  dsmR <- raster::crop(dsm,ext)
  dah <- raster::crop(dah,ext)
  uavRst:::R2SAGA(dah,"dah")
  rgb <- raster::stack(paste0(path_data,orthImg))
  rgb <- raster::crop(rgb,ext)
  rgb <- raster::resample(rgb, chmR, method = 'bilinear')
  raster::writeRaster(rgb,paste0(path_output,"ortho.tif"),
                      overwrite = TRUE)
  cat(":: calculate RGBI \n")
  rgbI <- uavRst::rs_rgbIndices(rgb[[1]],rgb[[2]],rgb[[3]],indices)
  
  #converting them to SAGA
  i <- 1
  for (index in indices) {
    uavRst:::R2SAGA(rgbI[[i]],index)
    i <- i + 1
  }
  
} else {
  cat("\n:: prepare and adjust ortho image\n")
  if (only_postprocessing) {
    chmR <- raster::raster(paste0(path_output,"chm.tif"))
    rgb <- raster::raster(paste0(path_data,"ortho.tif"))
    dah <- rgb <- raster::raster(paste0(path_output,"dah.tif"))
  }
  
  rgb <- raster::stack(paste0(path_data,orthImg))
  rgb <- raster::resample(rgb, chmR, method = 'bilinear')
  raster::writeRaster(rgb,paste0(path_output,"ortho.tif"),
                      overwrite = TRUE)
  cat(":: calculate RGBI \n")
  #converting them to SAGA
  rgbI <- uavRst::rs_rgbIndices(rgb[[1]],rgb[[2]],rgb[[3]],indices)
  i <- 1
  for (index in indices) {
    uavRst:::R2SAGA(rgbI[[i]],index)
    i <- i + 1
  }
}


#apply minium tree height to canopy height model (chm) -----------------------


# call tree crown segmentation 
crowns <- fa_tree_segementation(chmR,
                                minTreeAlt = 5,
                                is0_join = 1, 
                                is0_thresh = 0.5, 
                                majority_radius = 5.0, 
                                is3_sig1 = 0.01,
                                is3_leafsize = 8, 
                                is3_neighbour = 0,
                                is3_sig2 = 1.,
                                is3_threshold = 0.000025,
                                seeding = TRUE
)
cat(":: run post-classification...\n")
# very simple postclassifcation based on crownarea 
# TODO improve classification by training 
trees_crowns <- fa_basicTreeCrownFilter(crownFn = paste0(path_run,"treecrowns.shp"),
                                        minTreeAlt = 5,
                                        crownMinArea = 3,
                                        crownMaxArea = 225)

# pixvalues <- basicExtraction(x = chmR,fN = trees_crowns_2[[2]],responseCat = "ID")

crowns <- uavRst::fa_caMetrics(trees_crowns[[2]])
rgdal::writeOGR(obj = crowns, 
                layer = "crowns", 
                driver = "ESRI Shapefile", 
                dsn = path_run, 
                overwrite_layer = TRUE)
