# use case for the estimation of basic crown segmentation
# from an uav derived point cloud data set
# NOTE the ortho image is obligatory

# ---- define global parameters -----------------------------------------------
# 
# load package for linking  GI tools
require(link2GI)
require(uavRst)

# orthoimage filename
orthImg <- "ortho_05.tif"

# rgb indices 
indices <- c("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI")   


# only post processing to avoid the point cloud to DSM/DEM operation
calculate_chm <- FALSE

# just process a clipped area for testing
only_postprocessing <- TRUE
#ext  <- raster::extent(498372,498472,5664417,5664513)
#ext  <- raster::extent(498300,498620,5664070,5664475)
#ext  <- raster::extent(498432,498545,5664204,5664302)
#ext  <- raster::extent(498404,498488,5664458,5664536)

# all sample trees
ext  <- raster::extent(498310,498610,5664085,5664470)

# define project folder
projRootDir <- "~/temp6/GRASS7"

# define uav point cloud data folder 
las_data_dir <- "/home/creu/apps/LAStools/bin/input"

# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","output/","run/","las/") )

# clean dirs
if (!only_postprocessing) unlink(paste0(path_run,"*"), force = TRUE)
unlink(paste0(path_tmp,"*"), force = TRUE)
raster::rasterOptions(tmpdir=path_tmp) 

# set working directory
setwd(path_run)

# link GDAL and SAGA
gdal <- link2GI::linkgdalUtils()
saga <- link2GI::linkSAGA()

# ----- calculate DSM DTM & CHM  ---------------------------------------------------

# CREATE dtm & dsm
if (calculate_chm) {
  cat("\n::: calculate DSM DTM and CHM from point cloud data...\n")
  # create DSM
  dsm <- uavRst::fa_pc2DSM(lasDir = las_data_dir,
                           gisdbase_path = projRootDir,
                           otb_gauss_radius ="0.5",
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
  raster::writeRaster(dsmR,paste0(path_output,"dsm.tif"),
                      overwrite = TRUE)
  # calculate CHM
  chmR <- dsmR - dtmR
  raster::writeRaster(chmR,paste0(path_output,"chm.tif"),
                      overwrite = TRUE)
}

# ----- start preprocessing ---------------------------------------------------

  cat("\n::: preprocess input data...\n")
  dsm <- raster::raster(paste0(path_output,"dsm.tif"))
  dtm <- raster::raster(paste0(path_output,"dtm.tif"))
  rgb <- raster::stack(paste0(path_data,orthImg))

  dtmR <- raster::crop(dtm,ext)
  dsmR <- raster::crop(dsm,ext)
  dsmR <- raster::resample(dsmR, dtmR, method = 'bilinear')
  chmR <- dsmR - dtmR

  rgb <- raster::crop(rgb,ext)
  rgb <- raster::resample(rgb, chmR, method = 'bilinear')
  raster::writeRaster(rgb,paste0(path_output,"ortho.tif"),
                      overwrite = TRUE)

  cat("::: calculate RGBI... \n")
  rgbI <- uavRst::rs_rgbIndices(rgb[[1]],rgb[[2]],rgb[[3]],indices)
  
  cat("\n convert RGBIs to SAGA... \n")
  i <- 1
  for (index in indices) {
    cat("convert ",index,"\n")
    uavRst:::h_r2saga(rgbI[[i]],index)
    i <- i + 1
  }

# ----  start crown analysis --------------------------------------------------------

# call tree crown segmentation 
crowns <- fa_crown_segmentation(chmR,
                                minTreeAlt = 7,
                                crownMinArea = 3,
                                is0_join = 1, 
                                is0_thresh = 0.25, 
                                majority_radius = 5.0, 
                                is3_sig1 = 0.015,
                                is3_leafsize = 256, 
                                is3_neighbour = 0,
                                is3_sig2 = 2.5,
                                is3_threshold = 0.00005,
                                is3_seed_params = indices,
                                seeding = TRUE
)

# calculate potential insolation
uavRst::fa_pot_insol("chm",pi_day = "01/06/2017",pi_day_stop = "30/06/2017",pi_hour_step = 1.0)

# extract stats
polyStat <- xpolystat(c("tot","dir","dif","chm"),
                      spdf = "tree_crowns.shp")


cat("::: run post-classification...\n")

# calculate metrics of the crown geometries
crowns <- uavRst::fa_caMetrics(polyStat)

rgdal::writeOGR(obj = crowns,
                layer = "crowns", 
                driver = "ESRI Shapefile", 
                dsn = path_run, 
                overwrite_layer = TRUE)

# simple filtering of crownareas based on tree height min max area and artifacts at the analysis/image borderline
trees_crowns <- fa_basicTreeCrownFilter(crownFn = paste0(path_run,"crowns.shp"),
                                        minTreeAlt = 5,
                                        crownMinArea = 3,
                                        crownMaxArea = 225)

# pixvalues <- basicExtraction(x = chmR,fN = trees_crowns_2[[2]],responseCat = "ID")
 


# save results to shape
rgdal::writeOGR(obj = crowns, 
                layer = "crowns", 
                driver = "ESRI Shapefile", 
                dsn = path_run, 
                overwrite_layer = TRUE)
cat(":: ...finsihed \n")
