# use case for the estimation of basic crown segmentation
# from an uav derived point cloud data set
# NOTE the ortho image is obligatory

# ---- define global parameters -----------------------------------------------
# 
# load package for linking  GI tools
require(link2GI)
require(uavRst)

# orthoimage filename
orthImg <- "moles.tif"

# rgb indices 
indices <- c("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI")   


# only post processing to avoid the point cloud to DSM/DEM operation
calculate_chm <- FALSE

# just process a clipped area for testing
only_postprocessing <- FALSE
#ext  <- raster::extent(498372,498472,5664417,5664513)
#ext  <- raster::extent(498300,498620,5664070,5664475)
#ext  <- raster::extent(498432,498545,5664204,5664302)
#ext  <- raster::extent(498404,498488,5664458,5664536)

# all sample trees
ext  <- raster::extent(498310,498610,5664085,5664470)

# define project folder
projRootDir <- "~/temp7/GRASS7"


# create project structure and export global pathes
link2GI::initProj(projRootDir = projRootDir,
                  projFolders = c("data/","output/","run/") )

# clean dirs
if (!only_postprocessing) unlink(paste0(path_run,"*"), force = TRUE)
raster::rasterOptions(tmpdir=path_tmp) 

# set working directory
setwd(path_run)

# link GDAL and SAGA
gdal <- link2GI::linkgdalUtils()
saga <- link2GI::linkSAGA()


# ----- start preprocessing ---------------------------------------------------

  cat("\n::: preprocess input data...\n")
  rgb <- raster::stack(paste0(path_data,orthImg))

  cat("::: calculate RGBI... \n")
  rgbI <- uavRst::rs_rgbIndices(rgb[[1]],rgb[[2]],rgb[[3]],indices)
  
  cat("\n convert RGBIs to SAGA... \n")
  i <- 1
  for (index in indices) {
    cat("convert ",index,"\n")
    r2saga(rgbI[[i]],index)
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
