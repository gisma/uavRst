# use case for the estimation of basic tree/forest indices 
# from an uav derived point cloud data set

# load package for linking  GI tools
require(link2GI)
require(uavRst)

# only post
only_postprocessing <- TRUE

# type of crown segmentation
segType       <- 3

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
  
  if (crop) {
    # crop to clip
    dtmR <- raster::crop(dtmR,ext)
    dsmR <- raster::crop(dsmR,ext)
    # adjust dsm to dtm
    dsmR <- resample(dsmR, dtmR, method = 'bilinear')
    # calculate CHM
    chmR <- dsmR - dtmR
    raster::writeRaster(chmR,paste0(path_output,"chm_crop.tif"),
                        overwrite = TRUE)
  } else {
    # adjust dsm to dtm
    dsmR <- resample(dsmR, dtmR, method = 'bilinear')
    # calculate CHM
    chmR <- dsmR - dtmR
    raster::writeRaster(chmR,paste0(path_output,"chm_orig.tif"),
                        overwrite = TRUE)}
} 


# ---------- set tree thresholds ---------------------------------------------

# tree height 
minTreeAlt    <- 3   # -thresholdfor minimum tree altitude in meter

# tree nodes (possible sub-trees)
thtreeNodes   <- 6   # minimum number of ldd connections

# sqm crowns (only dominant crowns)
crownMinArea  <- 3   #(approx 1.25 m diameter)
crownMaxArea  <- 150 #(approx 17.8 m diameter)

# --------  postclassification thresholds----------------
WLRatio       <- 0.5 # crown width length ratio
thLongit      <- 0.5 # crown longitudiness 
solidity      <- 1.0 # solidity 


#----------- segementation thresholds -------------------

# --- watershed segementation (imagery_segmentation 0) for segType=2
is0_output    <- 0   # 0= seed value 1=segment id
is0_join      <- 1     # 0=no join, 1=seed2saddle diff, 2=seed2seed diff
is0_thresh    <- 0.5 # threshold for join difference in m

# --- growing segementation (imagery_segmentation 3) for segType=3
is3_leafsize  <- 8
is3_normalize <- 0
is3_neighbour <- 1
is3_method    <- 0
is3_sig1      <- 1.100000
is3_sig2      <- 1.100000
is3_threshold <- 0.000000

# --- itcSegement for segType=4
itc_seed      <- 0.45
itc_crown     <- 0.55

#--------- start core script     ---------------------------------------------

# ------ calculate horizontal "Forest" density (hFdensity) --------------------

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


# ----------  segType = 2 -----------------------------------------------------

if (segType == 2) {
  
  # (SAGA) create watershed crowns segmentation using imagery_segmentation 0 
  ret <- system(paste0(sagaCmd, " imagery_segmentation 0 ",
                       " -GRID ",path_run,"chm.sgrd",
                       " -SEGMENTS ",path_run,"crownsHeight.sgrd",
                       " -SEEDS ",path_run,"rawTrees.shp",
                       " -OUTPUT ",is0_output, 
                       " -DOWN 1", 
                       " -JOIN ",is0_join,
                       " -THRESHOLD ", is0_thresh, 
                       " -EDGE 1"),intern = TRUE)
  ret <- system(paste0(sagaCmd, " shapes_grid 6 ",
                       " -GRID ",path_run,"crownsHeight.sgrd",
                       " -POLYGONS ",path_run,"crownsHeight.shp",
                       " -CLASS_ALL 1",
                       " -CLASS_ID 1.000000",
                       " -SPLIT 1"),intern = TRUE)
  
  # calculate statistics for each crown 
  system(paste0(sagaCmd, " shapes_grid 2 ",
                " -GRIDS ",path_run,"chm.sgrd ",
                " -POLYGONS ",path_run,"crownsHeight.shp",
                " -NAMING 1",
                " -METHOD 2",
                " -COUNT 1 -MIN  1 -MAX 1 -RANGE 1 -SUM 1 -MEAN 1 -VAR 1 -STDDEV 1",
                " -QUANTILE 10",
                " -PARALLELIZED 1",
                " -RESULT ",path_run,"crownsHeightStat.shp"))
  
  # dirty combining of data tables
  ch <- rgdal::readOGR(path_run,"crownsHeight")
  names(ch) <- gsub(names(ch),pattern = "\\NAME",replacement = "NAME1")
  names(ch) <- gsub(names(ch),pattern = "\\ID",replacement = "ID1")
  names(ch) <- gsub(names(ch),pattern = "\\VALUE",replacement = "VALUE1")
  stats     <- rgdal::readOGR(path_run,"crownsHeightStat")
  names(ch) <- gsub(names(ch),pattern = "\\crownsHeigh",replacement = "crownsHeigh1")
  ch@data   <- cbind(ch@data,stats@data)
  names(ch) <- gsub(names(ch),pattern = "\\.",replacement = "")
  rgdal::writeOGR(obj = ch, 
                  layer = "crownsHeigh", 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  
  #  make crown vector data set and do basic filtering 
  trees_crowns_2 <- classifyTreeCrown(crownFn = paste0(path_run,"crownsHeigh.shp"),  
                                      funNames = c("eccentricityboundingbox","solidity"),
                                      minTreeAlt = minTreeAlt, 
                                      crownMinArea = crownMinArea, 
                                      crownMaxArea = crownMaxArea, 
                                      solidity = solidity, 
                                      WLRatio = WLRatio)
  rgdal::writeOGR(obj = trees_crowns_2[[2]],
                  layer = "trees_crowns_2", 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  # extraction of the data values for each crown 
  #cat("extaction of raster values for each crown. will probably run a while...\n")
  #pixvalues <- basicExtraction(x = chmR,fN = trees_crowns_2[[2]],responseCat = "NAME")
  
  # ----------------------
  
  # in addition we derive alternatively trees from the initial seedings 
  # read from the analysis (imagery_segmentation 0)
  trees <- rgdal::readOGR(path_run,"rawTrees")
  trees <- trees[trees$VALUE > minTreeAlt ,] 
  trees@proj4string <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  # converting them to raster due to the fact of speeding up the process
  # vector operations with some mio points/polys are cumbersome using sp objects in R
  # optionally you could use the sf package...
  rawTrees  <-  dsmR * 0.0
  maskCrown <-  dsmR * 0.0
  # rasterize is much to slow for big vec data 
  # so we do it the long run
  
  # raster::rasterize(crowns,mask=TRUE,rawCrowns)
  raster::writeRaster(rawTrees,paste0(path_run,"rawTrees.tif"),overwrite = TRUE)
  raster::writeRaster(maskCrown,paste0(path_run,"maskCrown.tif"),overwrite = TRUE)
  ret <- system(paste0("gdal_rasterize ",
                       path_run,"rawTrees.shp ", 
                       path_run,"rawTrees.tif",
                       " -l rawTrees",
                       " -a VALUE"),intern = TRUE)
  ret <- system(paste0("gdal_rasterize ",
                       path_run,"crowns.shp ", 
                       path_run,"maskCrown.tif",
                       " -l crowns",
                       " -burn 1"),intern = TRUE)
  rawTrees  <- raster::raster(paste0(path_run,"rawTrees.tif"))
  maskCrown <- raster::raster(paste0(path_run,"maskCrown.tif"))
  # now we reclassify the areas for latter operation
  maskCrown[maskCrown == 0] <- NA
  maskCrown[maskCrown == 1] <- 0
  # addition with NA and zero mask aout all na areas
  sTr <-  rawTrees + maskCrown
  sTr[sTr <= 0] <- NA
  # and reconvert it
  raster::writeRaster(sTr,paste0(path_run,"sTr.tif"),overwrite = TRUE)
  gdalUtils::gdal_translate(paste0(path_run,"sTr.tif"),
                            paste0(path_run,"sTr.xyz") ,
                            of = "XYZ",
                            overwrite = TRUE,
                            verbose = FALSE) 
  # make seedTree vector data
  sTr <- data.frame(data.table::fread(paste0(path_run,"sTr.xyz")))
  sTr <- sTr[sTr$V3 != "-Inf",] 
  sTr <- sTr[sTr$V3 >= minTreeAlt ,]
  sp::coordinates(sTr) <- ~V1+V2
  colnames(sTr@data) <- "crownsHeigh"
  sp::proj4string(sTr) <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  # save as shapefile
  rgdal::writeOGR(obj = sTr, 
                  layer = "sTr", 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  raster::plot(trees_crowns_2[[1]])
  raster::plot(trees_crowns_2[[2]])
  raster::plot(sTr)
  #
  # ----------  segType = 3 -----------------------------------------------------
  # 
} else if (segType == 3) {
  # create local maxima 
  system(paste0(sagaCmd," shapes_grid 9 ", 
                " -GRID ",path_run,"chm.sgrd",
                " -MAXIMA ",path_run,"maxChm.shp"))
  # create raw file to raster
  tmp <- dsmR * 0
  tmp[tmp@data@values == 0] <- NA
  raster::writeRaster(tmp,paste0(path_run,"seeds.tif"),overwrite = TRUE)  
  
  # rasterize and convert the seed data
  ret <- system(paste0("gdal_rasterize ",
                       path_run,"maxChm.shp ", 
                       path_run,"seeds.tif",
                       " -l maxChm",
                       " -a Z"), intern = TRUE)            
  
  gdalUtils::gdalwarp(paste0(path_run,"seeds.tif"),
                      paste0(path_run,"seeds.sdat"), 
                      of = 'SAGA',
                      overwrite = TRUE,
                      verbose = FALSE) 
  
  system(paste0(sagaCmd," grid_tools 15 ", 
                " -INPUT ",path_run,"seeds.sgrd",
                " -RESULT ",path_run,"seeds2.sgrd",
                " -METHOD 0",
                " -OLD 0.00",
                " -NEW 0.00",
                " -SOPERATOR 0",
                " -NODATAOPT 0",
                " -NODATA 0.0",
                "  -RESULT_NODATA_CHOICE 1", 
                " -RESULT_NODATA_VALUE 0.000000"))
  
  # SAGA Seeded Region Growing segmentation (imagery_segmentation 3)
  system(paste0(sagaCmd, " imagery_segmentation 3 ",
                " -SEEDS "   ,path_run,"seeds2.sgrd",
                " -FEATURES "   ,path_run,"chm.sgrd",
                " -SEGMENTS "   ,path_run,"crownsHeight.sgrd",
                " -LEAFSIZE "   ,is3_leafsize,
                " -NORMALIZE ",is3_normalize,
                " -NEIGHBOUR ",is3_neighbour, 
                " -METHOD ",is3_method,
                " -SIG_1 ",is3_sig1,
                " -SIG_2 ",is3_sig2,
                " -THRESHOLD ",is3_threshold))
  
  ret <- system(paste0(sagaCmd, " shapes_grid 6 ",
                       " -GRID ",path_run,"crownsHeight.sgrd",
                       " -POLYGONS ",path_run,"crownsHeight.shp",
                       " -CLASS_ALL 1",
                       " -CLASS_ID 1.000000",
                       " -SPLIT 1"),intern = TRUE)
  
  # calculate statistics for each crown 
  system(paste0(sagaCmd, " shapes_grid 2 ",
                " -GRIDS ",path_run,"chm.sgrd ",
                " -POLYGONS ",path_run,"crownsHeight.shp",
                " -NAMING 1",
                " -METHOD 2",
                " -COUNT 1 -MIN  1 -MAX 1 -RANGE 1 -SUM 1 -MEAN 1 -VAR 1 -STDDEV 1",
                " -QUANTILE 10",
                " -PARALLELIZED 1",
                " -RESULT ",path_run,"crownsHeightStat.shp"))
  
  # dirty combining of data tables
  ch <- rgdal::readOGR(path_run,"crownsHeight")
  names(ch) <- gsub(names(ch),pattern = "\\NAME",replacement = "NAME1")
  names(ch) <- gsub(names(ch),pattern = "\\ID",replacement = "ID1")
  names(ch) <- gsub(names(ch),pattern = "\\VALUE",replacement = "VALUE1")
  stats     <- rgdal::readOGR(path_run,"crownsHeightStat")
  names(ch) <- gsub(names(ch),pattern = "\\crownsHeigh",replacement = "crownsHeigh1")
  ch@data   <- cbind(ch@data,stats@data)
  names(ch) <- gsub(names(ch),pattern = "\\.",replacement = "")
  rgdal::writeOGR(obj = ch, 
                  layer = "crownsHeigh", 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  trees_crowns_3 <- classifyTreeCrown(crownFn = paste0(path_run,"crownsHeigh.shp"),  
                                      funNames = c("eccentricityboundingbox","solidity"),
                                      minTreeAlt = minTreeAlt, 
                                      crownMinArea = crownMinArea, 
                                      crownMaxArea = crownMaxArea, 
                                      solidity = solidity, 
                                      WLRatio = WLRatio)
  
  rgdal::writeOGR(obj = trees_crowns_3[[2]], 
                  layer = "crowns", 
                  driver = "ESRI Shapefile", 
                  dsn = path_run, 
                  overwrite_layer = TRUE)
  # pixvalues <- basicExtraction(x = chmR,fN = trees_crowns_2[[2]],responseCat = "ID")
  
  
  #  TODO postclassification stuff
  
  
  # ----------  segType = 4 -----------------------------------------------------
  #  
} else if (segType == 4) {
  EPSG <- 32632
  require(itcSegment)
  itcSeg <- itcSegment::itcIMG(imagery = chmR,
                               epsg = EPSG,
                               TRESHSeed = itc_seed,
                               TRESHCrown = itc_crown,
                               th = minTreeAlt,
                               ischm = TRUE,
                               DIST = sqrt(crownMaxArea/pi)*2)
}

