library(lidR)
# https://github.com/Jean-Romain/lidR/wiki/Rasterizing-perfect-canopy-height-models
gridSize<-0.5
# read data
lasfile<-path.expand("~/proj/uav/thesis/finn/output/477375_000_5631900_000_477475_000_5632000_000.las")
las_orig = readLAS(lasfile)
las<-las_orig
#grid_metrics(las, max(Z), 0.5) %>% plot

dtm = grid_terrain(las_orig, method = "kriging", k = 10L,res = 5)
dtm = as.raster(dtm)
plot(dtm)

lasTool(tool="lasthin" , lasFile = lasfile,outpath=path_run,keep_class = 2 , thin_with_grid = 2.)
lasred<-path.expand("~/proj/uav/thesis/finn/run/477375_000_5631900_000_477475_000_5632000_000_reduced.las")
las_red = readLAS(lasred)
# Classify ground points see example # https://github.com/Jean-Romain/lidR/wiki/Rasterizing-perfect-canopy-height-models
ws = seq(3,48,3)
th = seq(0.1, 3, length.out = length(ws))
#lasground(las_red, "pmf", ws, th)
dtm3 = grid_terrain(las_red, method = "kriging", k = 10L)
dtm2 = as.raster(dtm3)
plot(dtm2)


groundReturns = las %>% lasfilterground
raw<-lidR::grid_metrics(groundReturns,mean(Z), 10)
#dtm = grid_canopy(raw, res = 0.5, subcircle = 0.2, na.fill = "knnidw", k = 10, p= 10)


groundReturns@crs<-raster::crs(proj4)  
mapview::mapview(dtm)

# Normalize the dataset
#lasnormalize(las = las, method = "kriging", k = 10L, model = gstat::vgm(0.59, "Sph", 874))
lasnormalize(las = las, method = "kriging",  k = 10L)

#chm1<-grid_canopy(las, res = 0.5, subcircle = 0.2, na.fill = "kriging", k = 10L, model = gstat::vgm(0.59, "Sph", 874))
chm2<-grid_canopy(las, res = 0.5, subcircle = 0.2, na.fill = "knnidw", k = 3, p = 2)
chm2 = as.raster(chm2)
raster::plot(chm2)
# smoothing post-process (e.g. 2x mean)
kernel = matrix(1,3,3)
chm2 = raster::focal(chm2, w = kernel, fun = mean)
chm2 = raster::focal(chm2, w = kernel, fun = mean)}
raster::plot(chm2, col = height.colors(50)) # check the image


####  rapidlasso approach
#chm = grid_tincanopy(las, 0.5, c(0,2,5,10,15), c(0,1) , subcircle = 0.2)
#chm = as.raster(chm)
#raster::projection(chm)<- raster::crs(proj4)}



la_orig<-las
#### TREE segmentation
crowns_watershed = lastrees(las, "watershed", chm2, th = 3, extra = TRUE)
contour_watershed  = raster::rasterToPolygons(crowns_watershed , dissolve = TRUE)
raster::plot(chm2, col = height.colors(50))
raster::plot(contour_watershed , add = T)

# segemtation li2012
las<-la_orig
lidR::lastrees(las, "li2012", R=10, th_tree = 3)
crowns_li2012<-lidR::grid_metrics(las, unique(treeID), gridSize)
crowns_li2012<-as.raster(crowns_li2012)
contour_li  = raster::rasterToPolygons(crowns_li2012 , dissolve = TRUE)
raster::plot(chm2, col = height.colors(50))
raster::plot(contour_li , add = T)

# dalponte
las<-la_orig
ttops = lidR::tree_detection(chm, ws= 7, hmin=3)
lastrees_dalponte(las, chm2, ttops)
crowns_dalponte<-lidR::grid_metrics(las, unique(treeID), gridSize)
crowns_dalponte<-as.raster(crowns_dalponte)
contour_dalponte  = raster::rasterToPolygons(crowns_dalponte , dissolve = TRUE)
raster::plot(chm2, col = height.colors(50))
raster::plot(contour_dalponte , add = T)

# silva
las<-la_orig
crowns2_silva2016 = lastrees(las, "silva2016", chm2, exclusion = 0.2, treetops=ttops)
crowns_silva<-lidR::grid_metrics(las, unique(treeID), gridSize)
crowns_silva<-as.raster(crowns_silva)
contour_silva  = raster::rasterToPolygons(crowns_li2012 , dissolve = TRUE)
raster::plot(chm2, col = height.colors(50))
raster::plot(contour_silva , add = T)




# https://github.com/Jean-Romain/lidR/wiki/Rasterizing-perfect-canopy-height-models
library(lidR)
source("http://bioconductor.org/biocLite.R")
biocLite("EBImage")
library(EBImage)
# read data
lasfile<-path.expand("~/proj/uav/thesis/finn/output/477375_00_5631900_00_477475_00_5632000_00.las")
las = readLAS(lasfile)
# Classify ground points see example # https://github.com/Jean-Romain/lidR/wiki/Rasterizing-perfect-canopy-height-models
ws = seq(3,21, 3)
th = seq(0.1, 2, length.out = length(ws))
lasground(las, "pmf", ws, th)
dtm2<-grid_terrain(las, method = "kriging", k = 10L)
dtm2 = as.raster(dtm2)
raster::plot(dtm2)
writeRaster(dtm2,paste0(path_output,"dtm2.tif"),overwrite =T)
groundReturns = las %>% lasfilterground
dtm = grid_canopy(groundReturns, res = 0.5, subcircle = 0.2, na.fill = "knnidw", k = 10, p= 10)
dtm = as.raster(dtm)
raster::plot(dtm)
plot(groundReturns)
# Normalize the dataset
# Here no  DTM is used but exact interpolation of each point
lasnormalize(las = las, 
             method = "knnidw",
             k = 10L)

# make it the lidR tutorial
chm1 = grid_canopy(las_clip, res = 0.5, subcircle = 0.2, na.fill = "knnidw", k = 10, p= 10)
chm1 = as.raster(chm1)
# smoothing post-process (e.g. 2x mean)
kernel = matrix(1,3,3)
chm1 = raster::focal(chm1, w = kernel, fun = mean)
chm1 = raster::focal(chm1, w = kernel, fun = mean)
raster::plot(chm1, col = height.colors(50)) # check the image

####  rapidlasso approach
chm2 = grid_tincanopy(las_clip, 0.5, c(0,2,5,10,15), c(0,1) , subcircle = 0.2)
chm2 = as.raster(chm2)
raster::projection(chm2)<- raster::crs(proj4)
plot(chm2)
# show diffrence
plot(chm2-chm1)


#### TREE segmentation

# lidR way
crowns1 = lastrees(las_clip, "watershed", chm1, th = 4, extra = TRUE)
# get contours as a SpatialPolygonsDataFrame
# (take a while to run)
contour1 = raster::rasterToPolygons(crowns1, dissolve = TRUE)
plot(chm1, col = height.colors(50))
plot(contour1, add = T)

# rapidlasso 
crowns2 = lastrees(las_clip, "watershed", chm2, th = 4, extra = TRUE)
# get contours as a SpatialPolygonsDataFrame
# (take a while to run)
contour2 = raster::rasterToPolygons(crowns2, dissolve = TRUE)
plot(chm2, col = height.colors(50))
plot(contour2, add = T)

plot(crowns1-crowns2)
