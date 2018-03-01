# https://github.com/Jean-Romain/lidR/wiki/Rasterizing-perfect-canopy-height-models
library(lidR)
lasfile<-path.expand("~/proj/uav/thesis/finn/data/lidar/merged_477000_00_5631000_00_477999_99_5632999_99.las")
las = readLAS(lasfile)
extent <- c(477393.,477460. ,5631938. , 5632003.)
las_clip<-lasclipRectangle(las, extent[1], extent[3], extent[2], extent[4])



# Classify ground points see example
ws = seq(3,21, 3)
th = seq(0.1, 2, length.out = length(ws))
lasground(las_clip, "pmf", ws, th)

# DTM
dtm = grid_terrain(las_clip, method = "knnidw", k = 10L)

# Normalize the dataset
# Subtract the DTM to obtain ground points at 0. 
# Here no  DTM is used but exact interpolation of each point
lasnormalize(las = las_clip, 
                      method = "knnidw",
                      k = 10L)

# same with DTM
# lasnormalize(las = las_clip,
#                      dtm = dtm, 
#                      method = "knnidw",)

# make it the lidR tutorial
chm1 = grid_canopy(las_clip, res = 0.5, subcircle = 0.2, na.fill = "knnidw", k = 10, p= 10)
chm1 = as.raster(chm1)
# smoothing post-process (e.g. 2x mean)
kernel = matrix(1,3,3)
chm1 = raster::focal(chm1, w = kernel, fun = mean)
chm1 = raster::focal(chm1, w = kernel, fun = mean)
raster::plot(chmlidR, col = height.colors(50)) # check the image

# rapidlasso approach
chm2 = grid_tincanopy(las_clip, 0.5, c(0,2,5,10,15), c(0,1) , subcircle = 0.2)
chm2 = as.raster(chm2)
raster::projection(chm2)<- raster::crs(proj4)
plot(chm2)
# show diffrence
plot(chm2-chm1)


#### TREE segmentation
crowns = lastrees(las_clip, "watershed", chm1, th = 4, extra = TRUE)
# get contours as a SpatialPolygonsDataFrame
# (take a while to run)
contour = rasterToPolygons(crowns, dissolve = TRUE)

plot(chm, col = height.colors(50))
plot(contour, add = T)
