#' 2D plot of pointclouds
#'
#' @description Creates a 2d plot of a 3d pointcloud along a line (canvas).
#' 
#' @param sfm The pointcloud as a lidR las object
#' @param canvas Two SpatialPoint or one SpatialLine as the projection line.
#' @param cluster data.frame containing the clusters from sfm_dens_structure
#' 
#' @return ggplot
#' 
#' 
#' @author Marvin Ludwig
#' 
#' @examples 
#' \dontrun{
#' # requires packages
#' require(dbscan)
#' require(lidR)
#' 
#' 
#' # get the pointcloud data (3.8 MB)
#' url <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/tree_cloud.las"
#' res <- curl::curl_download(url, paste0(getwd(), "tree_cloud.las"))
#'
#' pc <- readLAS(paste0(getwd(), "tree_cloud.las"))
#' # reproject to UTM
#' pc <- las_projection(pc)
#' 
#' # identify dense structures
#' pc_clust <- sfm_dens_structure(pc)
#' 
#' # create canvas
#' canvas_points <- data.frame(id = c("1,2"), x = c(477636.2, 477623.9), y = c(5632836, 5632891))
#' coordinates(canvas_points) <- ~ x + y
#' projection(canvas_points) <- "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "
#'
#' # plot without canvas uses x-axis
#' sfm_canvas(pc, cluster = pc_clust$cluster)
#' 
#' # plot with canvas
#' sfm_canvas(pc, canvas_points, pc_clust$cluster)
#' 
#' }
#' 
#' 

sfm_plot2d <- function(sfm, canvas = NULL, cluster = NULL){
  library(ggplot2)
  library(viridis)
  library(cowplot)
  
  # # # CREATE CANVAS # # #
  # from line or polygon
  if(is(canvas, "SpatialLines")){
    canvas <- canvas@lines[[1]]@Lines[[1]]@coords
  }else if(is(canvas, "SpatialPoints")){
    canvas <- canvas@coords[1:2,]
  }else{
    print("No canvas. Using X-Coordinates instead.")
  }
  
  # # # POINT ROTATION # # #
  # x and y coordinates as Matrix
  if(is.null(cluster)){
    M <- as.matrix(sfm@data[,1:2])
  }else{
    M <- as.matrix(rbind(sfm@data[,1:2], cluster[,1:2]))
  }
  
  # calculate rotation angle
  if(is.null(canvas)){
    alpha <- 0
  }else{
    alpha <- pi - atan((canvas[1,2]-canvas[2,2])/(canvas[1,1]-canvas[2,1]))
  }
  # create rotation matrix
  rotm <- matrix(c(cos(alpha),sin(alpha),-sin(alpha),cos(alpha)),ncol=2)
  # rotate x and y
  Mrot <- t(rotm %*% (t(M)-c(M[1,1],M[1,2]))+c(M[1,1],M[1,2]))
  
  sfm@data$X_canvas <- Mrot[1:nrow(sfm@data),1]
  sfm@data$Y_canvas <- Mrot[1:nrow(sfm@data),2]
  
  
  # # # PLOT # # #
  canvas_plot <- ggplot(sfm@data, aes(x = X_canvas, y = Z))+
    stat_binhex(bins=400)+
    scale_x_continuous(name = element_blank(), labels = NULL)+
    scale_y_continuous(name = "Height [m]")+
    scale_fill_gradientn(name = "Point count", trans = "log",
                         breaks = 10^(0:4), colors=viridis(10))
  # # # ADD CLUSTER # # #
  if(!is.null(cluster)){
    cluster$X_canvas <- Mrot[(nrow(Mrot)-nrow(cluster)+1):nrow(Mrot),1]
    cluster$Y_canvas <- Mrot[(nrow(Mrot)-nrow(cluster)+1):nrow(Mrot),2]
    
    canvas_plot <- canvas_plot + geom_point(data = cluster,
                                            aes(x = X_canvas, y = Z, size = rel_points),
                                            color = "orange", alpha = 0.75) + labs(size = "Rel. Points")
  }
  return(canvas_plot)
}


