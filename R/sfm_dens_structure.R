#' Identify dens structures in a pointcloud 
#'
#' @description Density clusters in a 3d sfm pointcloud.
#' Filters a pointcloud based on local point densities,
#' decimates the pointcloud and performs a dbscan density-based clustering.
#' 
#' 
#' @param sfm The pointcloud as a lidR las object
#' @param pd_eps Search range for point density calculation (see dbscan::pointdensity)
#' @param point_thresh Threshold of point in pd_eps for keeping the point
#' @param db_eps Search range for the clustering (see dbscan::dbscan)
#' @param db_minPts Minimum pointa in db_eps for the clustering (see dbscan::dbscan)
#'  
#' @return A list contaning: 1. the decimated pointcloud with an additional column indication the cluster of the points
#' and 2. a data.frame with the centers of the cluster
#' 
#' @author Marvin Ludwig
#' 
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
#' # cluster stats
#' structure_stats(pc_clust$cluster)
#' }
#' 
#' 
#' 
sfm_dens_structure <- function(sfm, point_thresh = 1200, pd_eps = 1, db_eps = 0.3, db_minPts = 8){
  library(dbscan)
  library(lidR)
  
  sfm@data$density <- dbscan::pointdensity(x = sfm@data[,1:3], eps = pd_eps, type = "frequency")
  sfm <- lasfilter(sfm, density > point_thresh)
  
  sfm <- lidR::lasfilterdecimate(sfm, density = 100, res = 1, homogenize = FALSE)
  sfm_db <- dbscan(sfm@data[,1:3], eps = db_eps, minPts = db_minPts)
  
  # cluster centers
  df <- sfm@data[,1:3]
  df$cluster <- sfm_db$cluster
  sfm@data$cluster <- sfm_db$cluster
  centers <- data.frame(X = aggregate(df$X, by = list(df$cluster), FUN = "mean")[,2],
                        Y = aggregate(df$Y, by = list(df$cluster), FUN = "mean")[,2],
                        Z = aggregate(df$Z, by = list(df$cluster), FUN = "mean")[,2],
                        rel_points = aggregate(df$X, by = list(df$cluster), FUN = "length")[,2])
  centers$rel_points <- centers$rel_points/sum(centers$rel_points)
  
  return(list(clustered_cloud = sfm, 
              cluster = centers))
}
