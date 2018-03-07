if (!isGeneric('local_relief')) {
  setGeneric('local_relief', function(x, ...)
    standardGeneric('local_relief'))
}

#'@name local_relief
#'@title Create a hillshading model from LiDAR/UAV generated point cloud
#'
#'@description
#' Create a hillshading model on base of LiDAR/UAV generated point cloud.
#'
#'@author Chris Reudenbach
#'
#'@param lasDir  character. default is \code{NULL} path to the laz/las file(s)
#'@param path_lastools character. character folder containing the Windows binary files of the lastools
#'@param gisdbase_path character. gisdbase will be linked or created depending on \code{gisdbase_exist}
#'@param GRASSlocation character. location will be linked or created depending on \code{gisdbase_exist}
#'@param projFolder character. subfolders in gisdbase for R related processing
#'@param grid_size numeric. resolution for raster operations
#'@param spline_level_max numeric. default is 9 number ob spline iterations
#'@param proj4  character. default is EPSG 32632 any valid proj4 string that is assumingly the correct one
#'@param gisdbase_exist logical. switch if gisdbase is created or  linked only
#'@param param_list  character. default is c("i","v", "n", "g","f","overwrite","quiet")
#'                   i Save intermediate maps; \cr
#'                   v Use bspline interpolation to construct the surface.Uses v.surf.bspline cubic interpolation instead of r.fillnulls cubic interpolation\cr
#'                   n Invert colors in the color table \cr
#'                   g Logarithmic scaling of the color table\cr
#'                   f Do not perform histogram equalization on the color table

#'
#'
#'@export local_relief
#'@examples
#'\dontrun{
#' # create a hillshade based on a las/laz point clouds
#'   hs <- uavRst::local_relief(lasDir =  "~/proj/Monte_Bernorio/las/",
#'                              gisdbase_path = "~/temp55/GRASS7",
#'                              grid_size = "1.0")
#'}
#'

local_relief <- function(lasDir = NULL,
                            gisdbase_path = NULL,
                            GRASSlocation = "tmp/",
                            projFolder = c("data/","output/","run/","las/"),
                            grid_size = "1.0",
                            param_list = c("i","v", "n", "g","f","overwrite","quiet"),
                            spline_level_max = "9" ,
                            proj4 = "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                            gisdbase_exist = FALSE,
                            path_lastools = NULL) {

  # some basic checks
  if (is.null(lasDir)) stop("no directory containing las/laz files provided...\n")
  lasDir <- path.expand(lasDir)

  if (is.null(path_lastools)) {
    if (Sys.info()["sysname"] == "Windows") {
      cmd <- path_lastools <- paste(system.file(package = "uavRst"), "lastools", sep = "/")
    } else {
      cmd <- paste("wine ",path_lastools <- paste(system.file(package = "uavRst"), "lastools", sep = "/"))
    }
  }

  # create cmd strings
  lasmerge      <- paste(cmd,"lasmerge-cli.exe",sep = "/")
  lasinfo       <- paste(cmd,"/lasinfo-cli.exe",sep = "/")

  # check las / laz files; laz will be preferred
  lasFileNames <- list.files(pattern = "[.]las$", path = lasDir, full.names = TRUE)
  lazFileNames <- list.files(pattern = "[.]laz$", path = lasDir, full.names = TRUE)
  if (length(lazFileNames) > 0 ) {
    extFN <- substr(extension(basename(lazFileNames[1])),2,4)
    noF <- length(lazFileNames)
  }
  else if (length(lasFileNames) > 0) {
    extFN <- substr(extension(basename(lasFileNames[1])),2,4)
    noF <- length(lazFileNames)
  }
  else stop("no valid las or laz files found...\n")

  # set lastool folder
  path_lastools <- path.expand(path_lastools)

  # create project structure and export global pathes
  link2GI::initProj(projRootDir = gisdbase_path,
                    projFolders =  c("output/","run/"))

  # delete content in run directory
  unlink(paste0(path_run,"*"), force = TRUE)

  setwd(path_run)

  # merge all files
  cat("NOTE: You are dealing with a huge UAV generated point cloud data set.\n      so take time and keep relaxed... :-)\n")
  cat(":: merge and decompress ",noF," point cloud files...\n")
  ret <- system(paste0(lasmerge,
                       " -i ",lasDir,"/*.",extFN,
                       " -olas",
                       " -o ",path_run,"full_point_cloud.las"),
                intern = TRUE,
                ignore.stderr = TRUE
  )
  # get extent of merged file
  #sp_param <- getSpatialLASInfo(lasinfo,paste0(path_run,"full_point_cloud.las"))
  sp_param <-lastool(lasFile= paste0(path_run,"full_point_cloud.las"))
  # add proj4 string manually
  sp_param[5] <- proj4

  # create and export globally project folder structure
  link2GI::initProj(projRootDir = gisdbase_path,
                    GRASSlocation = GRASSlocation,
                    projFolders =  projFolder)

  # create GRASS7 connection according gisdbase_exist (permanent or temporary)
  if (gisdbase_exist)
    link2GI::linkGRASS7(gisdbase = gisdbase_path, location = GRASSlocation, gisdbase_exist = TRUE)
  else
    link2GI::linkGRASS7(gisdbase = gisdbase_path, location = GRASSlocation, spatial_params = sp_param,resolution = grid_size)

  # TOFIX  (latest?) late raw DSM using r.in.lidar
  cat(":: calculate DSM...\n")
  ret <- rgrass7::execGRASS("r.in.lidar",
                            flags  = c("overwrite","quiet","o"),
                            input  = paste0(path_run,"full_point_cloud.las"),
                            output = "point_cloud_dsm",
                            method = "max",
                            intern = TRUE,
                            ignore.stderr = TRUE
  )



  cat(":: calculate local relief ...\n")
  ret <- rgrass7::execGRASS("r.local.relief",
                            flags  = param_list,
                            input  = "point_cloud_dsm",
                            output = "hillshade",
                            shaded_output = "shaded_hillshade",
                            intern = TRUE,
                            ignore.stderr = TRUE
  )





  cat(":: convert raw DSM  geotiff \n")
  # write GRASS to TIF
  raster::writeRaster(raster::raster(rgrass7::readRAST(paste0("hillshade"))),paste0(path_output,"hillshade"), overwrite=TRUE,format="GTiff")
  #hillshade <- h_grass2tif(runDir = path_output, layer = "hillshade",returnRaster = TRUE)
  hillshade<-raster::raster(rgrass7::readRAST(paste0("hillshade")))
  return(hillshade)
}
