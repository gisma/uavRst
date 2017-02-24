if (!isGeneric('pc2DSM')) {
  setGeneric('pc2DSM', function(x, ...)
    standardGeneric('pc2DSM'))
}

#'@name pc2DSM
#'@title Create a Digital Surface Model from a UAV generated point cloud 
#'
#'@description
#' Create a Digital Surface Model from a UAV generated point cloud 
#'
#'@author Chris Reudenbach
#'
#'@param lasDir  default is \code{NULL} path  to the laz/las file(s)
#'@param gisdbase_path gisdbase will be linked or created depending on \code{gisdbase_exist}
#'@param GRASSlocation location will be linked or created depending on \code{gisdbase_exist}
#'@param projFolder subfolders in gisdbase for R related processing 
#'@param grid_size resolution for raster operations 
#'@param type_smooth  default is \code{otb_gauss} alternatives is \code{saga_spline}
#'@param saga_spline_level_max default is 9 number ob spline iterations
#'@param otb_gauss_radius radius of otb smoothing filter
#'@param proj4  default is EPSG 32632 any valid proj4 string that is assumingly the correct one
#'@param gisdbase_exist switch if gisdbase is created or  linked only


#'@return pc2DSM basically returns a  DSM
#'
#'
#'@export pc2DSM
#'@examples
#'\dontrun{
#' # create a DSM based on a uav point cloud 
#'  pc2DSM(lasDir =  lasDir,
#'         gisdbase_path = "~/temp6/GRASS7",
#'         GRASSlocation = "tmp/",
#'         projFolder = c("data/","output/","run/","las/"),
#'         grid_size = "0.05",
#'         gisdbase_exist = FALSE)
#'}
#'

pc2DSM <- function(lasDir = NULL,
                   gisdbase_path = NULL,
                   GRASSlocation = "tmp/",
                   projFolder = c("data/","output/","run/","las/"),
                   grid_size = "0.5", 
                   saga_spline_level_max = "9" ,
                   otb_gauss_radius = "2",
                   type_smooth = "otb_gauss",
                   proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs",
                   gisdbase_exist = FALSE,
                   path_lastools = NULL) {
  
  # some basic checks 
  if (is.null(lasDir)) stop("no directory containing las/laz files provided...\n")
  lasDir <- path.expand(lasDir)
  
  if (is.null(path_lastools)) {
    if (Sys.info()["sysname"] == "Windows") {
      cmd <- path_lastools <- paste(system.file(package = "uavRst"), "LAStools", sep = "/")
    } else {
      cmd <- paste("wine ",path_lastools <- paste(system.file(package = "uavRst"), "LAStools", sep = "/"))
    }
  }
  
  # create cmd strings
  lasmerge      <- paste(cmd,"lasmerge-cli.exe",sep = "/")
  lasinfo       <- paste(cmd,"/lasinfo-cli.exe",sep = "/")
  
  # check las / laz files laz will be preferred
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
  sp_param <- uavRst:::getSpatialLASInfo(lasinfo,paste0(path_run,"full_point_cloud.las"))
  
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
  
  # late raw DSM using r.in.lidar
  cat(":: calculate DSM...\n")
  ret <- rgrass7::execGRASS("r.in.lidar",
                            flags  = c("overwrite","quiet","o"),
                            input  = paste0(path_run,"full_point_cloud.las"),
                            output = "point_cloud_dsm",
                            method = "max",
                            intern = TRUE,
                            ignore.stderr = TRUE
  )
  
  
  
  cat(":: convert raw DSM  geotiff \n")
  uavRst:::G2Tiff(runDir = path_output, layer = "point_cloud_dsm")
  
  gdal <- link2GI::linkgdalUtils()
  cat("\n:: preliminary fill of gaps... \n")
  ret <- system(paste0("gdal_fillnodata.py ",
                       path_output,"point_cloud_dsm.tif ",
                       path_output,"filled_point_cloud_dsm.tif"),intern = TRUE)
  
  gdalUtils::gdalwarp(paste0(path_output,"filled_point_cloud_dsm.tif"), 
                      paste0(path_run,"filled_point_cloud_dsm.sdat"), 
                      overwrite = TRUE,  
                      of = 'SAGA',
                      verbose = FALSE) 
  
  # otb gaussian smooth
  if (type_smooth == "otb_gauss") {
    link2GI::linkOTB()
    module  <- "otbcli_Smoothing"
    command <- paste0(path_OTB, module)
    command <- paste0(command, " -in ",path_output,"filled_point_cloud_dsm.tif")
    command <- paste0(command, " -out ", path_output, "/dsm.tif")
    command <- paste0(command, " -ram 4096")
    command <- paste0(command, " -type gaussian")
    command <- paste0(command, " -type.gaussian.radius ",otb_gauss_radius)
    ret <- system(command, intern = TRUE)  
    dsm <- raster::raster(paste0(path_output, "/dsm.tif"))
  }
  if (type_smooth == "saga_spline") {
    # saga spline interpolation of the alt values
    cat(":: iterative spline smooth of dsm... \n")
    ret <- system(paste0(sagaCmd,' grid_spline 5 ',
                         ' -GRID ', path_run,'filled_point_cloud_dsm.sgrd',
                         ' -TARGET_DEFINITION 0',
                         ' -TARGET_OUT_GRID ',path_run,"spline_filled_point_cloud_dsm.sgrd",
                         ' -TARGET_USER_SIZE ',grid_size,
                         ' -METHOD 1',
                         ' -EPSILON 0.0001',
                         ' -LEVEL_MAX ',saga_spline_level_max),
                  intern = TRUE, 
                  ignore.stderr = TRUE)
    
    dsm <- gdalUtils::gdalwarp(paste0(path_run,"spline_filled_point_cloud_dsm.sdat"), 
                               paste0(path_output,"dsm.tif"), 
                               t_srs = proj4,
                               output_Raster = TRUE,
                               overwrite = TRUE,  
                               verbose = FALSE) 
  }
  return(dsm)  
}
