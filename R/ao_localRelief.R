if (!isGeneric('ao_pc2Hillshade')) {
  setGeneric('ao_pc2Hillshade', function(x, ...)
    standardGeneric('ao_pc2Hillshade'))
}

#'@name ao_pc2Hillshade
#'@title Create a hillshading model l from a UAV generated point cloud 
#'
#'@description
#' Create a hillshading model l from a UAV generated point cloud. return ao_pc2Hillshade basically returns a  DSM
#'
#'@author Chris Reudenbach
#'
#'@param lasDir  default is \code{NULL} path  to the laz/las file(s)
#'@param path_lastools directory for the windows lastools
#'@param gisdbase_path gisdbase will be linked or created depending on \code{gisdbase_exist}
#'@param GRASSlocation location will be linked or created depending on \code{gisdbase_exist}
#'@param projFolder subfolders in gisdbase for R related processing 
#'@param grid_size resolution for raster operations 
#'@param spline_level_max default is 9 number ob spline iterations
#'@param proj4  default is EPSG 32632 any valid proj4 string that is assumingly the correct one
#'@param gisdbase_exist switch if gisdbase is created or  linked only
#'@param param_list  default is c("i","v", "n", "g","f","overwrite","quiet")
#'                   i Save intermediate maps; \cr
#'                   v Use bspline interpolation to construct the surface Uses v.surf.bspline cubic interpolation instead of r.fillnulls cubic interpolation\cr 
#'                   n Invert colors in the color table \cr
#'                   g Logarithmic scaling of the color table\cr
#'                   f Do not perform histogram equalization on the color table

#'
#'
#'@export ao_pc2Hillshade
#'@examples
#'\dontrun{
#' # create a hillshade based on a las/laz point clouds 
#'   hs <- uavRst::ao_pc2Hillshade(lasDir =  "~/proj/Monte_Bernorio/las/",
#'                              gisdbase_path = "~/temp55/GRASS7",
#'                              grid_size = "1.0")
#'}
#'

ao_pc2Hillshade <- function(lasDir = NULL,
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
  sp_param <- getSpatialLASInfo(lasinfo,paste0(path_run,"full_point_cloud.las"))
  
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
