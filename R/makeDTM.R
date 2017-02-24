
if (!isGeneric('pc2DTM')) {
  setGeneric('pc2DTM', function(x, ...)
    standardGeneric('pc2DTM'))
}

#'@name pc2DTM
#'@title Create a Digital Terrain Model from a UAV generated point cloud 
#'
#'@description
#' Create a Digital Terrain Model from a high density point cloud as typically derived by an optical UAV retrieval.  
#'
#'@author Chris Reudenbach
#'
#'@param lasDir  default is \code{NULL} path  to the laz/las file(s)
#'@param gisdbase_path default is \code{NULL} root directory of the project. NOTE the function creates two subfolder named \code{run} and \code{output}
#'@param grid_size  resolution of the DTM raster
#'@param path_lastools directory for the windows lastools
#'@param level_max number ob spline iterations
#'@param thin_with_grid default 0.5 meter. Grid stepsize for data thinning 
#'@param keep_class default is 2. Default ground class of las/laz conform data 
#'@param bulge  default is 1.5. 'A parameter to filter spikes it is set to a step_size/10 and then clamped into the range from 1.0 to 2.0
#'@param level_max default is 3, Maximum number of spline iterations highly suggested to take odd numbers. As higher as more detailed (spurious).
#'@param step_size  default is 25 meter. LAStools key words if \code{city},\code{town},\code{metro},\code{nature},\code{wilderness} or experiment with free values
#'@param sub_size = "8", default is 8 meter. LAStools key words if \code{extra_coarse},\code{coarse},\code{fine},\code{extra_fine},\code{ultra_fine},\code{hyper_fine} or experiment with free values
#'@param cores number of cores that will be used
#'@param proj4  default is EPSG 32632 any valid proj4 string that is assumingly the correct one



#'@return pc2DTM basically returns a DEM and DSM
#'
#'
#'@export pc2DTM
#'
#'@examples 
#'\dontrun{
#' pc2DTM(lasDir =  "~/path/to/lasdata",
#'        gisdbase_path = "~/temp5",
#'        thin_with_grid = "0.5",
#'        level_max = "5" ,
#'        grid_size = "0.5")
#'}

pc2DTM <- function(lasDir = NULL,
                   gisdbase_path = NULL,
                   thin_with_grid = "0.5",
                   keep_class = "2",
                   bulge = "1.5",
                   level_max = "9" ,
                   step_size = "city",
                   sub_size = "ultra_fine",
                   grid_size = "0.5", 
                   proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs",
                   path_lastools = NULL,
                   cores = "3") {
  
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
  las2las       <- paste(cmd,"las2las-cli.exe",sep = "/")
  lasmerge      <- paste(cmd,"lasmerge-cli.exe",sep = "/")
  lasground_new <- paste(cmd,"lasground_new-cli.exe",sep = "/")
  las2dem       <- paste(cmd,"las2dem-cli.exe",sep = "/")
  las2txt       <- paste(cmd,"las2txt-cli.exe",sep = "/")
  
  # check las / laz files laz will be preferred
  lasFileNames <- list.files(pattern = "[.]las$", path = lasDir, full.names = TRUE)
  lazFileNames <- list.files(pattern = "[.]laz$", path = lasDir, full.names = TRUE)
  if (length(lazFileNames) > 0 ) extFN <- substr(extension(basename(lazFileNames[1])),2,4)
  else if (length(lasFileNames) > 0) extFN <- substr(extension(basename(lasFileNames[1])),2,4)
  else stop("no valid las or laz files found...\n")
  
  #sp_param <- uavRst:::getSpatialLASInfo(lasDir)
  
  # map the code words
  if (step_size == "city") step <- "25"
  else if (step_size == "town") step <- "10"
  else if (step_size == "metro") step <- "50"
  else if (step_size == "nature") step <- "5"
  else if (step_size == "wilderness") step <- "3"
  if (sub_size == "extra_coarse") sub <- "3"
  else if (sub_size == "coarse") sub <- "4"
  else if (sub_size == "fine") sub <- "6"
  else if (sub_size == "extra_fine") sub <- "7"
  else if (sub_size == "ultra_fine") sub <- "8"
  else if (sub_size == "hyper_fine") sub <- "9"
  
  # create project structure and export global pathes
  link2GI::initProj(projRootDir = gisdbase_path, 
                    projFolders =  c("output/","run/"))
  
  # set lastool folder
  path_lastools <- path.expand(path_lastools)
  
  unlink(paste0(path_run,"*"), force = TRUE)
  
  setwd(path_run)
  
  
  ### reduce data amount
  cat("reducing the point density...\n")
  ret <- system(paste0(las2las,
                       " -i ",path_lasdata,"/*.",extFN,
                       " -odix _red2 ",
                       " -odir ",path_run,
                       " -o",extFN,
                       " -keep_class ",keep_class,
                       " -thin_with_grid ",thin_with_grid), 
                intern = TRUE, 
                ignore.stderr = TRUE
  )
  
  # merge all files
  cat("merge point cloud files ...\n")
  ret <- system(paste0(lasmerge,
                       " -i ",path_run,"/export*_red2.",extFN,
                       " -o ",path_run,"out.",extFN),
                intern = TRUE, 
                ignore.stderr = TRUE
  )
  
  #### starting lastools classification 
  # run lasground 
  cat("classify ground points (LAStools) ...\n")
  ret <- system(paste0(lasground_new,
                       " -i ",path_run,"out.",extFN,
                       " -all_returns ",
                       " -bulge ", bulge,
                       " -skip_files",
                       " -step ", step,
                       " -sub ", sub,
                       " -odix g -o",extFN,
                       " -cores ",cores),
                intern = FALSE,
                ignore.stderr = TRUE
  )
  
  # create lastools  DTM
  ret <- system(paste0(las2dem,
                       " -i ",path_run,"*g.",extFN,
                       " -keep_class 2",
                       " -extra_pass",
                       " -step ",grid_size,
                       " -ocut 3 ",
                       " -odix _dtm ",
                       " -otif ",
                       " -odir ",path_output,
                       " -cores ",cores),
                intern = TRUE, 
                ignore.stderr = TRUE
  )
  
  
  # export las file to text file
  ret <- system(paste0(las2txt,
                       " -i ",path_run,"*g.",extFN,
                       " -parse xyzrRGB",
                       " -sep komma"),
                intern = TRUE, 
                ignore.stderr = TRUE
  )
  
  #### starting SAGA classification
  # create output mask file for interpolation
  cat("classify ground points (SAGA) ...\n")
  r <- raster::raster(paste0(path_output,"o_dtm.tif"))
  r[r > 0] <- 0
  raster::writeRaster(r,filename = paste0(path_run,"rawdtm.tif"),overwrite = TRUE)
  gdalUtils::gdalwarp(paste0(path_run,"rawdtm.tif"), 
                      paste0(path_run,"rawdtm.sdat"), 
                      overwrite = TRUE,  
                      of = 'SAGA',
                      verbose = FALSE) 
  
  
  # import to saga as point cloud
  ret <- system(paste0(sagaCmd,' io_shapes 16 ',
                       ' -POINTS ', path_run,'pointcloud',
                       ' -FILE  ', path_run,'outg.txt',
                       ' -XFIELD 1',
                       ' -YFIELD 2',
                       ' -FIELDS "4;5;6;7"',
                       ' -FIELDNAMES "r;R;G;B"',
                       ' -FIELDTYPES "1;1;1;1"',
                       ' -SKIP_HEADER 0',
                       ' -FIELDSEP 2'),
                intern = TRUE, 
                ignore.stderr = TRUE)
  
  # saga spline interpolation of the alt values
  ret <- system(paste0(sagaCmd,' grid_spline 4 ',
                       ' -SHAPES ', path_run,'pointcloud.spc',
                       ' -FIELD 2',
                       ' -TARGET_DEFINITION 0',
                       ' -TARGET_OUT_GRID ',paste0(path_run,"rawdtm.sgrd"),
                       ' -TARGET_USER_SIZE ',grid_size,
                       ' -METHOD 1',
                       ' -EPSILON 0.000100',
                       ' -LEVEL_MAX ',level_max),
                intern = TRUE, 
                ignore.stderr = TRUE)
  
  dtm <- gdalUtils::gdalwarp(paste0(path_run,"rawdtm.sdat"), 
                             paste0(path_output,"dtm.tif"), 
                             t_srs = proj4,
                             output_Raster = TRUE,
                             overwrite = TRUE,  
                             verbose = FALSE) 
  return(dtm)
}