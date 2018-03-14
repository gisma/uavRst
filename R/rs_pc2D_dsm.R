
#' Create a Digital Terrain Model from UAV generated point clouds by minimum altitude sampling
#'
#'@description
#' Create a Digital Surface Model from a high density point cloud as typically derived by an
#' optical UAV retrieval. It simply samples the maximum or whatever values of a given target 
#' grid size and fills the no data holes if so.
#'
#'@author Chris Reudenbach
#'
#'@param laspcFile  character. default is \code{NULL} path  to the laz/las file(s)
#'@param gisdbasePath character. default is \code{NULL} root directory of the project. NOTE the function creates two subfolder named \code{run} and \code{output}
#'@param targetGridSize numeric. the resolution of the target DTM raster
#'@param sampleMethod sampling method for point aggregation
#'@param threshold  numeric. percentile threshold
#'@param proj4  character. valid proj4 string that should be assumingly the correct one
#'@param giLinks list of link2GI cli pathes, default is NULL
#'@param projFolder subfolders that will be created/linked for R related GRASS processing
#'@param verbose to be quiet (1)
#'@param cutExtent clip area
#'
#'@importFrom lidR tree_detection
#'@importFrom lidR writeLAS
#'@importFrom lidR readLAS
#'@importFrom lidR lasclipRectangle
#'@importFrom rlas readlasheader
#'@export pc2D_dsm

#'@examples
#'\dontrun{
#'
#' require(uavRst)
#' require(raster)
#' require(link2GI)
#' 
#' # proj subfolders
#' projRootDir<-getwd()
#' #setwd(paste0(projRootDir,"run"))
#' 
#' paths<-link2GI::initProj(projRootDir = projRootDir,
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")
#' 
#' # get some colors
#' pal = mapview::mapviewPalette("mapviewTopoColors")
#' 
#' # get the data
#' url <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/lidar.las"
#' res <- curl::curl_download(url, "run/lasdata.las")
#' # make the folders and linkages
#' giLinks<-uavRst::get_gi()
#' 
#' # create 2D pointcloud DSM
#' dsm <- pc2D_dsm(laspcFile = paste0(path_run,"lasdata.las"),
#'                 gisdbasePath = projRootDir,
#'                 sampleMethod = "max",
#'                 targetGridSize = 0.5, 
#'                 giLinks = giLinks)
#'}

pc2D_dsm <- function(laspcFile = NULL,
                    gisdbasePath = NULL,
                    sampleMethod = "max",
                    threshold = 20 ,
                    cutExtent = NULL,
                    targetGridSize = 0.25,
                    projFolder = NULL,
                    proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs",
                    giLinks =NULL,
                    verbose = FALSE) {
  if (is.null(giLinks)){
    giLinks <- get_gi()
  }
  gdal <- giLinks$gdal
  saga <- giLinks$saga
  otb <- giLinks$otb
  sagaCmd<-saga$sagaCmd
  path_OTB <- otb$pathOTB
  
  if (!verbose){
    GV <- Sys.getenv("GRASS_VERBOSE")
    Sys.setenv("GRASS_VERBOSE"=0)
    ois <- get.ignore.stderrOption()
    set.ignore.stderrOption(TRUE)}
  
  if (is.null(projFolder)) projFolder <-  c("data/","output/","run/","las/")
  
  # get/map the las binary folder and create the base command line
  if (is.null(laspcFile)) stop("no directory containing las/laz files provided...\n")
  else laspcFile <- path.expand(laspcFile)
  name<-basename(laspcFile)
  
  # create project structure and export global paths
  if (!nchar(Sys.getenv("GISDBASE")) > 0 ){
  link2GI::initProj(projRootDir = tempdir() ,
                    projFolders =  projFolder)
  }


  
  if (!file.exists(paste0(path_run,name)))
    cat(":: create copy of the las file at the working directory... \n")
    file.copy(from = laspcFile,
              to = paste0(path_run,name),
              overwrite = TRUE)
  cat(":: get extent of the point cloud \n")
  if (!is.null(cutExtent)){
    las<-lidR::lasclipRectangle(las, as.numeric(cutExtent[1]), as.numeric(cutExtent[3]), as.numeric(cutExtent[2]), as.numeric(cutExtent[4]))
    lidR::writeLAS(las ,paste0(path_run,"cut_point_cloud.las"))
    sp_param <- c(as.character(las$`Min X`),as.character(las$`Min Y`),as.character(las$`Max X`),as.character(las$`Max Y`))
    # rename output file according to the extent
    fn<- paste(sp_param ,collapse=" ")
    tmp <- gsub(paste(sp_param ,collapse=" "),pattern = " ",replacement = "_")
    name<-paste0(gsub(tmp,pattern = "[.]",replacement = "_"),".las")
    file.rename(from =paste0(path_run,"cut_point_cloud.las"),
                to = paste0(path_run,name))
    
  } else {
    las<-rlas::readlasheader(paste0(path_run,name))
    sp_param <- c(as.character(las$`Min X`),as.character(las$`Min Y`),as.character(las$`Max X`),as.character(las$`Max Y`))
    # rename output file according to the extent
    fn<- paste(sp_param ,collapse=" ")
    tmp <- gsub(paste(sp_param ,collapse=" "),pattern = " ",replacement = "_")
    name<-paste0(gsub(tmp,pattern = "[.]",replacement = "_"),".las")
    file.rename(from =paste0(path_run,basename(laspcFile)),
                to = paste0(path_run,name))
  }
  # copy it to the output folder
  sp_param[5] <- proj4
  cat(":: link to GRASS\n")
  link2GI::linkGRASS7(gisdbase = gisdbasePath,
                      location = "pc2D_dsm", 
                      spatial_params = sp_param, 
                      resolution = targetGridSize, 
                      returnPaths = FALSE, 
                      quiet = TRUE)
  
  cat(":: sampling ", sampleMethod, " altitudes using : ", targetGridSize ,"meter grid size\n")  
  ret <- rgrass7::execGRASS("r.in.lidar",
                            flags  = c("overwrite","quiet","o","e","n"),
                            input  = paste0(path_run,name),
                            output = paste0("dsm",targetGridSize),
                            method = sampleMethod,
                            pth    = threshold,
                            resolution = targetGridSize,
                            intern = TRUE,
                            ignore.stderr = TRUE
  )
  
  cat(":: filling no data values if so \n")
  dsm<- raster::writeRaster(raster::raster(rgrass7::readRAST(paste0("dsm",targetGridSize))),paste0(path_run,"dsm"), overwrite=TRUE,format="GTiff")
  ret <- system(paste0("gdal_fillnodata.py ",
                       path_run,"dsm.tif ",
                       path_run,"dsm.tif"),intern = TRUE)
  dsm <- raster::raster(paste0(path_run,"dsm.tif"))
  if (!verbose)  {
    Sys.setenv("GRASS_VERBOSE"=GV)
    set.ignore.stderrOption(ois)
  }

  return(dsm)
}
