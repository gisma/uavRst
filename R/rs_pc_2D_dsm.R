
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
#'@param grassVersion numeric. version of GRASS as derived by findGRASS() default is 1 (=oldest/only version) please note GRASS version later than 7.4 is not working with r.inlidar
#'@param searchPath path to look for grass
#'

#'@export pc_2D_dsm

#'@examples

#'\dontrun{
#' require(uavRst)
#' require(link2GI)
#'
#' # create and check the links to the GI software
#' giLinks<-list()
#' giLinks$grass<-link2GI::linkGRASS7(returnPaths = TRUE)
#' if (giLinks$grass$exist) {
#'
#' # proj subfolders
#' owd <- getwd()
#' projRootDir<-tempdir()
#'unlink(paste0(projRootDir,"*"), force = TRUE)
#' paths<-link2GI::initProj(projRootDir = projRootDir,
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")

#' # get some colors
#' pal = mapview::mapviewPalette("mapviewTopoColors")
#'
#' # get the data
#' utils::download.file(url="https://github.com/gisma/gismaData/raw/master/uavRst/data/lidar.las",
#' destfile="lasdata.las")
#'
#' # create 2D pointcloud DSM
#' dsm <- pc_2D_dsm(laspcFile = "lasdata.las",
#'                 gisdbasePath = projRootDir,
#'                 sampleMethod = "max",
#'                 targetGridSize = 0.5,
#'                 giLinks = giLinks)
#'                 }
#'  raster::plot(dsm)
#'  setwd(owd)
#'}

pc_2D_dsm <- function(laspcFile = NULL,
                    gisdbasePath = NULL,
                    grassVersion=1,
                    searchPath =NULL,
                    sampleMethod = "max",
                    threshold = 20 ,
                    cutExtent = NULL,
                    targetGridSize = 0.25,
                    projFolder = NULL,
                    proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs",
                    giLinks =NULL,
                    verbose = FALSE) {
  if (!exists("path_run")) path_run = tempdir()
  gdal <- link2GI::linkGDAL()
  if (is.null(searchPath)){
  if(Sys.info()["sysname"]=="Windows") searchPath="C:"
  else searchPath <- "/usr"}

  if (!verbose){
    GV <- Sys.getenv("GRASS_VERBOSE")
    Sys.setenv("GRASS_VERBOSE"=0)
    ois <- rgrass7::get.ignore.stderrOption()
    rgrass7::set.ignore.stderrOption(TRUE)}

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



  if (!file.exists(file.path(R.utils::getAbsolutePath(path_run),name))){
    cat(":: create copy of the las file at the working directory... \n")
    file.copy(from = laspcFile,
              to = file.path(R.utils::getAbsolutePath(path_run),name),
              overwrite = TRUE)}
  cat(":: get extent of the point cloud \n")
  if (!is.null(cutExtent)){
    las<-lidR::readLAS(file.path(R.utils::getAbsolutePath(path_run),name))
    las<-lidR::lasclipRectangle(las, as.numeric(cutExtent[1]), as.numeric(cutExtent[3]), as.numeric(cutExtent[2]), as.numeric(cutExtent[4]))
    lidR::writeLAS(las ,file.path(R.utils::getAbsolutePath(path_run),"cut_point_cloud.las"))
    lasxt<-lidR::extent(las)
    sp_param <- c(lasxt@xmin,lasxt@ymin,lasxt@xmax,lasxt@ymax)
    # rename output file according to the extent
    fn<- paste(sp_param ,collapse=" ")
    tmp <- gsub(paste(sp_param ,collapse=" "),pattern = " ",replacement = "_")
    name<-paste0(gsub(tmp,pattern = "[.]",replacement = "_"),".las")
    file.rename(from =file.path(R.utils::getAbsolutePath(path_run),"cut_point_cloud.las"),
                to = file.path(R.utils::getAbsolutePath(path_run),name))

  } else {
    las<-rlas::read.lasheader(file.path(R.utils::getAbsolutePath(path_run),name))
    sp_param <- c(as.character(las$`Min X`),as.character(las$`Min Y`),as.character(las$`Max X`),as.character(las$`Max Y`))
    # rename output file according to the extent
    fn<- paste(sp_param ,collapse=" ")
    tmp <- gsub(paste(sp_param ,collapse=" "),pattern = " ",replacement = "_")
    name<-paste0(gsub(tmp,pattern = "[.]",replacement = "_"),".las")
    file.rename(from =file.path(R.utils::getAbsolutePath(path_run),basename(laspcFile)),
                to = file.path(R.utils::getAbsolutePath(path_run),name))
  }
  # copy it to the output folder
  sp_param[5] <- proj4
  cat(":: link to GRASS\n")
  link2GI::linkGRASS7(gisdbase = gisdbasePath,
                      location = "pc_2D_dsm",
                      spatial_params = sp_param,
                      resolution = targetGridSize,
                      returnPaths = FALSE,
                      ver_select = grassVersion,
                      search_path = searchPath,
                      quiet = TRUE)

  cat(":: sampling ", sampleMethod, " altitudes using : ", targetGridSize ,"meter grid size\n")

    # ret <- rgrass7::execGRASS("r.in.pdal",
    #                           flags  = c("overwrite","quiet"),
    #                           input  = file.path(R.utils::getAbsolutePath(path_run),name),
    #                           output = paste0("dsm",targetGridSize),
    #                           method = sampleMethod,
    #                           pth = threshold,
    #                           proj_in = sp_param[5],
    #                           resolution = as.numeric(targetGridSize),
    #                           intern = TRUE,
    #                           ignore.stderr = FALSE
    # )

  #else if (grepl(rgrass7::execGRASS(cmd = "g.extension",flags =  c("l"),Sys_show.output.on.console = TRUE),pattern = "r.in.lidar"))
rgrass7::execGRASS("r.in.lidar",
                            flags  = c("overwrite","quiet","o"),
                            input  = file.path(R.utils::getAbsolutePath(path_run),name),
                            output = paste0("dsm",targetGridSize),
                            method = sampleMethod,
                            pth    = threshold,
                            resolution = targetGridSize,
                            intern = TRUE,
                            ignore.stderr = FALSE
  )
    dsm<- raster::writeRaster(raster::raster(rgrass7::readRAST(paste0("dsm",targetGridSize))),file.path(R.utils::getAbsolutePath(path_run),"dsm1.tif"),
                              overwrite=TRUE,format="GTiff")

    if (Sys.info()["sysname"] == "Linux"){
  cat(":: filling no data values if so \n")
  ret <- system(paste0("gdal_fillnodata.py ",
                       path_run,"dsm1.tif ",
                       path_run,"dsm.tif"),intern = TRUE)
  dsm <- raster::raster(file.path(R.utils::getAbsolutePath(path_run),"dsm.tif"))
    }
    else
  dsm <- raster::raster(file.path(R.utils::getAbsolutePath(path_run),"dsm1.tif"))
  if (!verbose)  {
    Sys.setenv("GRASS_VERBOSE"=GV)
    rgrass7::set.ignore.stderrOption(ois)
  }

  return(dsm)
}
