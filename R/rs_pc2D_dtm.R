
#' Create a Digital Terrain Model from UAV generated point clouds by minimum altitude sampling (fix window)
#'
#'@description
#' Create a Digital Terrain Model from a high density point cloud as typically derived by an optical UAV retrieval. Due to the poor estimation of ground points
#' a minimum samopling approach is applied. It retrieves on a coarse sampling gridsize the minimum value and interpolates on these samples a surface grid with a higher target
#' resolution. this is a kind of an try and error process and provides fairly good results if the point cloud shows at least some real surface points on a not to coarse grid.
#'
#'@author Chris Reudenbach
#'
#'@param laspcFile  character. default is \code{NULL} path  to the laz/las file(s)
#'@param gisdbasePath character. default is \code{NULL} root directory of the project. NOTE the function creates two subfolder named \code{run} and \code{output}
#'@param sampleMethod  character. sampling method of r.in.lidar Statistic to use for raster values Options: n, min, max, range, sum, mean, stddev, variance, coeff_var, median, percentile, skewness, trimmean Default: mean
#'@param sampleGridSize  numeric, resolution extraction raster
#'@param targetGridSize numeric. the resolution of the target DTM raster
#'@param splineThresGridSize numeric. threshold of minimum gridsize tha is used for splininterpolation if the desired resolution is finer a two step approximation is choosen
#'first step spline interpolation using the treshold gridsize second step bilinear resampling to the desired targetGridSize.
#'@param tension  numeric. tension of spline interpolation.
#'@param proj4  character. valid proj4 string that should be assumingly the correct one
#'@param giLinks list of link2GI cli pathes, default is NULL
#'@param projFolder subfolders that will be created/linked for R related GRASS processing
#'@param verbose to be quiet (1)
#'@param cutExtent clip area
#'@param grassVersion numeric. version of GRASS as derived by findGRASS() default is 1 (=oldest/only version) please note GRASS version later than 7.4 is not working with r.inlidar
#'@param searchPath path to look for grass

#'@export pc2D_dtm_fw

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
#' projRootDir<-tempdir()
#' unlink(paste0(projRootDir,"*"), force = TRUE)
#' paths<-link2GI::initProj(projRootDir = projRootDir,
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")
#'  
#' # get some colors
#' pal = mapview::mapviewPalette("mapviewTopoColors")
#'
#' # get the data
#' utils::download.file(url="https://github.com/gisma/gismaData/raw/master/uavRst/data/lidar.las",
#' destfile=paste0("lasdata.las"))
#'
#' # create 2D point cloud DTM
#' dtm <- pc2D_dtm_fw(laspcFile = "lasdata.las",
#'                 gisdbasePath = projRootDir,
#'                 tension = 20 ,
#'                 sampleGridSize = 25,
#'                 targetGridSize = 0.5,
#'                 giLinks = giLinks)
#'                 
#' mapview::mapview(dtm)                 
#'                 }
#' 
#' }

pc2D_dtm_fw <- function(laspcFile = NULL,
                        grassVersion=1,
                        searchPath =NULL,
                        gisdbasePath = NULL,
                        tension = 20 ,
                        sampleMethod ="min",
                        cutExtent = NULL,
                        sampleGridSize=25,
                        targetGridSize = 0.25,
                        splineThresGridSize = 0.5,
                        projFolder = NULL,
                        proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs",
                        giLinks =NULL,
                        verbose = FALSE) {
  if (is.null(searchPath)){
    if(Sys.info()["sysname"]=="Windows") searchPath="C:"
    else searchPath <- "/usr"}
  
  # if (is.null(giLinks)){
  #   giLinks <- linkAll()
  # }
  gdal <- giLinks$gdal
  # saga <- giLinks$saga
  # otb <- giLinks$otb
  # sagaCmd<-saga$sagaCmd
  # path_OTB <- otb$pathOTB
  
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
                      projFolders =  projFolder,global = TRUE
    )
  }
  
  
  if (!file.exists(paste0(path_run,name))){
    cat(":: create copy of the las file at the working directory... \n")
    if (laspcFile != paste0(path_run,name))
      file.copy(from = laspcFile,
                to = paste0(path_run,name),
                overwrite = TRUE)}
  cat(":: get extent of the point cloud \n")
  if (!is.null(cutExtent)){
    las<-lidR::readLAS(paste0(path_run,name))
    las<-lidR::lasclipRectangle(las, as.numeric(cutExtent[1]), as.numeric(cutExtent[3]), as.numeric(cutExtent[2]), as.numeric(cutExtent[4]))
    lidR::writeLAS(las ,paste0(path_run,"cut_point_cloud.las"))
    lasxt<-lidR::extent(las)
    sp_param <- c(lasxt@xmin,lasxt@ymin,lasxt@xmax,lasxt@ymax)
    # rename output file according to the extent
    fn<- paste(sp_param ,collapse=" ")
    tmp <- gsub(paste(sp_param ,collapse=" "),pattern = " ",replacement = "_")
    name<-paste0(gsub(tmp,pattern = "[.]",replacement = "_"),".las")
    file.rename(from =paste0(path_run,"cut_point_cloud.las"),
                to = paste0(path_run,name))
    
  } else {
    las<-rlas::read.lasheader(paste0(path_run,name))
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
  
  link2GI::linkGRASS7(gisdbase = gisdbasePath,
                      location = "pc2D_dtm_fw",
                      spatial_params = sp_param,
                      resolution = sampleGridSize,
                      returnPaths = FALSE,
                      quiet = TRUE,
                      ver_select = grassVersion,
                      search_path = searchPath)
  
  cat(":: sampling minimum altitudes using : ", sampleGridSize ,"meter grid size\n")
  #if (!grepl(system("g.extension -l",ignore.stdout = TRUE),pattern = "r.in.lidar"))
  # ret <- rgrass7::execGRASS("r.in.pdal",
  #                           flags  = c("overwrite","quiet"),
  #                           input  = paste0(path_run,name),
  #                           output = paste0("dem",sampleGridSize),
  #                           method = sampleMethod,
  #                           proj_in = sp_param[5],
  #                           resolution = as.numeric(sampleGridSize),
  #                           intern = TRUE,
  #                           ignore.stderr = FALSE
  # )
  # else
  ret <- rgrass7::execGRASS("r.in.lidar",
                            flags  = c("overwrite","quiet","o","e","n"),
                            input  = paste0(path_run,name),
                            output = paste0("dem",sampleGridSize),
                            method  = sampleMethod ,
                            resolution = sampleGridSize,
                            intern = TRUE,
                            ignore.stderr = TRUE
  )
  
  # vectorize points
  ret <- rgrass7::execGRASS("r.to.vect",
                            flags  = c("overwrite","quiet","z","b"),
                            input  = paste0("dem",sampleGridSize),
                            output = "vdtm",
                            type = "point",
                            intern = TRUE,
                            ignore.stderr = TRUE
  )
  
  # set regio with new gridsize
  if (targetGridSize < splineThresGridSize) {
    oldtgs<-targetGridSize
    targetGridSize <- splineThresGridSize
    cat(":: target grid size is", targetGridSize ," => setting grid size to: ",splineThresGridSize,"\n")
  } else {
    splineThresGridSize <- targetGridSize
    oldtgs<-targetGridSize
  }
  # else  {
  #   oldtgs<- splineThresGridSize
  #   splineThresGridSize <- targetGridSize
  #   }
  
  ret <- rgrass7::execGRASS("g.region",
                            flags  = c("quiet"),
                            res= as.character(targetGridSize),
                            intern = TRUE,
                            ignore.stderr = TRUE
  )
  cat(":: create DTM by interpolation to a raster size of: ", targetGridSize ,"\n")
  ret <- rgrass7::execGRASS("v.surf.rst",
                            flags  = c("overwrite","quiet"),
                            input  = "vdtm",
                            elevation = "tdtm",
                            tension = tension,
                            intern = TRUE,
                            ignore.stderr = TRUE
  )
  
  # apply mask for input data area
  ret <- rgrass7::execGRASS("r.mapcalc",
                            flags  = c("overwrite","quiet"),
                            expression= paste0('"dtm = if(',paste0("dem",sampleGridSize),' > 0  ,tdtm,0)"'),
                            intern = TRUE,
                            ignore.stderr = TRUE
  )
  
  dtm0<- raster::writeRaster(raster::raster(rgrass7::readRAST("dtm")),paste0(path_run,"dtm0"), overwrite=TRUE,format="GTiff")
  if (oldtgs < splineThresGridSize) {
    cat(":: Resample to a grid size of: ", targetGridSize ,"\n")
    res<-gdalUtils::gdalwarp(srcfile = paste0(path_run,"dtm0.tif"),
                             dstfile = paste0(path_run,"dtm.tif"),
                             tr=c(oldtgs,oldtgs),
                             r="bilinear",
                             overwrite = TRUE,
                             multi = TRUE)
    dtm <- raster::raster(paste0(path_run,"dtm.tif"))
  } else {
    dtm <- raster::raster(paste0(path_run,"dtm0.tif"))
  }
  
  # cat(":: calculate metadata ... \n")
  # raster::writeRaster(dtm, paste0(path_run,fn, "_dtm.tif"),overwrite = TRUE)
  # e <- extent(dtm)
  # dtmA <- as(e, 'SpatialPolygons')
  # dtmA <- methods::as(raster::extent(dtm), "SpatialPolygons")
  if (!verbose)  {
    Sys.setenv("GRASS_VERBOSE"=GV)
    rgrass7::set.ignore.stderrOption(ois)
  }
  #return(list(dtm,dtmA,paste0(fn,".",extFN)))
  return(dtm)
}
