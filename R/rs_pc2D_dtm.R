
#' Create a Digital Terrain Model from UAV generated point clouds by minimum altitude sampling
#'
#'@description
#' Create a Digital Terrain Model from a high density point cloud as typically derived by an optical UAV retrieval. Due to the poor estimation of ground points 
#' a minimum samopling approach is applied. It retrieves on a coarse sampling gridsize the minimum value and interpolates on these samples a surface grid with a higher target 
#' resolution. this is a kind of an try and error process and provides fairly good results if the point cloud shows at least some real surface points on a not to coarse grid. 
#'
#'@author Chris Reudenbach
#'
#'@param laspcFile  character. default is \code{NULL} path  to the laz/las file(s)
#'@param gisdbase_path character. default is \code{NULL} root directory of the project. NOTE the function creates two subfolder named \code{run} and \code{output}
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
#'
#'@importFrom lidR tree_detection
#'@importFrom lidR writeLAS
#'@importFrom lidR readLAS
#'@importFrom lidR lasclipRectangle
#'@importFrom rlas readlasheader
#'@export pc2D_dtm

#'@examples
#'\dontrun{
#' pc2D_dtm(laspcFile =  "~/path/to/lasdata",
#'        gisdbase_path = "~/temp5",
#'        thin_with_grid = "0.5",
#'        level_max = "5" ,
#'        grid_size = "0.5")
#'}

pc2D_dtm <- function(laspcFile = NULL,
                    gisdbase_path = NULL,
                    tension = 20 ,
                    cutExtent = NULL,
                    sampleGridSize=25,
                    targetGridSize = 0.25,
                    splineThresGridSize = 0.5,
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

  link2GI::linkGRASS7(gisdbase = gisdbase_path,
                      location = "pc2D_dtm", 
                      spatial_params = sp_param, 
                      resolution = sampleGridSize, 
                      returnPaths = FALSE, 
                      quiet = TRUE)
  
  cat(":: sampling minimum altitudes using : ", sampleGridSize ,"meter grid size\n")  
  ret <- rgrass7::execGRASS("r.in.lidar",
                            flags  = c("overwrite","quiet","o","e","n"),
                            input  = paste0(path_run,name),
                            output = paste0("dem",sampleGridSize),
                            method = "min",
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
    }
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
    }
  dtm <- raster::raster(paste0(path_run,"dtm.tif"))
  # cat(":: calculate metadata ... \n")
  # raster::writeRaster(dtm, paste0(path_output,fn, "_dtm.tif"),overwrite = TRUE)
  # e <- extent(dtm)
  # dtmA <- as(e, 'SpatialPolygons')
  # dtmA <- methods::as(raster::extent(dtm), "SpatialPolygons")
  if (!verbose)  {
    Sys.setenv("GRASS_VERBOSE"=GV)
    set.ignore.stderrOption(ois)
  }
  #return(list(dtm,dtmA,paste0(fn,".",extFN)))
  return(dtm)
}
