
#' Create a Digital Terrain Model from a UAV generated point cloud by minimum sampling
#'
#'@description
#' Create a Digital Terrain Model from a high density point cloud as typically derived by an optical UAV retrieval.
#'
#'@author Chris Reudenbach
#'
#'@param laspcFile  default is \code{NULL} path  to the laz/las file(s)
#'@param gisdbase_path default is \code{NULL} root directory of the project. NOTE the function creates two subfolder named \code{run} and \code{output}
#'@param gridSize  resolution extraction raster
#'@param targetGridSize the resolution of the target DTM raster
#'@param tension  numeric. tension of spline interpolation.
#'@param proj4  default is EPSG 32632, any valid proj4 string that is assumingly the correct one
#'@param giLinks list of GI tools cli pathes, default is NULL
#'@param projFolder subfolders that will be created/linked for R related GRASS processing
#'@param verbose to be quiet (1)
#'@param cutExtent clip area
#'
#'@importFrom lidR tree_detection
#'@importFrom lidR writeLAS
#'@importFrom lidR readLAS
#'@importFrom lidR lasclipRectangle
#'@export spc2dtm
#'
#'@examples
#'\dontrun{
#' spc2dtm(laspcFile =  "~/path/to/lasdata",
#'        gisdbase_path = "~/temp5",
#'        thin_with_grid = "0.5",
#'        level_max = "5" ,
#'        grid_size = "0.5")
#'}

spc2dtm <- function(laspcFile = NULL,
                    gisdbase_path = NULL,
                    tension = 20 ,
                    cutExtent = NULL,
                    gridSize=25,
                    targetGridSize = 0.5,
                    projFolder = c("data/","output/","run/","las/"),
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

  # get/map the las binary folder and create the base command line
  if (is.null(laspcFile)) stop("no directory containing las/laz files provided...\n")
  laspcFile <- path.expand(laspcFile)



  # create project structure and export global paths
  link2GI::initProj(projRootDir = gisdbase_path,
                    projFolders =  projFolder)

  

    name<-basename(laspcFile)
    extFN<- substr(raster::extension(basename(paste0(path_run,name))),2,4)
    if (!file.exists(paste0(path_run,name)))
    file.copy(from = laspcFile,
              to = paste0(path_run,name),
              overwrite = TRUE)
    cat(":: check raw point cloud \n")
    las = lidR::readLAS(paste0(path_run,name))
    sp_param <- c(as.character(las@header@PHB$`Min X`),as.character(las@header@PHB$`Min Y`),as.character(las@header@PHB$`Max X`),as.character(las@header@PHB$`Max Y`))
    if (!is.null(cutExtent)){
    las<-lidR::lasclipRectangle(las, as.numeric(cutExtent[1]), as.numeric(cutExtent[3]), as.numeric(cutExtent[2]), as.numeric(cutExtent[4]))
    lidR::writeLAS(las ,paste0(path_run,"cut_point_cloud.las"))
    sp_param <- c(as.character(las@header@PHB$`Min X`),as.character(las@header@PHB$`Min Y`),as.character(las@header@PHB$`Max X`),as.character(las@header@PHB$`Max Y`))
    name<-"cut_point_cloud.las"
  }

  # rename output file according to the extent
  fn<- paste(sp_param ,collapse=" ")
  tmp <- gsub(paste(sp_param ,collapse=" "),pattern = " ",replacement = "_")
  fn<-gsub(tmp,pattern = "[.]",replacement = "_")
  # # copy it to the output folder
  # file.copy(from = paste0(path_run,name),
  #           to = paste0(path_output,fn,".las"),
  #           overwrite = TRUE)

  # add proj4 string manually
  sp_param[5] <- proj4

  
  paths<-link2GI::linkGRASS7(gisdbase = gisdbase_path, location = "test4", spatial_params = sp_param,resolution = gridSize)
  cat(":: finding minimum data on a raster size of: ", gridSize ,"\n")  
  ret <- rgrass7::execGRASS("r.in.lidar",
                            flags  = c("overwrite","quiet","o","e","n"),
                            input  = paste0(path_run,name),
                            output = paste0("dem",gridSize),
                            method = "min",
                            resolution = gridSize,
                            intern = TRUE,
                            ignore.stderr = TRUE
  )
  #r.to.vect -z -b --overwrite input=dem20@PERMANENT output=dem20 type=point       
  ret <- rgrass7::execGRASS("r.to.vect",
                            flags  = c("overwrite","quiet","z","b"),
                            input  = paste0("dem",gridSize),
                            output = "vdtm",
                            type = "point",
                            intern = TRUE,
                            ignore.stderr = TRUE
  )
  
  
  ret <- rgrass7::execGRASS("g.region",
                            flags  = c("quiet"),
                            res= as.character(targetGridSize),
                            intern = TRUE,
                            ignore.stderr = TRUE
  )
  cat(":: interpolate points on a raster size of: ", targetGridSize ,"\n")  
  ret <- rgrass7::execGRASS("v.surf.rst",
                            flags  = c("overwrite","quiet"),
                            input  = "vdtm",
                            elevation = "tdtm",
                            tension = tension,
                            intern = TRUE,
                            ignore.stderr = TRUE
  )
  ret <- rgrass7::execGRASS("r.mapcalc",
                            flags  = c("overwrite","quiet"),
                            expression= paste0('"dtm = if(',paste0("dem",gridSize),' > 0  ,tdtm,0)"'),
                            intern = TRUE,
                            ignore.stderr = TRUE
  )
  
  
  fn<-"dtm"
  dtm<- raster::writeRaster(raster::raster(rgrass7::readRAST(fn)),paste0(path_run,fn), overwrite=TRUE,format="GTiff")
  cat(":: calculate metadata ... \n")
  raster::writeRaster(dtm, paste0(path_output,fn, "_dtm.tif"),overwrite = TRUE)
  e <- extent(dtm)
  dtmA <- as(e, 'SpatialPolygons')
  dtmA <- methods::as(raster::extent(dtm), "SpatialPolygons")
  if (!verbose)  {
    Sys.setenv("GRASS_VERBOSE"=GV)
    set.ignore.stderrOption(ois)
  }
  return(list(dtm,dtmA,paste0(fn,".",extFN)))
}
