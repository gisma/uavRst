#'@name pc3D_dtm
#'@title create a Digital Terrain Model from preclassified point cloud data
#'
#'@description
#' Create a Digital Terrain Model from a high density point cloud as typically derived by an optical UAV retrieval.
#'
#'@author Chris Reudenbach
#'
#'@param lasDir  character. default is \code{NULL} path  to the laz/las file(s)
#'@param gisdbasePath character. default is \code{NULL} root directory of the project. NOTE the function creates two subfolder named \code{run} and \code{output}
#'@param gridSize  numerical. resolution of the DTM raster
#'@param pathLastools character. folder containing the Windows binary files of the lastools
#'@param thinGrid numerical. default 0.5 meter. Grid stepsize for data thinning
#'@param keepClass numerical. default is 2. Default ground class of las/laz conform data
#'@param bulge  numerical. default is 1.5. 'A parameter to filter spikes it is set to a stepSize/10 and then clamped into the range from 1.0 to 2.0
#'@param splineNumber numerical. default is 3, Maximum number of spline iterations highly suggested to take odd numbers. As higher as more detailed (spurious).
#'@param stepSize  character. default is 25 meter. lastools key words if \code{city},\code{town},\code{metro},\code{nature},\code{wilderness} or experiment with free values
#'@param subSize = character. "8", default is 8 meter. lastools key words if \code{extra_coarse},\code{coarse},\code{fine},\code{extra_fine},\code{ultra_fine},\code{hyper_fine} or experiment with free values
#'@param dtmarea logical. default \code{FALSE} generate polygon of valid DTM data
#'@param cores numerical. number of cores that will be used
#'@param proj4  default is EPSG 32632, any valid proj4 string that is assumingly the correct one
#'@param giLinks list of GI tools cli pathes, default is NULL
#'@param projsubFolders list of character contaiing subfolders that will be created/linked for R related GRASS processing
#'@param verbose logical. to be quiet (1)
#'@param cutExtent object of typ extent deteerming the clip area
#'@param MP character mounting point / drive letter default is "~"

#'@export pc3D_dtm
#'
#'@examples

#' \dontrun{
#'require(uavRst)
#'require(link2GI)
#'
#' # create and check the links to the GI software
#' giLinks<-list()
#' giLinks$saga<-link2GI::linkSAGA()
#' if (giLinks$saga$exist) {
#'# proj subfolders
#'projRootDir<-tempdir()
#'unlink(paste0(projRootDir,"*"), force = TRUE)

#'projsubFolders<-c("data/","data/ref/","output/","run/","las/")
#'paths<-link2GI::initProj(projRootDir = projRootDir,
#'                         projFolders = projsubFolders,
#'                         global = TRUE,
#'                         path_prefix = "path_")
#'setwd(paste0(projRootDir,"run"))
#'# get some colors
#'pal = mapview::mapviewPalette("mapviewTopoColors")
#'
#'# get the data
#'utils::download.file(url="https://github.com/gisma/gismaData/raw/master/uavRst/data/lidar.las",
#'                     destfile="lasdata.las")
#'
#'# create a DSM  based on a uav point cloud
#' pc3DTM <- pc3D_dtm(lasDir =  "lasdata.las",
#'                       gisdbasePath = projRootDir,
#'                       projsubFolders = projsubFolders,
#'                       thinGrid = 1.,
#'                       splineNumber = 5 ,
#'                       gridSize = 0.5,
#'                       giLinks = giLinks)
#'mapview::mapview(pc3DTM[[1]]) 
#'}
#'}


pc3D_dtm <- function(lasDir = NULL,
                      gisdbasePath = NULL,
                      thinGrid = "0.5",
                      keepClass = "2",
                      bulge = "1.5",
                      splineNumber = "4" ,
                      stepSize = "city",
                      subSize = "ultra_fine",
                      gridSize = "0.25",
                      dtmarea = FALSE,
                   cutExtent = NULL,
                      projsubFolders = c("data/","output/","run/","las/"),
                      proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs",
                      cores = "3",
                   pathLastools = NULL,
                   giLinks =NULL,
                   MP ="~",
                   verbose = FALSE) {
  LASbin<-searchLastools(MP=MP)
  if (length(LASbin)<1) stop("\n At ",MP," no LAStool binaries found")
  else lasbin<- as.character(LASbin[[1]][,])
  # if (is.null(giLinks)){
  #   giLinks <- linkAll()
  # }
  gdal <- link2GI::linkGDAL()
  saga <- link2GI::linkSAGA()
  #otb <- giLinks$otb
  sagaCmd<-saga$sagaCmd
  #path_OTB <- otb$pathOTB
  if (!verbose){
    GV <- Sys.getenv("GRASS_VERBOSE")
    Sys.setenv("GRASS_VERBOSE"=0)
    ois <- rgrass7::get.ignore.stderrOption()
    rgrass7::set.ignore.stderrOption(TRUE)}

  # get/map the las binary folder and create the base command line
  if (is.null(lasDir)) stop("no directory containing las/laz files provided...\n")
  lasDir <- path.expand(lasDir)
  if (Sys.info()["sysname"] == "Windows") {
    #cmd <- pathLastools <- paste(system.file(package = "uavRst"), "lastools", sep="/")/lastools/bin
    if (is.null(pathLastools)) {cmd <- pathLastools <- lasbin
    if (verbose) cat("\n You did not provide a path to your lastool binary folder. Assuming C:/lastools/bin\n")}
  } else {
    #cmd <- paste("wine ",pathLastools <- paste(system.file(package = "uavRst"), "lastools",  sep="/"))
    if (is.null(pathLastools)) {cmd <- pathLastools <- paste0("wine ",path.expand(lasbin))
    if (verbose) cat("\n You did not provide a path to your lastool binary folder.\n
                         Assumed to be somewher in your home directory")}

  }

  # create cmd strings
  las2las       <- paste(cmd,"las2las.exe",sep = "/")
  lasmerge      <- paste(cmd,"lasmerge.exe",sep = "/")
  lasground_new <- paste(cmd,"lasground_new.exe",sep = "/")
  las2dem       <- paste(cmd,"las2dem.exe",sep = "/")
  las2txt       <- paste(cmd,"las2txt.exe",sep = "/")

  # map the code words
  if (stepSize == "city") step <- "25"
  else if (stepSize == "town") step <- "10"
  else if (stepSize == "metro") step <- "50"
  else if (stepSize == "nature") step <- "5"
  else if (stepSize == "wilderness") step <- "3"
  if (subSize == "extra_coarse") sub <- "3"
  else if (subSize == "coarse") sub <- "4"
  else if (subSize == "fine") sub <- "6"
  else if (subSize == "extra_fine") sub <- "7"
  else if (subSize == "ultra_fine") sub <- "8"
  else if (subSize == "hyper_fine") sub <- "9"

  # create project structure and export global paths
  link2GI::initProj(projRootDir = gisdbasePath,
                    projFolders =  projsubFolders)

  # set lastool folder
  pathLastools <- path.expand(pathLastools)


  setwd(path_run)


  if(raster::extension(basename(lasDir)) !=".las" & raster::extension(basename(lasDir)) !=".laz") {
    # check las / laz files; laz will be preferred
    lasFileNames <- list.files(pattern = "[.]las$", path = lasDir, full.names = TRUE)
    lazFileNames <- list.files(pattern = "[.]laz$", path = lasDir, full.names = TRUE)
    if (length(lazFileNames) > 0 ) {
      extFN <- substr(raster::extension(basename(lazFileNames[1])),2,4)
      noF <- length(lazFileNames)
    }
    else if (length(lasFileNames) > 0) {
      extFN <- substr(raster::extension(basename(lasFileNames[1])),2,4)
      noF <- length(lasFileNames)
    }
    else stop("no valid las or laz files found...\n")
    # merge all files
    cat("\nNOTE: You are dealing with a huge UAV generated point cloud data set.\n      so take time and keep relaxed... :-)\n")
    cat(":: merge and decompress ",noF," point cloud files...\n")
    ret <- system(paste0(lasmerge,
                         " -i ",lasDir,"/*.",extFN,
                         " -olas",
                         " -o ",path_run,"full_point_cloud.las"),
                  intern = TRUE,
                  ignore.stderr = TRUE
    )
    name<-"full_point_cloud.las"
  } else {

    name<-basename(lasDir)
    extFN<- substr(raster::extension(basename(paste0(path_run,name))),2,4)
    if (!file.exists(paste0(path_run,name)))
    file.copy(from = lasDir,
              to = paste0(path_run,name),
              overwrite = TRUE)


  }

  if (!is.null(cutExtent)){
    #lastool(tool = "lasclip",lasFile = lasfile,cutExtent = cutExtent)
    las = lidR::readLAS(paste0(path_run,name))
    las_clip<-lidR::lasclipRectangle(las, as.numeric(cutExtent[1]), as.numeric(cutExtent[3]), as.numeric(cutExtent[2]), as.numeric(cutExtent[4]))
    lidR::writeLAS(las_clip ,paste0(path_run,"cut_point_cloud.las"))
    name<-"cut_point_cloud.las"
  }
  # get extent of merged file
  sp_param <-lastool(lasFile= paste0(path_run,name))

  # rename output file according to the extent
  fn<- paste(sp_param ,collapse=" ")
  tmp <- gsub(paste(sp_param ,collapse=" "),pattern = " ",replacement = "_")
  fn<-gsub(tmp,pattern = "[.]",replacement = "_")
  # copy it to the output folder
  file.rename(from = paste0(path_run,name),
            to = paste0(path_run,fn,".las"))
  name<-paste0(path_run,fn,".las")
  # add proj4 string manually
  sp_param[5] <- proj4

  ### reduce the data amount
  cat("\n:: reducing the point density...\n")
  ret <- system(paste0(las2las,
                       " -i ",name,
                       " -odix _reduced ",
                       " -odir ",path_run,
                       " -o",extFN,
                       " -keep_class ",keepClass,
                       " -thin_with_grid ",thinGrid),
                intern = TRUE,
                ignore.stderr = TRUE
  )


  #### starting lastools classification
  # run lasground
  cat(":: classify ground points (lastools) ...\n")
  ret <- system(paste0(lasground_new,
                       " -i ",tools::file_path_sans_ext(name),"_reduced.",extFN,
                       " -all_returns ",
                       " -bulge ", bulge,
                       " -skip_files",
                       " -step ", step,
                       " -sub ", sub,
                       " -odix _ground -o",extFN,
                       " -cores ",cores),
                intern = FALSE,
                ignore.stderr = TRUE
  )


  # create lastools  DTM
  ret <- system(paste0(las2dem,
                       " -i ",path_run,"*ground.",extFN,
                       " -keep_class 2",
                       " -extra_pass",
                       " -step ",gridSize,
                       " -ocut 3 ",
                       " -odix _las2dtm_DTM ",
                       " -otif ",
                       " -odir ",path_run,
                       " -cores ",cores),
                intern = TRUE,
                ignore.stderr = TRUE
  )


  # export las file to text file
  ret <- system(paste0(las2txt,
                       " -i ",path_run,"*ground.",extFN,
                       " -parse xyzrRGB",
                       " -sep komma"),
                intern = TRUE,
                ignore.stderr = TRUE
  )

  #### starting SAGA classification
  # create output mask file for interpolation
  # cat(":: classify ground points (aDTM) ...\n")
  # r <- raster::rasterFromXYZ(paste0(path_run,"o_dtm.tif"))
  # r[r > 0] <- 0
  # raster::writeRaster(r,filename = paste0(path_run,"rawdtm.tif"),overwrite = TRUE)
  # gdalUtils::gdalwarp(paste0(path_run,"rawdtm.tif"),
  #                     paste0(path_run,"rawdtm.sdat"),
  #                     overwrite = TRUE,
  #                     of = 'SAGA',
  #                     verbose = FALSE)
  #

  # import to saga as point cloud

  #saga <- link2GI::linkSAGA()
  #sagaCmd<-saga$sagaCmd
  txtfile <- lasFileNames <- list.files(pattern = "ground[.]txt$", path = path_run, full.names = TRUE)
  ret <- system(paste0(sagaCmd,' io_shapes 16 ',
                       ' -POINTS ', path_run,'pointcloud',
                       ' -FILE  ', txtfile,
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
                       ' -TARGET_USER_SIZE ',gridSize,
                       ' -TARGET_USER_XMIN ',sp_param[1],
                       ' -TARGET_USER_XMAX ',sp_param[3],
                       ' -TARGET_USER_YMIN ',sp_param[2],
                       ' -TARGET_USER_YMAX ',sp_param[4],
                       ' -METHOD 1',
                       ' -EPSILON 0.000100',
                       ' -LEVEL_MAX ',splineNumber),
                intern = TRUE,
                ignore.stderr = TRUE)
  dtm<-raster::raster(paste0(path_run,"rawdtm.sdat"))
  raster::projection(dtm)<-proj4
  # dtm <- gdalUtils::gdalwarp(paste0(path_run,"rawdtm.sdat"),
  #                            paste0(path_run,"dtm.tif"),
  #                            t_srs = proj4,
  #                            output_Raster = TRUE,
  #                            overwrite = TRUE,
  #                            verbose = FALSE)
  cat(":: calculate metadata ... \n")
  raster::writeRaster(dtm, paste0(path_run,fn, "_dtm.tif"),overwrite = TRUE)
  e <- extent(dtm)
  dtmA <- methods::as(e, 'SpatialPolygons')
  dtmA <- methods::as(raster::extent(dtm), "SpatialPolygons")
  if (dtmarea) {
    dtm2 <- dtm > -Inf
    tmp <- raster::aggregate(dtm2,fact = 1 / gridSize)
    dtmdA  <- rasterToPolygons(tmp)
    dtmdA  <- rgeos::gUnaryUnion(dtmdA)
    #dtmdA <- rasterToPolygons(dtm2, dissolve=TRUE)
  } else { dtmdA <- NULL}
  if (!verbose)  {
    Sys.setenv("GRASS_VERBOSE"=GV)
    rgrass7::set.ignore.stderrOption(ois)
  }
  return(list(dtm,dtmA,dtmdA,paste0(fn,".",extFN)))
}
