
if (!isGeneric('pc2dtm')) {
  setGeneric('pc2dtm', function(x, ...)
    standardGeneric('pc2dtm'))
}

#'@name pc2dtm
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
#'@param path_lastools character. folder containing the Windows binary files of the lastools
#'@param thin_with_grid default 0.5 meter. Grid stepsize for data thinning
#'@param keep_class default is 2. Default ground class of las/laz conform data
#'@param bulge  default is 1.5. 'A parameter to filter spikes it is set to a step_size/10 and then clamped into the range from 1.0 to 2.0
#'@param level_max default is 3, Maximum number of spline iterations highly suggested to take odd numbers. As higher as more detailed (spurious).
#'@param step_size  default is 25 meter. lastools key words if \code{city},\code{town},\code{metro},\code{nature},\code{wilderness} or experiment with free values
#'@param sub_size = "8", default is 8 meter. lastools key words if \code{extra_coarse},\code{coarse},\code{fine},\code{extra_fine},\code{ultra_fine},\code{hyper_fine} or experiment with free values
#'@param dtm_minalt default is \code{0}, minimum DTM altitude accepted
#'@param dtm_area default \code{FALSE} generate polygon of valid DTM data
#'@param cores number of cores that will be used
#'@param proj4  default is EPSG 32632, any valid proj4 string that is assumingly the correct one
#'@param giLinks list of GI tools cli pathes, default is NULL
#'@param dtm_maxalt dtm maximum altitude
#'@param projSubFolder subfolders that will be created/linked for R related GRASS processing
#'@param verbose to be quiet (1)
#'@param cutExtent clip area

#'@export pc2dtm
#'
#'@examples
#'\dontrun{
#' pc2dtm(lasDir =  "~/path/to/lasdata",
#'        gisdbase_path = "~/temp5",
#'        thin_with_grid = "0.5",
#'        level_max = "5" ,
#'        grid_size = "0.5")
#'}

pc2dtm <- function(lasDir = NULL,
                      gisdbase_path = NULL,
                      thin_with_grid = "0.5",
                      keep_class = "2",
                      bulge = "1.5",
                      level_max = "4" ,
                      step_size = "city",
                      sub_size = "ultra_fine",
                      grid_size = "0.25",
                      dtm_minalt = 0,
                      dtm_maxalt = 4000,
                      dtm_area = FALSE,
                   cutExtent = NULL,
                      projSubFolder = c("data/","output/","run/","las/"),
                      proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs",
                      cores = "3",
                   path_lastools = NULL,
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
  if (is.null(lasDir)) stop("no directory containing las/laz files provided...\n")
  lasDir <- path.expand(lasDir)
  if (Sys.info()["sysname"] == "Windows") {
    #cmd <- path_lastools <- paste(system.file(package = "uavRst"), "lastools", sep="/")/lastools/bin
    if (is.null(path_lastools)) {cmd <- path_lastools <- "C:/lastools/bin"
    if (verbose) cat("\n You did not provide a path to your lastool binary folder. Assuming C:/lastools/bin\n")}
  } else {
    #cmd <- paste("wine ",path_lastools <- paste(system.file(package = "uavRst"), "lastools",  sep="/"))
    if (is.null(path_lastools)) {cmd <- path_lastools <- paste0("wine ",path.expand("~/apps/lastools/bin"))
    if (verbose) cat("\n You did not provide a path to your lastool binary folder. Assuming wine ~/apps/lastools/bin\n")}

  }

  # create cmd strings
  las2las       <- paste(cmd,"las2las.exe",sep = "/")
  lasmerge      <- paste(cmd,"lasmerge.exe",sep = "/")
  lasground_new <- paste(cmd,"lasground_new.exe",sep = "/")
  las2dem       <- paste(cmd,"las2dem.exe",sep = "/")
  las2txt       <- paste(cmd,"las2txt.exe",sep = "/")

  # check las / laz files; laz will be preferred
  tmplasDir<-dirname(lasDir)
  lasFileNames <- list.files(pattern = "[.]las$", path = tmplasDir, full.names = TRUE)
  lazFileNames <- list.files(pattern = "[.]laz$", path = tmplasDir, full.names = TRUE)
  if (length(lazFileNames) > 0 ) extFN <- substr(extension(basename(lazFileNames[1])),2,4)
  else if (length(lasFileNames) > 0) extFN <- substr(extension(basename(lasFileNames[1])),2,4)
  else stop("no valid las or laz files found...\n")



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

  # create project structure and export global paths
  link2GI::initProj(projRootDir = gisdbase_path,
                    projFolders =  projSubFolder)

  # set lastool folder
  path_lastools <- path.expand(path_lastools)

  #unlink(paste0(path_run,"*"), force = TRUE)

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
  file.copy(from = paste0(path_run,name),
            to = paste0(path_output,fn,".las"),
            overwrite = TRUE)

  # add proj4 string manually
  sp_param[5] <- proj4

  ### reduce the data amount
  cat("\n:: reducing the point density...\n")
  ret <- system(paste0(las2las,
                       " -i ",path_run,name,
                       " -odix _reduced ",
                       " -odir ",path_run,
                       " -o",extFN,
                       " -keep_class ",keep_class,
                       " -thin_with_grid ",thin_with_grid),
                intern = TRUE,
                ignore.stderr = TRUE
  )


  #### starting lastools classification
  # run lasground
  cat(":: classify ground points (lastools) ...\n")
  ret <- system(paste0(lasground_new,
                       " -i ",path_run,tools::file_path_sans_ext(name),"_reduced.",extFN,
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
                       " -step ",grid_size,
                       " -ocut 3 ",
                       " -odix _las2dtm_DTM ",
                       " -otif ",
                       " -odir ",path_output,
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
  # r <- raster::rasterFromXYZ(paste0(path_output,"o_dtm.tif"))
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
                       ' -TARGET_USER_SIZE ',grid_size,
                       ' -METHOD 1',
                       ' -EPSILON 0.000100',
                       ' -LEVEL_MAX ',level_max),
                intern = TRUE,
                ignore.stderr = TRUE)
  dtm<-raster::raster(paste0(path_run,"rawdtm.sdat"))
  raster::projection(dtm)<-proj4
  # dtm <- gdalUtils::gdalwarp(paste0(path_run,"rawdtm.sdat"),
  #                            paste0(path_output,"dtm.tif"),
  #                            t_srs = proj4,
  #                            output_Raster = TRUE,
  #                            overwrite = TRUE,
  #                            verbose = FALSE)
  cat(":: calculate metadata ... \n")
  dtm[dtm <= dtm_minalt] <- NA
  dtm[dtm > dtm_maxalt] <- NA
  raster::writeRaster(dtm, paste0(path_output,fn, "_dtm.tif"),overwrite = TRUE)
  e <- extent(dtm)
  dtmA <- as(e, 'SpatialPolygons')
  dtmA <- methods::as(raster::extent(dtm), "SpatialPolygons")
  if (dtm_area) {
    dtm2 <- dtm > -Inf
    tmp <- raster::aggregate(dtm2,fact = 1 / grid_size)
    dtmdA  <- rasterToPolygons(tmp)
    dtmdA  <- rgeos::gUnaryUnion(dtmdA)
    #dtmdA <- rasterToPolygons(dtm2, dissolve=TRUE)
  } else { dtmdA <- NULL}
  if (!verbose)  {
    Sys.setenv("GRASS_VERBOSE"=GV)
    set.ignore.stderrOption(ois)
  }
  return(list(dtm,dtmA,dtmdA,paste0(fn,".",extFN)))
}
