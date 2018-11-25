

#' simple wrapper for some LAStools functions 
#'
#'@description
#' simple wrapper for some lastools functions
#'
#'@author Chris Reudenbach
#'@param tool default is \code{lasinfo}   additionally you may choose lasrepair, lasthin,  lasmerge, lasground_new, las2dem, las2txt, lasoverage, lasclip
#'@param lasFile  default is \code{NULL} path  to the laz/las file(s)
#'@param gridSize  resolution of the DTM raster
#'@param thinGrid default 0.5 meter. Grid stepsize for data thinning
#'@param keepClass default is 2. Default ground class of las/laz conform data
#'@param bulge  default is 1.5. 'A parameter to filter spikes it is set to a stepSize/10 and then clamped into the range from 1.0 to 2.0
#'@param stepSize  default is 25 meter. lastools key words if \code{city},\code{town},\code{metro},\code{nature},\code{wilderness} or experiment with free values
#'@param subSize = "8", default is 8 meter. lastools key words if \code{extra_coarse},\code{coarse},\code{fine},\code{extra_fine},\code{ultra_fine},\code{hyper_fine} or experiment with free values
#'@param cores number of cores that will be used
#'@param proj4  default is EPSG 32632, any valid proj4 string that is assumingly the correct one
#'@param rscale rscale
#'@param stepoverlap  Spacing of overlap steps aused in lasoverage, default is NULL
#'@param xoff xoff
#'@param yoff yoff
#'@param outpath outpath
#'@param pathLastools character. folder containing the Windows binary files of the lastools
#'@param verbose keep it quiet
#'@param cutExtent NULL
#'@param cutSlice NULL

#'
#'
#'@export
#'
#'@examples
#'\dontrun{
#'
#' require(uavRst)
#' require(link2GI)
#' # get a las file from the Spain authorithy (29.2 MB)
#' # source: (https://b5m.gipuzkoa.eus/url5000/es/G_22485/PUBLI&consulta=HAZLIDAR)
#' utils::download.file(url="ftp://ftp.geo.euskadi.net/lidar/LIDAR_2012_ETRS89/LAS/038/522-4812.zip",
#'                            destfile="522-4812.zip",  quiet = TRUE, mode = "wb")
#' unzip("522-4812.zip",junkpaths = TRUE,overwrite = TRUE)
#' 
#' # convert from laz to las
#' lastool(tool="las2txt","522-4812.las")
#' 
#' # view it
#' head(read.table("522-4812.txt",sep = ","))
#' 
#'}

lastool <- function(  tool="lasinfo",
                      lasFile = NULL,
                      thinGrid = "1.0",
                      keepClass = "2",
                      bulge = "1.5",
                      stepSize = "city",
                      subSize = "ultra_fine",
                      gridSize = "1.0",
                      proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs",
                      rscale= "0.01 0.01 0.01",
                      stepoverlap = NULL,
                      cores = "2",
                      xoff = 0,
                      yoff = 0,
                      outpath = NULL,
                      pathLastools = NULL,
                      verbose = FALSE,
                      cutExtent = NULL,
                      cutSlice = NULL) {

  if (is.null(outpath)) outpath <- path.expand(getwd())
  outpath <- path.expand(outpath)

  # some basic checks
  if (is.null(lasFile)) stop("no directory containing las/laz files provided...\n")
  lasFile <- path.expand(lasFile)
  extFN<-tools::file_ext(lasFile)


  # get/map the las binary folder and create the base command line
  # if (is.null(pathLastools)) stop("no directory containing the Windows lastool binary files is provided...\n")
  if (Sys.info()["sysname"] == "Windows") {
    #cmd <- pathLastools <- paste(system.file(package = "uavRst"), "lastools", sep="/")/lastools/bin
    if (is.null(pathLastools)) {cmd <- pathLastools <- "C:/lastools/bin"
    if (verbose) cat("\n You did not provide a path to your lastool binary folder. Assuming C:/lastools/bin\n")}
  } else {
    #cmd <- paste("wine ",pathLastools <- paste(system.file(package = "uavRst"), "lastools",  sep="/"))
    if (is.null(pathLastools)) {cmd <- pathLastools <- paste0("wine ",path.expand("~/apps/lastools/bin"))
    if (verbose) cat("\n You did not provide a path to your lastool binary folder. Assuming wine ~/apps/lastools/bin\n")}

  }

  # create cmd strings according to the "keywords"
  lasinfo       <- paste(cmd,"lasinfo.exe",sep = "/")
  getSpacing    <- paste(cmd,"lasinfo.exe",sep = "/")
  lasthin       <- paste(cmd,"las2las.exe",sep = "/")
  laz2las       <- paste(cmd,"las2las.exe",sep = "/")
  lasmerge      <- paste(cmd,"lasmerge.exe",sep = "/")
  lasground_new <- paste(cmd,"lasground_new.exe",sep = "/")
  las2dem       <- paste(cmd,"las2dem.exe",sep = "/")
  las2txt       <- paste(cmd,"las2txt.exe",sep = "/")
  txt2las       <- paste(cmd,"txt2las.exe",sep = "/")
  lasrepair     <- paste(cmd,"las2las.exe",sep = "/")
  lasoverage    <- paste(cmd,"lasoverage.exe",sep = "/")
  lasclip       <- paste(cmd,"lasclip.exe",sep = "/")

  # check las / laz files laz will be preferred
  #  lasFileNames <- list.files(pattern = "[.]las$", path = lasFile, full.names = TRUE)
  #  lazFileNames <- list.files(pattern = "[.]laz$", path = lasFile, full.names = TRUE)
  #  if (length(lazFileNames) > 0 ) extFN <- substr(extension(basename(lazFileNames[1])),2,4)
  #  else if (length(lasFileNames) > 0) extFN <- substr(extension(basename(lasFileNames[1])),2,4)
  #  else stop("no valid las or laz files found...\n")


  # map the las code words
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



  # merge all files
  if (tool == "lasmerge"){
    cat("\nNOTE: You are dealing with a huge UAV generated point cloud data set.\n      so take time and keep relaxed... :-)\n")
    # remove old temp merge file
    file.remove(paste0(outpath,"full_point_cloud.las"))
    # get input file list
    lasfiles<-list.files(paste0(lasFile),pattern=".las$", full.names=TRUE)

    # build command
    command <- lasmerge
    command <- paste0(command," -i ",lasFile,"/*.las")
    command <- paste0(command," -olas")
    command <- paste0(command," -o ",outpath,"full_point_cloud.las")

    # execute
    ret <- system(command,intern = FALSE, ignore.stderr = FALSE)
    # get extent of merged file
    #sp_param <- getSpatialLASInfo(lasinfo,paste0(outpath,"full_point_cloud.las"))
    sp_param <- lastool(lasFile= paste0(outpath,"full_point_cloud.las"))
    # create name of merged file
    name<- paste(sp_param ,collapse=" ")
    tmp <- gsub(paste(sp_param ,collapse=" "),pattern = " ",replacement = "_")
    fn<-gsub(tmp,pattern = "[.]",replacement = "_")
    # rename merged file
    file.rename(from = paste0(outpath,"full_point_cloud.las"),
                to = paste0(outpath,"merged_",fn,".las"))
    # delete input
    file.remove(lasfiles)
    # add proj4 string manually
    sp_param[5] <- proj4
  }
  ### reduce data amount
  if (tool=="lasthin"){
    cat("\n:: reducing points by factor",thinGrid," ...\n")
    # build command
    command <- lasthin
    command <- paste0(command, " -i ",lasFile)
    command <- paste0(command," -odix _reduced")
    command <- paste0(command," -odir ",outpath)
    command <- paste0(command," -olas")
    command <- paste0(command," -keep_class ",keepClass)
    command <- paste0(command," -thin_with_grid ",thinGrid)

    # execute
    ret <- system(command,intern = FALSE, ignore.stderr = FALSE)


  }
  # convert laz to las
  if (tool=="laz2las"){
    cat("\n:: converting laz to las..\n")
    # build command
    command <- laz2las
    command <- paste0(command, " -i ",lasFile)
    command <- paste0(command," -odir ",outpath)
    command <- paste0(command," -olas")

    # execute
    ret <- system(command,intern = FALSE, ignore.stderr = FALSE)
  }
  # convert laz to las
  if (tool=="lasclip"){
    cat("\n:: clipping las data..\n")
    # build command
    if(!is.null(cutExtent)){
    min_x<- cutExtent[1]
    min_y<- cutExtent[3]
    max_x<- cutExtent[2]
    max_y<- cutExtent[4]
    min_z<- cutSlice[1]
    max_z<- cutSlice[2]
    cat(paste(min_x,min_y),paste(max_x,min_y),paste(max_x,max_y),paste(min_x,max_y),paste(min_x,min_y),file =paste0(path_run,"tmp.txt") ,sep = "\n")

    command <- lasclip
    command <- paste0(command, " -i ",lasFile)
    #command <- paste0(command, "-keep_xyz ",min_x," ",min_y," ", min_z, " ", max_x," ", max_y," ", max_z)
    command <- paste0(command," -poly ",paste0(path_run,"tmp.txt"))

    command <- paste0(command," -olas")

    # execute
    ret <- system(command,intern = FALSE, ignore.stderr = FALSE)
    }  else {
      cat("\n::: no extent or slice provided....")
    }
  }
  # classify ground points (lastools)"
  if (tool == "lasground"){
    cat(":: classify ground points (lastools) ...\n")

    # build command
    command <- las2dem
    command <- paste0(command, " -i ",lasFile)
    command <- paste0(command," -all_returns ")
    command <- paste0(command," -bulge ", bulge)
    command <- paste0(command," -skip_files")
    command <- paste0(command," -step ", step)
    command <- paste0(command," -sub ", sub)
    command <- paste0(command," -odix g -o",extFN)
    command <- paste0(command," -cores ",cores)

    # execute
    ret <- system(command,intern = FALSE, ignore.stderr = FALSE)
  }
  # create lastools  DTM
  if (tool == "las2dem"){
    # build command
    command <- las2dem
    command <- paste0(command, " -i ",lasFile)
    command <- paste0(command," -keep_class 2 ")
    command <- paste0(command," -extra_pass ")
    command <- paste0(command," -step ",gridSize)
    command <- paste0(command," -ocut 3 ")
    command <- paste0(command," -odix _dtm ")
    command <- paste0(command," -otif ")
    command <- paste0(command," -odir ",outpath)
    command <- paste0(command," -cores ",cores)
    # execute
    ret <- system(command,intern = FALSE, ignore.stderr = FALSE)

  }
  if (tool == "las2txt"){
    cat(" export las to xyz - this may take a while...\n")
    # build command
    command <- las2txt
    command <- paste0(command, " -i ",lasFile)
    command <- paste0(command," -parse xyzrRGB ")
    command <- paste0(command," -sep komma ")

    # execute
    ret <- system(command,intern = FALSE, ignore.stderr = FALSE)
  }
  if (tool == "txt2las"){
    cat(" export  xyz to las - this may take a while...\n")
    # build command
    command <- txt2las
    command <- paste0(command, " -i ",lasFile)
    command <- paste0(command," -parse xyzrRGB ")
    command <- paste0(command," -o out.las ")
    command <- paste0(command," -sep komma ")

    # execute
    ret <- system(command,intern = FALSE, ignore.stderr = FALSE)
  }
  #txt2las -i lidar.zip -o lidar.laz -parse ssxyz -utm 14T
  if (tool == "getSpacing"){
    command <- lasinfo
    command <- paste0(command," -i ",lasFile)
    command <- paste0(command," -cd ")
    command <- paste0(command," -stdout ")
    # execute
    ret <- system(command,intern = TRUE)
    spatial_params<- list()
    tmp <- grep(pattern = "spacing: all returns", ret, value = TRUE)
    tmp <- unlist(strsplit(tmp, "spacing: all returns"))
    tmp <- unlist(strsplit(stringr::str_trim(tmp[2]), " "))
    return(tmp[1])
  }

  if (tool == "lasinfo"){
    command <- lasinfo
    command <- paste0(command," -i ",lasFile)
    command <- paste0(command," -no_check ")
    command <- paste0(command," -stdout ")
    # execute
    ret <- system(command,intern = TRUE)

    # make a minimal check on the min max spatial extension
    spatial_params<- list()
    tmp <- grep(pattern = "min x y z", ret, value = TRUE)
    tmp <- unlist(strsplit(tmp, ":"))
    tmp <- unlist(strsplit(stringr::str_trim(tmp[2]), " "))
    spatial_params[1] <- tmp[1]
    spatial_params[2] <- tmp[2]
    tmp <- grep(pattern = "max x y z", ret, value = TRUE)
    tmp <- unlist(strsplit(tmp, ":"))
    tmp <- unlist(strsplit(stringr::str_trim(tmp[2]), " "))
    spatial_params[3] <- tmp[1]
    spatial_params[4] <- tmp[2]
    tmp <- spatial_params %in% c("0.00", "-Inf", "Inf", "NAN")

    # try to correct it
    if (TRUE %in% tmp){
      if ((as.numeric(spatial_params[1]) <= as.numeric("0.00")) || (as.numeric(spatial_params[2])) <= as.numeric("0.00") ){
        spatial_params[1] <-as.numeric(spatial_params[3]) - 999.99
        spatial_params[2] <- as.numeric(spatial_params[4]) -999.99
      }
      if ((as.numeric(spatial_params[3]) <= as.numeric("0.00")) || (as.numeric(spatial_params[4])) <= as.numeric("0.00") ){
        spatial_params[3] <-as.numeric(spatial_params[1]) + 999.99
        spatial_params[4] <- as.numeric(spatial_params[2]) +999.99
      }
      lastool(tool = "lasrepair", lasFile = lasFile, xoff= spatial_params[1],yoff=spatial_params[2])
      cat("\n::: NOTE: Something wrong with the las extent tried to repair...")
    }
    return(unlist(spatial_params))}

  # rescales the file to a broader numeric scale
  if (tool == "rescale"){

    command <- lasthin
    command <- paste0(command," -i ",lasFile)
    command <- paste0(command," -rescale 0.01 0.01 0.01 ")
    command <- paste0(command," -auto_reoffset ")
    command <- paste0(command," -o ",dirname(lasFile), "/s_",basename(lasFile))
    # execute
    ret <- system(command,intern = FALSE, ignore.stderr = FALSE)

  }
  # rewrite las file and header according to a new offset
  if (tool == "lasrepair"){
    # build command
    command <- lasrepair
    command <- paste0(command, " -i ",lasFile)
    command <- paste0(command," -olas ")
    command <- paste0(command," -keep_tile", xoff, " ",yoff," 10000 ")
    # execute
    ret <- system(command,intern = FALSE, ignore.stderr = FALSE)
    # remove and rename
    file.remove(lasFile)
    file.rename(from = paste0(tools::file_path_sans_ext(lasFile),"_1.las"),to = lasFile)
  }
  # fixes the multi-coverage of flightlines as described https://rapidlasso.com/lastools/lasoverage/
  if (tool=="lasoverage") {
    # calculate spacing with lasinfo
    if (is.null(stepoverlap)) stepoverlap <- as.numeric(lastool(tool = "getSpacing", lasFile= lasFile)) * 2
    # build command
    command <- lasoverage
    command <- paste0(command, " -i ",lasFile)
    command <- paste0(command," -step ", stepoverlap)
    command <- paste0(command," -remove_overage ")
    command <- paste0(command," -o ", dirname(lasFile), "/o_",basename(lasFile))
    # execute
    ret <- system(command,intern = FALSE,ignore.stderr = FALSE)
  }
}
