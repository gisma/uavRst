#' lasR Using a bit of lastools from R
#'
#'@description
#' using lasLib from R. returns lasR basically returns a DTM
#'
#'@author Chris Reudenbach
#'@param tool default is \code{lasinfo}   additionally xou may choose las2las, lasmerge, lasground_new, las2dem, las2txt
#'@param lasFile  default is \code{NULL} path  to the laz/las file(s)
#'@param gisdbase_path default is \code{NULL} root directory of the project. NOTE the function creates two subfolder named \code{run} and \code{output}
#'@param grid_size  resolution of the DTM raster
#'@param path_lastools directory for the windows lastools
#'@param thin_with_grid default 0.5 meter. Grid stepsize for data thinning
#'@param keep_class default is 2. Default ground class of las/laz conform data
#'@param bulge  default is 1.5. 'A parameter to filter spikes it is set to a step_size/10 and then clamped into the range from 1.0 to 2.0
#'@param step_size  default is 25 meter. LAStools key words if \code{city},\code{town},\code{metro},\code{nature},\code{wilderness} or experiment with free values
#'@param sub_size = "8", default is 8 meter. LAStools key words if \code{extra_coarse},\code{coarse},\code{fine},\code{extra_fine},\code{ultra_fine},\code{hyper_fine} or experiment with free values
#'@param cores number of cores that will be used
#'@param proj4  default is EPSG 32632 any valid proj4 string that is assumingly the correct one


#'
#'
#'@export
#'
#'@examples
#'\dontrun{
#' lasR(lasFile =  "~/path/to/lasdata",
#'        gisdbase_path = "~/temp5",
#'        thin_with_grid = "0.5",
#'        level_max = "5" ,
#'        grid_size = "0.5")
#'}

lasTool <- function(  tool="lasinfo",
                      lasFile = NULL,
                      thin_with_grid = "1.0",
                      keep_class = "2",
                      bulge = "1.5",
                      step_size = "city",
                      sub_size = "ultra_fine",
                      grid_size = "1.0",
                      proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs",
                      rscale= "0.01 0.01 0.01",
                      stepoverlap=2,
                      cores = "2") {
  


  # some basic checks
  if (is.null(lasFile)) stop("no directory containing las/laz files provided...\n")
  lasFile <- path.expand(lasFile)

  # if not provided take the onboard laslib


    if (Sys.info()["sysname"] == "Windows") {
      cmd <- path_lastools <- paste(system.file(package = "uavRst"), "LAStools", sep="/")
    } else {
      cmd <- paste("wine ",path_lastools <- paste(system.file(package = "uavRst"), "LAStools",  sep="/"))
    }





  # create cmd strings
  lasinfo       <- paste(cmd,"lasinfo-cli.exe",sep = "/")
  las2las       <- paste(cmd,"las2las-cli.exe",sep = "/")
  lasmerge      <- paste(cmd,"lasmerge-cli.exe",sep = "/")
  lasground_new <- paste(cmd,"lasground_new-cli.exe",sep = "/")
  las2dem       <- paste(cmd,"las2dem-cli.exe",sep = "/")
  las2txt       <- paste(cmd,"las2txt-cli.exe",sep = "/")
  lasoverage    <- paste(cmd,"lasoverage.exe",sep = "/")

  # check las / laz files laz will be preferred
  lasFileNames <- list.files(pattern = "[.]las$", path = lasFile, full.names = TRUE)
  lazFileNames <- list.files(pattern = "[.]laz$", path = lasFile, full.names = TRUE)
  #if (length(lazFileNames) > 0 ) extFN <- substr(extension(basename(lazFileNames[1])),2,4)
  #else if (length(lasFileNames) > 0) extFN <- substr(extension(basename(lasFileNames[1])),2,4)
  #else stop("no valid las or laz files found...\n")

  #sp_param <- uavRst:::getSpatialLASInfo(lasFile)

  # map the las code words
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



  # merge all files
  if (tool == "lasmerge"){
    cat("\nNOTE: You are dealing with a huge UAV generated point cloud data set.\n      so take time and keep relaxed... :-)\n")

    ret <- system(paste0(lasmerge,
                         " -i ",lasFile,"/*.las",
                         " -olas",
                         " -o ",path_run,"full_point_cloud.las"),
                  intern = TRUE,
                  ignore.stderr = TRUE
    )
    # get extent of merged file
    sp_param <- getSpatialLASInfo(lasinfo,paste0(path_run,"full_point_cloud.las"))

    # add proj4 string manually
    sp_param[5] <- proj4
  }

  if (tool=="las2las"){
    ### reduce data amount
    cat("\n:: converting laz to las..\n")
    ret <- system(paste0(las2las,
                         " -i ",lasFile,
#                         " -odix  ",
                         " -odir ",path_run,
                         " -o","las",
                         " -keep_class ",keep_class
#                         " -thin_with_grid ",thin_with_grid
),
                  intern = TRUE,
                  ignore.stderr = TRUE
    )
  }

  #### starting lastools classification

  if (tool == "lasground"){
    cat(":: classify ground points (LAStools) ...\n")
    ret <- system(paste0(lasground_new,
                         " -i ",lasFile,
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
  }
  if (tool == "las2dem"){
    # create lastools  DTM
    ret <- system(paste0(las2dem,
                         " -i ",lasFile,
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
  }
  if (tool == "las2txt"){
     cat(" export las to xyz - this may take a while...\n")
    ret <- system(paste0(las2txt,
                         " -i ",lasFile,
                         " -parse xyzrRGB",
                         " -sep komma"),
                  intern = TRUE,
                  ignore.stderr = TRUE
    )
  }
  if (tool == "lasinfo"){
    ret <- system(paste0(lasinfo,
                         " -i ",lasFile,
                         " -no_check  -stdout"),intern = TRUE)
    #paste0("wine ",fun,"LASTools/lasinfo-cli.exe ")
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
    #spatial_params[5] <- grep(pattern = "+proj", ret, value = TRUE)

    return(unlist(spatial_params))}

   if (tool == "rescale"){
  ret <- system(paste0(las2las,
                       " -i ",lasFile,
                       " -rescale 0.01 0.01 0.01 ",
                       " -auto_reoffset " ,
                       " -o ",dirname(lasFile), "/s_",basename(lasFile)),
                intern = TRUE,
                ignore.stderr = TRUE
  )

   }

  if (tool=="lasoverage") {
    ret <- system(paste0(lasoverage,
                         " -i ",lasFile,
                         " -step ", stepoverlap,
                         " -o ", dirname(lasFile), "/o_",basename(lasFile)),
                  intern = FALSE,
                  ignore.stderr = FALSE)
    }
}



getSpatialLASInfo <- function(lasinfo,lasFN){
  
  #ret <- linkGRASS7(spatial_params = c(-180,-90,180,90,"+proj=longlat +datum=WGS84 +no_defs"))
  #ret <- rgrass7::execGRASS("r.in.lidar",
  #                          flags = c("p"),
  #                          input=paste0(lasFN),
  #                          intern=TRUE)
  ret <- system(paste0(lasinfo,
                       " -i ",lasFN,
                       " -no_check  -stdout"),intern = TRUE)
  
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
  #spatial_params[5] <- grep(pattern = "+proj", ret, value = TRUE)
  
  return(unlist(spatial_params))
}

# 1973  history
# 1977  wine lascanopy.exe -i /home/creu/lehre/msc/active/msc-2017/data/gis/input/*.las  -dns -gap -b 50 75 -o stands.csv
# 1978  wine lasinfo.exe /home/creu/lehre/msc/active/msc-2017/data/gis/input/U4775630.las -histo point_source 1
# 1979  las2las -i /home/creu/lehre/msc/active/msc-2017/data/gis/input/U4775630.las -keep_point_source 314 -o U4775630_314.las
# 1980  wine las2las.exe -i /home/creu/lehre/msc/active/msc-2017/data/gis/input/U4775630.las -keep_point_source 314 -o U4775630_314.las
# 1981  wine lasoverage.exe -i /home/creu/lehre/msc/active/msc-2017/data/gis/input/U4775630.las -step 2 -o tile_overage.laz
# 1982  wine lasoverage.exe -i /home/creu/lehre/msc/active/msc-2017/data/gis/input/U4775630.las -step 2 -o U4775630_clean.las
# 1983  history
#
#
