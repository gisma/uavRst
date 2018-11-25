
#' Create a Digital Terrain Model from UAV generated point clouds by minimum altitude sampling (multiple resolutions of sampling gridsize)
#'
#'@description
#' Create a Digital Terrain Model from a high density point cloud as typically derived by an optical UAV retrieval. Due to the poor estimation of ground points
#' a minimum samopling approach is applied. It retrieves on analyzing multiple sampling resolutions gridsizes the ground minimum values and interpolates on these samples a surface grid with a higher target
#' resolution. this is a kind of an try and error process and provides fairly good results if the point cloud shows at least some real surface points on a not to coarse grid.
#'
#'@author Chris Reudenbach, Finn Möller
#'
#'@param laspcFile  character. default is \code{NULL} path  to the laz/las file(s)
#'@param gisdbasePath character. default is \code{NULL} root directory of the project. NOTE the function creates two subfolder named \code{run} and \code{output}

#'@param sampleMethod  character. sampling method of r.in.lidar Statistic to use for raster values Options: n, min, max, range, sum, mean, stddev, variance, coeff_var, median, percentile, skewness, trimmean Default: mean
#'@param sampleGridSize  list of integer values for the chooses moving window resolution for optimize Ground model default is c(150,50,5)
#'@param sw_range numeric multiplicator for the accepted percentage difference of two search windows height values range typically [0,1] . Example z value of  sampleGridSize[100] - sampleGridSize[50] = 50  =>    50 *0.2 = 10. So 10 meters are acceped as a valid value.
## sprich innerhalb des 100x100 Suchfensters lasse ich zu das Punkte
# die 10 meter höher als der referenzpunkt liegen übernommen werden.
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
#'@export
#'@examples
#'\dontrun{
#'
#' require(uavRst)
#' require(link2GI)
#'
#' # create and check the links to the GI software
#' giLinks<-list()
#' giLinks$grass<-link2GI::linkGRASS7(returnPaths = TRUE)
#' if (giLinks$grass$exist) {
#'
#' # get the data
#' utils::download.file(url="https://github.com/gisma/gismaData/raw/master/uavRst/data/lidar.las",
#'                      destfile="lasdata.las")
#'
#' # create 2D point cloud DTM
#' dtm3 <- pc_2D_mdtm(laspcFile = "lasdata.las",
#'                    gisdbasePath = tempdir(),
#'                    tension = 20 ,
#'                    targetGridSize = 0.5,
#'                    sampleGridSize = c(100, 50, 25),
#'                    giLinks = giLinks)
#'  raster::plot(dtm3)
#'}
#'}
pc_2D_mdtm<- function(laspcFile = NULL,
                       gisdbasePath = NULL,
                       grassVersion=1,
                       searchPath =NULL,
                       tension = 20 ,
                       sampleMethod="min",
                       cutExtent = NULL,
                       sampleGridSize = c(100,50,25),
                       sw_range = 0.2,
                       targetGridSize = 0.1,
                       splineThresGridSize = 0.5,
                       projFolder = NULL,
                       proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84",
                       giLinks =giLinks,
                       verbose = FALSE) {

  if (!exists("path_run")) path_run = tempdir()
  ## Bis Zeile 69 ist das eins zu eins von dir

  gdal <- link2GI::linkGDAL()
  if (is.null(searchPath)){
    if(Sys.info()["sysname"]=="Windows") searchPath="C:"
    else searchPath <- "/usr"}
  # if (is.null(giLinks)){
  #   giLinks <- linkAll()
  # }

  #saga <- giLinks$saga
  #otb <- giLinks$otb
  #sagaCmd<-saga$sagaCmd
  #path_OTB <- otb$pathOTB

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



  if (!file.exists(file.path(R.utils::getAbsolutePath(path_run),name)))
  {  cat(":: create copy of the las file at the working directory... \n")
  file.copy(from = laspcFile,
            to = file.path(R.utils::getAbsolutePath(path_run),name),
            overwrite = TRUE)}



  las<-rlas::read.lasheader(laspcFile)
  sp_param <- c(as.character(las$`Min X`),as.character(las$`Min Y`),as.character(las$`Max X`),as.character(las$`Max Y`))
  # rename output file according to the extent
  fn<- paste(sp_param ,collapse=" ")
  tmp <- gsub(paste(sp_param ,collapse=" "),pattern = " ",replacement = "_")
  name<-paste0(gsub(tmp,pattern = "[.]",replacement = "_"),".las")
  file.rename(from =file.path(R.utils::getAbsolutePath(path_run),basename(laspcFile)),
              to = file.path(R.utils::getAbsolutePath(path_run),name))

  # copy it to the output folder
  sp_param[5] <- proj4


  ## The first step creates one for each selected resolution vectormap and saves the in the "vdtms" list. 
  #  Creating the vectormap is on your way, say r.in.lidar to read (create
  #  of a minimum grid) and then r.to.vect to convert to a points / vectormap.

  vdtms <- list()

  for (i in  sampleGridSize) {
    link2GI::linkGRASS7(gisdbase = gisdbasePath,
                        location = "pc2D_dtm",
                        spatial_params = sp_param,
                        resolution = i,
                        returnPaths = FALSE,
                        quiet = TRUE,
                        ver_select = grassVersion,
                        search_path = searchPath)

    # cat(":: sampling minimum altitudes using : ", sampleGridSize ,"meter grid size\n")
#    if (!grepl(system("g.extension -l",ignore.stdout = TRUE),pattern = "r.in.lidar"))
      # ret <- rgrass7::execGRASS("r.in.pdal",
      #                           flags  = c("overwrite","quiet"),
      #                           input  = paste0(path_run,name),
      #                           output = paste0("dem",i),
      #                           method = "min",
      #                           proj_in = sp_param[5],
      #                           resolution = as.numeric(i),
      #                           intern = TRUE,
      #                           ignore.stderr = TRUE
      # )
    # else
    ret <- rgrass7::execGRASS("r.in.lidar",
                              flags  = c("overwrite","quiet","o","e","n"),
                              input  = file.path(R.utils::getAbsolutePath(path_run),name),
                              output = paste0("dem",i),
                              method = "min",
                              resolution = i,
                              # trim = 49,
                              intern = TRUE,
                              ignore.stderr = TRUE
    )

    ret <- rgrass7::execGRASS("r.to.vect",
                              flags  = c("overwrite","quiet","v", "z"),
                              input  = paste0("dem",i),
                              output = paste0("vdtm",i),
                              type = "point",
                              intern = TRUE,
                              ignore.stderr = TRUE
    )

    ## This will create the vectormaps in the vdtms list via
    ## rgrass7 :: readVECT stored to make them available in R and the
    ## Grasslogy leave.
    vdtms[paste0("vdtm",i)] <- rgrass7::readVECT(paste0("vdtm",i))
  }

  ## The next step goes through each Vectormap point by point and
  # Search within a buffer for points that meet all criteria and
  #then be selected as "keeps". The respective Vectormap is in round one
  #the vectormap # of the largest search window, and from round two the vector map of the
  # "keeps" points. create empty keeps vector the new from round to round
  # is filled vdtms_edit <- vdtms
  keeps <- c()
  nofWinSize<-length(sampleGridSize)-1
  if (nofWinSize < 1) nofWinSize<-1
  for (k in c(1:(nofWinSize))) {
    # here it is determined that he all search window sizes except the last
    # goes through.
    
    ## the if switch ensures only that he keeps in the first round
    ## is still empty the original Vectormap of the largest search window as
    ## uses "vdtm_run". As soon as stops is filled these points will be considered as
    ## "vdtm_run" used.
    ##
    if (length(keeps) > 1) {
      vdtm_run <- keeps
    } else if (length(keeps) < 1) {
      vdtm_run <-vdtms[paste0("vdtm",sampleGridSize[k] )]
      vdtm_run <- vdtm_run[[1]]
    }


    keeps <- vdtm_run[1,]
    ## here, keeps is filled with the first dot from the first vector map
    ## so that the later rbing is not zero and flies out. There you go
    ## I have time to come up with something smarter again but there
    ## presumably no study area so close to the edge I have that for the time being
    ## ignored.

    for (o in c(1:length(vdtm_run))) {
      ## every point of the intervening vdtm_run should be played through

      buffer <- rgeos::gBuffer(vdtm_run[o,], capStyle= "SQUARE", width = sampleGridSize[k]/2)
      ## creates rectangular buffer per point each
      ## is half the size of the search window size in the corresponding pass
      ## Since I will also check again time if you make the variable
      ## and as a function parameter. Let's see how that affects.
      vdtm_run_match <- vdtms[paste0("vdtm",sampleGridSize[k+1] )]
      ## vtdm_run_match selects the vectormap of the next smaller search window.

      hits <- which(rgeos::gContains(buffer,vdtm_run_match[[1]] , byid=TRUE))
      ## And now all points of the next smaller point cloud will be in the buffer
      ## selected and saved as "hits".

      # the following line in single steps:
      # In "vdtm_run_match [[1]]", all the points should be the following
      # Criterion match selected and saved as keeps_new
      # become:
      # 1) The height differences between the points of the different resolutions are determined.
      # hits [which (abs (vdtm_run [o,] @ coords [, 3] - vdtm_run_match [[1]] [c (hits),] @ coords [, 3])
      # 2) If height differences less than a 0.2 * difference of the two search windows
      # then they are taken
      # <((sampleGridSize [k] - sampleGridSize [k + 1]) / 5))
      ##
      ### Example: sampleGridSize [100] - sampleGridSize [50] = 50
      ## 50/5 = 10
      ## speak within the 100x100 search window I leave to the points
      # which are 10 meters higher than the reference point.


      keeps_new <- vdtm_run_match[[1]][hits[which( abs(vdtm_run[o,]@coords[,3] - vdtm_run_match[[1]][c(hits),]@coords[,3]) < ((sampleGridSize[k]- sampleGridSize[k+1])*sw_range))],]


      keeps <-  rbind(keeps, keeps_new)
      ## Then old ones are connected with new keeps and off in the
      # next pass.
      ## Ggf I would also test here if it is more meaningful
      # throw out old keeps and just take over the new ones.
    }
  }

  # rm(keeps)
  # par(mfcol=c(1,5))
  # plot(vdtm_run[[1]],  col="black")
  # plot(vdtm_run[[1]], col="black")
  # plot(vdtm_run_match[[1]], col= "lightgrey", add=TRUE)
  # plot(vdtm_run[[1]], col="black")
  # plot(vdtm_run_match[[1]], col= "lightgrey", add=TRUE)
  # plot(keeps, add=TRUE, col="red")
  # plot(keeps, col="red")
  # raster::plot(dtm, axes=FALSE, box=FALSE, legend= FALSE)
  # plot(keeps_new, add=TRUE)


  # if that's done for every window size the last vector map
  # stored and loaded via "v.in.ascii" back into the GRASS environment.
  utils::write.csv(data.frame(as.numeric(keeps@coords[,1]),
                              as.numeric(keeps@coords[,2]),
                              as.numeric(keeps@coords[,3])), 
                   file.path(R.utils::getAbsolutePath(path_run), "vec"), 
                   sep = " ", 
                   col.names = FALSE, 
                   row.names = FALSE)
  ret <- rgrass7::execGRASS("v.in.ascii",
                            flags  = c("quiet", "overwrite"),
                            input= file.path(R.utils::getAbsolutePath(path_run), "vec"),
                            output= "ascii",
                            separator = "comma",
                            skip = 1,
                            z=3,
                            intern = TRUE,
                            ignore.stderr = TRUE
  )

################################################# old algo ###################################

  # set regio with new gridsize
  if (targetGridSize < splineThresGridSize) {
    oldtgs<-targetGridSize
    targetGridSize <- splineThresGridSize
    cat(":: target grid size is", targetGridSize ," => setting grid size to: ",splineThresGridSize,"\n")
  } else {
    splineThresGridSize <- targetGridSize
    oldtgs<-targetGridSize
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
                            input  = "ascii",
                            elevation = "tdtm",
                            tension = tension,
                            intern = TRUE,
                            ignore.stderr = TRUE
  )




  # apply mask for input data area
  ret <- rgrass7::execGRASS("r.mapcalc",
                            flags  = c("overwrite","quiet"),
                            expression= paste0('"dtm = if(',paste0("dem",min(sampleGridSize)),' > 0  ,tdtm,0)"'),
                            intern = FALSE,
                            ignore.stderr = TRUE
  )



  dtm0<- raster::writeRaster(raster::raster(rgrass7::readRAST("dtm")),file.path(R.utils::getAbsolutePath(path_run),"dtm0"), overwrite=TRUE,format="GTiff")
  if (oldtgs < splineThresGridSize) {
    cat(":: Resample to a grid size of: ", targetGridSize ,"\n")
    res<-gdalUtils::gdalwarp(srcfile = file.path(R.utils::getAbsolutePath(path_run),"dtm0.tif"),
                             dstfile = file.path(R.utils::getAbsolutePath(path_run),"dtm.tif"),
                             tr=c(oldtgs,oldtgs),
                             r="bilinear",
                             overwrite = TRUE,
                             multi = TRUE)
    dtm <- raster::raster(file.path(R.utils::getAbsolutePath(path_run),"dtm.tif"))
  } else {
    dtm <- raster::raster(file.path(R.utils::getAbsolutePath(path_run),"dtm0.tif"))
  }

  if (!verbose)  {
    Sys.setenv("GRASS_VERBOSE"=GV)
    rgrass7::set.ignore.stderrOption(ois)
  }
  #return(list(dtm,dtmA,paste0(fn,".",extFN)))
  return(dtm)
}

