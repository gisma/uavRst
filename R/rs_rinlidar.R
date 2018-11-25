#'@name r_in_lidar
#'@title wraps the r.in.lidar tool
#'
#'@description
#' simple wrapper for 'r.in.lidar' to calculate LiDAR derived raster grids. It creates a \code{raster*} object.
#' @seealso \href{https://grass.osgeo.org/grass70/manuals/r.in.lidar.html}{r.in.lidar help}
#'
#'@author Chris Reudenbach
#'
#'@param r.in.lidar [-penosgijdv]
#'@param input input
#'@param output output
#'@param file file
#'@param method method
#'@param type type
#'@param base_raster base_raster
#'@param zrange zrange
#'@param zscale zscale
#'@param  intensity_range intensity_range
#'@param intensity_scale intensity_scale
#'@param percent percent
#'@param pth pth
#'@param trim trim
#'@param resolution resolution
#'@param return_filter return_filter
#'@param class_filter class filter
#'@param flags flags
#'@export
#'@examples
#'\dontrun{
#'require(uavRst)
#' ##- Straightforward example to generate a DTM 
#'     based on the class 2 minimum returns of a LiDAR file
#'     
#' # get a las file from the Spain authorithy (29.2 MB)
#' # Source: (https://b5m.gipuzkoa.eus/url5000/es/G_22485/PUBLI&consulta=HAZLIDAR)
#' utils::download.file(url="https://b5m.gipuzkoa.eus/lidar/MT13_08_au_las.zip",
#'                            destfile="MT13_08_au_las.zip", mode = "wb")
#' unzip("MT13_08_au_las.zip",junkpaths = TRUE,overwrite = TRUE)
#' 
#' ##- extract extent for setting up GRASS region
#' ext<-lastool(lasFile =  "MT13_08_au.las")
#' ## according to the meta data 
#' #  source: https://b5m.gipuzkoa.eus/web5000/en/csw/MT13_08_au_en 
#' #  the projection is EPSG25830. NOTE it is obligate to at the zero transformation params
#' proj4 = "+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#' 
#' ##- set up GRASS
#' result<-link2GI::linkGRASS7(spatial_params = c(ext[2],ext[1],ext[4],ext[3],proj4),
#'                              resolution = 5.0)
#'                              
#' ##- use the r.in.lidar tool to generate a pseudo surface model
#' r_in_lidar(input = "MT13_08_au.las",
#'            output = "dem",
#'            method = "min",
#'            resolution = 5,
#'            class_filter = 2)
#'            
#' ##- visualize it
#' raster::plot(raster::raster(rgrass7::readRAST("dem")))
#'                             
#'}


r_in_lidar<- function(input=NULL,
                      output=NULL,
                      file=NULL,
                      method=NULL,
                      type=NULL,
                      base_raster=NULL,
                      zrange=NULL,
                      zscale=NULL,
                      intensity_range=NULL,
                      intensity_scale=NULL,
                      percent=NULL,
                      pth=NULL,
                      trim=NULL,
                      resolution=NULL,
                      return_filter=NULL,
                      class_filter=NULL,
                      flags = c("e","n","overwrite","o")) {
  ### TODO some strange effects with zrange and file are passed by
  #stopifnot(!grepl(system("g.extension -l",ignore.stdout = TRUE),pattern = "r.in.lidar"),cat("NO r.in.lidar installed"))
  file=NULL
  input<-path.expand(input)
  if (!is.null(zrange)){
    rgrass7::execGRASS("r.in.lidar",
                       input = input,
                       output = output,
                       flags = flags,
                       resolution = resolution,
                       method = method,
                       base_raster = base_raster,
                       zrange = zrange,
                       class_filter =class_filter,
                       echoCmd=TRUE,
                       intern = FALSE,
                       ignore.stderr = FALSE)
  } else if (!is.null(return_filter)){
    rgrass7::execGRASS("r.in.lidar",
                       input = input,
                       output = output,
                       flags = flags,
                       resolution = resolution,
                       method = method,
                       return_filter =return_filter,
                       echoCmd=TRUE,
                       intern = FALSE,
                       ignore.stderr = FALSE)

  } else if (!is.null(class_filter)) {


    # (step 0) during r.in.lidar check if an error occurs
    m<-try(rgrass7::execGRASS("r.in.lidar",
                              input = input,
                              output = output,
                              flags = flags,
                              resolution = resolution,
                              method = method,
                              class_filter =class_filter,
                              echoCmd=FALSE,
                              intern = TRUE,
                              ignore.stderr = FALSE))

    if (class(m)=="try-error") {
      cat("\nno correct las extent - already tried to correct...\n")
      # cat("\nwork around for broken extent headers")
      # cat("\nif GRASS can not allocate a magical hugfe raster the following may help:")
      # cat("\n1) export las file to ASCII xyz -> gettting the real minx maxx miny maxy values by parsing")
      # cat("\n2) get the the extent from lasinfo tool")
      # cat("\n3) reset the GRASS region to this extent and resolution")
      # cat("\n4) use r.in.xyz to import the DEM data")
      # cat("\n5) resulting DEM raster is used further on as reference\n")
      #
      # cat("\nstep 1) ")
      # lastool(tool = "las2txt", lasFile = input)
      # cat("\nstep 2) get extent of the original las file")
      # ext<-lastool(lasFile = input)
      # xmin <-as.numeric(ext[3]) -999.99
      # ymin <- as.numeric(ext[4]) -999.99
      # ext<- c(xmin,ymin,ext[3],ext[4])
      #
      #
      # cat("\nstep 3) reset the region")
      # rgrass7::execGRASS('g.region',
      #                    flags = c('quiet','d'),
      #                    n = ext[4],
      #                    s = ext[2],
      #                    e = ext[3],
      #                    w = ext[1],
      #                    res = as.character(resolution))
      # cat("\nstep 4) import raster from xyz ASCII\n")
      # m<-try(rgrass7::execGRASS("r.in.xyz",
      #                           input = paste0(tools::file_path_sans_ext(input),".txt"),
      #                           output = output,
      #                           flags = c("i","overwrite"),
      #                           method = method,
      #                           separator="comma",
      #                           echoCmd=FALSE,
      #                           intern = TRUE,
      #                           ignore.stderr = TRUE))
    }
  }


  else {

    # create a list of arguments
    arguments    <- list(input, output, file, method, type, base_raster,zrange, zscale, intensity_range, intensity_scale,
                         percent, pth, trim, resolution, return_filter, class_filter)
    # create a list of corresponding keys
    argumentsKey <- list("input", "output", "file", "method", "type", "base_raster",
                         "zrange", "zscale", "intensity_range", "intensity_scale",
                         "percent", "pth", "trim", "resolution", "return_filter", "class_filter")
    charargs<-list( "input" , "output", "method" , "base_raster" ,"file")
    # paste together the r.lidar xommand
    command<-"'r.in.lidar' ,"
    # for all not NULL arguments do
    for (i in 1:length(arguments)) {
      if (!is.null(arguments[[i]])) {
        # for some arguments wr needs quotation
        if (argumentsKey[[i]] %in% charargs)
          command<-paste0(command,argumentsKey[[i]],"=",shQuote(arguments[[i]]),",")
        else
          command<-paste0(command,argumentsKey[[i]],"=",arguments[[i]],",")
      }
    }
    # now paste the flags
    command<-paste0(command,"flags=c(",paste(shQuote(flags),collapse = ","),")")
    # change the quotation to ""
    command<-gsub(x = command,pattern = "'",replacement = '"')
    # big trick evaluate this parsed text as an command
    eval(parse(text=paste0("rgrass7::execGRASS(",noquote(command),",intern = FALSE,ignore.stderr = FALSE)")))
  }
}






