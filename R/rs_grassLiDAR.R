#'@name r_in_lidar
#'@title wraps the r.in.lidar tool
#'
#'@description
#' Creates raster* objects from a LiDAR derived  point cloud based on regular las formatted data files
#'
#'@author Chris Reudenbach,Thomas Nauss, Jannis Gottwald
#'
#'@param r.in.lidar [-penosgijdv] 
#'@param input=name LAS input file LiDAR input files in LAS format (*.las or *.laz) 
#'@param output=name Name for output raster map 
#'@param file=name File containing names of LAS input files LiDAR input files in LAS format (*.las or *.laz) 
#'@param method=string Statistic to use for raster values Options: n, min, max, range, sum, mean, stddev, variance, coeff_var, median, percentile, skewness, trimmean Default: mean 
#'@param n: Number of points in cell 
#'@param min: Minimum value of point values in cell 
#'@param max: Maximum value of point values in cell 
#'@param range: Range of point values in cell 
#'@param sum: Sum of point values in cell 
#'@param mean: Mean (average) value of point values in cell 
#'@param stddev: Standard deviation of point values in cell 
#'@param variance: Variance of point values in cell 
#'@param coeff_var: Coefficient of variance of point values in cell 
#'@param median: Median value of point values in cell 
#'@param percentile: pth (nth) percentile of point values in cell 
#'@param skewness: Skewness of point values in cell 
#'@param trimmean: Trimmed mean of point values in cell 
#'@param type=string Type of raster map to be created Storage type for resultant raster map Options: CELL, FCELL, DCELL Default: FCELL 
#'@param CELL: Integer 
#'@param FCELL: Single precision floating point 
#'@param DCELL: Double precision floating point 
#'@param base_raster=name Subtract raster values from the Z coordinates The scale for Z is applied beforehand, the range filter for Z afterwards 
#'@param zrange=min,max Filter range for Z data (min,max) Applied after base_raster transformation step 
#'@param zscale=float Scale to apply to Z data  Default: 1.0 
#'@param intensity_range=min,max Filter range for intensity values (min,max) intensity_scale=float Scale to apply to intensity values Default: 1.0 
#'@param percent=integer Percent of map to keep in memory Options: 1-100 Default: 100 
#'@param pth=integer pth percentile of the values Options: 1-100 
#'@param trim=float  Discard given percentage of the smallest and largest values Discard <trim> percent of the smallest and <trim> percent of the largest observations Options: 0-50 
#'@param resolution=float Output raster resolution 
#'@param return_filter=string Only import points of selected return type If not specified, all points are imported Options: first, last, mid 
#'@param class_filter=integer[,integer,...] Only import points of selected class(es) Input is comma separated integers. If not specified, all points are imported. 
#'@param -p Print LAS file info and exit 
#'@param -e Use the extent of the input for the raster extent Set internally computational region extents based on the point cloud 
#'@param -n Set computation region to match the new raster map Set computation region to match the 2D extent and resolution of the newly created new raster map 
#'@param -o Override projection check (use current location's projection) Assume that the dataset has same projection as the current location 
#'@param -s Scan data file for extent then exit 
#'@param -g In scan mode, print using shell script style 
#'@param -i Use intensity values rather than Z values Uses intensity values everywhere as if they would be Z coordinates 
#'@param -j Use Z values for filtering, but intensity values for statistics 
#'@param -d Use base raster resolution instead of computational region For getting values from base raster, use its actual resolution instead of computational region resolution 
#'@param -v Use only valid points Points invalid according to APSRS LAS specification will be filtered out 
#'@examples
#'\dontrun{
#' # create a DEM based on the class 2 Minimum returns
#' require(curl)
#' # get a laz file from Mr. Isenburg
#' res <- curl::curl_download(url="http://www.cs.unc.edu/~isenburg/lastools/download/test/s1885565.laz",
#'                            destfile="test.laz",  quiet = TRUE, mode = "wb")
#' # convert it to las
#' lasTool(tool="las2las","test.laz")
#' # extract extension for setting up GRASS region
#' ext<-lasTool(lasDir = "test.las")
#' # set up GRASS
#' result<-link2GI::linkGRASS7(spatial_params = c(ext[2],ext[1],ext[4],ext[3],proj4),resolution = gridsize)
#' # use the r.in.lidar tool to generate a pseudo surface model
#' r_in_lidar(input = paste0(getwd(),"/test.las"),
#'            output = "testdem",
#'            method = "min",
#'            resolution = 10,
#'            class_filter = 2)
#'}
#'

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
  file=NULL
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
    

    # (step 0) during r.in.lidar check if an error occure
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
      cat("\nno correct las extent - try to correct...\n")
      cat("\nwork around for broken extent headers")
      cat("\nif GRASS can not allocate a magical hugfe raster the following may help:")
      cat("\n1) export las file to ASCII xyz -> gettting the real minx maxx miny maxy values by parsing")
      cat("\n2) get the the extent from lasinfo tool")
      cat("\n3) reset the GRASS region to this extent and resolution")
      cat("\n4) use r.in.xyz to import the DEM data")
      cat("\n5) resulting DEM raster is used further on as reference\n")
      
      cat("\nstep 1) ")
      lasTool(tool = "las2txt", lasDir = input)
      cat("\nstep 2) get extent of the original las file")
      ext<-lasTool(lasDir = paste0(gi_input, lasfiles[j]))
      cat("\nstep 3) reset the region")
      rgrass7::execGRASS('g.region',
                         flags = c('quiet','d'),
                         n = ext[4],
                         s = ext[2],
                         e = ext[3],
                         w = ext[1],
                         res = as.character(resolution))
      cat("\nstep 4) import raster from xyz ASCII\n")
      m<-try(rgrass7::execGRASS("r.in.xyz",
                                input = paste0(tools::file_path_sans_ext(input),".txt"),
                                output = output,
                                flags = c("i","overwrite"),
                                method = method,
                                separator="comma",
                                echoCmd=FALSE,
                                intern = TRUE,
                                ignore.stderr = TRUE))
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
        # for some arguments wr need quotation
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






