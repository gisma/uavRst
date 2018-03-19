#' imagemagick based function to transform the RGB color space into another
#'
#' @description Imagemagick is used for the calculation of the transformations. Please provide a tif file
#' @note you need to install imagemagick on your system, please look at see \href{https://www.systutorials.com/qa/1954/how-to-convert-tiff-images-from-rgb-color-cmyk-color-on-linux}{transform RGB colorspace} seealso \href{https://www.imagemagick.org/script/download.php}{imagemagick download section}. 
#' 
#' @param input character. name of a (Geo)Tiff containing RGB data channels
#' @param colorspace character. argument to determine colorspace see \href{https://www.systutorials.com/qa/1954/how-to-convert-tiff-images-from-rgb-color-cmyk-color-on-linux}{transform RGB colorspace} seealso \href{https://www.imagemagick.org/script/command-line-options.php#colorspace}{colorspace manual}
#' @param compress character. compression type depending on the imagemagick version the choices are: None, BZip, Fax, Group4, JPEG, JPEG2000, Lossless, LZW, RLE or Zip.
#' @param retRaster logical if true a rasterstack is returned
#' @param verbose be quiet
#' @export colorspace
#' @examples
#' \dontrun{
#' 
#' # required packages
#'  require(uavRst)
#'  require(curl)
#'  
#' # project folder
#' projRootDir<-tempdir()
#' 
#' # create subfolders please mind that the pathes are exported as global variables
#'  paths<-link2GI::initProj(projRootDir = projRootDir,
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")
#' ## overide trailing backslash issue
#'  path_run<-ifelse(Sys.info()["sysname"]=="Windows", sub("/$", "",path_run),path_run)
#'  setwd(path_run)                                          
#'  unlink(paste0(path_run,"*"), force = TRUE
#'  
#'  url <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/tutorial.zip"
#'  res <- curl::curl_download(url, paste0(path_run,"tutorial.zip"))
#'  unzip(zipfile = res, exdir = path_run)
#'  
#' # change colorspace from RGB to CIElab  
#'  r2 <- colorspace(input=paste0(path_run,"rgb_1.tif"),colorspace="CIELab")
#'  raster::plotRGB(r2)
#' }




colorspace<- function(input=NULL,
                      colorspace =c("CIELab","CMY","Gray","HCL","HSB","HSI","Log","XYZ","YUV"),
                      compress="None",
                      verbose=FALSE,
                      retRaster=TRUE){
  
  
  
  #convert 2017_05_20_RGB_DEFS17_16_OrthoMosaic.tif -colorspace cmyk -compress LZW to.tif
  
  #if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (colMod in colorspace) {
    retStack<- list()
    outName<-paste0(path_run,colMod,"_",basename(input))
    
    command<-paste0("convert")
    command<-paste(command, input)
    command<-paste(command, " -colorspace ", colMod)
    command<-paste(command, " -compress ",compress)
    command<-paste(command,  outName)
    
    if (verbose) {
      cat("\nrunning cmd:  ", command,"\n")
      system(command)}
    else{
      ret<-system(command,intern = TRUE,ignore.stdout = TRUE,ignore.stderr = TRUE)}
    
    ex<-raster::extent(raster::raster(input))
    pr<-raster::crs(raster::projection(raster::raster(input)))
    r2<-raster::raster(outName)
    raster::projection(r2) <- pr
    raster::extent(r2) <-ex
    if (retRaster) retStack[[paste0(tools::file_path_sans_ext(basename(outName)))]]<-assign(paste0(tools::file_path_sans_ext(basename(outName))),r2)
  }
  return(retStack[[1]])
}

