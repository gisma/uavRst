#' imagemagick based function to transform the RGB color space into another
#'
#' @description Imagemagick is used for the calculation of the transformations. Please provide a tif file
#' @note you need to install imagemagick on your system, please look at see \href{https://www.imagemagick.org/script/download.php}{imagemagick download section}.
#'
#' @param input character. name of a (Geo)Tiff containing RGB data channels
#' @param colorspace character. For a list of argument to determine colorspace see \href{https://www.imagemagick.org/script/command-line-options.php#colorspace}{colorspace manual}
#' @param compress character. compression type depending on the imagemagick version the choices are: None, BZip, Fax, Group4, JPEG, JPEG2000, Lossless, LZW, RLE or Zip.
#' @param depth numeric. color space depth in bit default is 8 
#' @param retRaster logical if true a rasterstack is returned
#' @param verbose be quiet
#' @export colorspace
#' @importFrom gdalUtils ogr2ogr
#' @importFrom gdalUtils gdal_translate
#' @importFrom gdalUtils gdalwarp
#' @importFrom gdalUtils gdalinfo
#' @examples
#' \dontrun{
#'
#' ##- required packages
#' require(uavRst)
#' setwd(tempdir())
#' data("pacman")
#' red<- trunc(pacman * 0.5)
#' green<- trunc(pacman * 0.8)
#' blue<- trunc(pacman * 0.1)
#' b <- raster::brick(pacman)
#' names(b)<-c("red","green","blue")
#' raster::writeRaster(b,"pacman2.tif",overwrite=TRUE)
#'
#' ##- change colorspace from RGB to HSI
#'  r2 <- colorspace(input=paste0("pacman2.tif"),colorspace="HSI")
#'
#' ##- visualize it
#' raster::plotRGB(r2)
#' ##+}




colorspace<- function(input=NULL,
                      colorspace =c("CIELab","CMY","Gray","HCL","HSB","HSI","Log","XYZ","YUV"),
                      compress="None",
                      depth = 8,
                      verbose=FALSE,
                      retRaster=TRUE){
  if (!exists("path_run")) path_run = paste0(getwd(),"/")

  #convert 2017_05_20_RGB_DEFS17_16_OrthoMosaic.tif -colorspace cmyk -compress LZW to.tif

  #if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (colMod in colorspace) {
    retStack<- list()
    outName<-paste0(path_run,colMod,"_",basename(input))

    command<-paste0("convert")
    command<-paste(command, input)
    command<-paste(command, " -colorspace ", colMod)
    command<-paste(command, " -compress ",compress)
    command<-paste(command, " -depth ", depth)
    command<-paste(command,  outName)

    if (verbose) {
      cat("\nrunning cmd:  ", command,"\n")
      system(command)}
    else{
      ret<-system(command,intern = TRUE,ignore.stdout = TRUE,ignore.stderr = TRUE)}

    
    pr<-raster::crs(raster::projection(raster::raster(input)))
    if (is.na(pr@projargs)) {
    ex<-raster::extent(raster::raster(input))  
    r2<-raster::stack(outName)
    raster::projection(r2) <- pr
    raster::extent(r2) <-ex}
    if (retRaster) retStack[[paste0(tools::file_path_sans_ext(basename(outName)))]]<-assign(paste0(tools::file_path_sans_ext(basename(outName))),r2)
  }
  return(retStack[[1]])
}

