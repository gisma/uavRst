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
#'@return raster* object
#' @examples
#' \dontrun{
#'
#' ##- required packages
#' require(uavRst)
#' setwd(tempdir())
#' data("rgb")
#' raster::plotRGB(rgb)
#' fn<-file.path(tempdir(),"rgb.tif")
#' raster::writeRaster(rgb, 
#'                     filename=fn,
#'                     format="GTiff", 
#'                     overwrite=TRUE)

#' ##- original color space
#' raster::plotRGB(raster::stack("rgb.tif"))

#' ##- change colorspace from RGB to HSI
#' r2 <- colorspace(input="rgb.tif",colorspace="HSI")
#' 
#' ##- visualize it
#' raster::plotRGB(r2)
#' 
#' ##+}




colorspace<- function(input=NULL,
                      colorspace =c("cielab","CMY","Gray","HCL","HSB","HSI","Log","XYZ","YUV"),
                      compress="None",
                      depth = 8,
                      verbose=FALSE,
                      retRaster=TRUE){

  
  catOk   <- getCrayon()[[3]]
  #convert 2017_05_20_RGB_DEFS17_16_OrthoMosaic.tif -colorspace cmyk -compress LZW to.tif

  #if (is.null(channel)) channel<-seq(length(grep(system(paste0(gdal$path,'gdalinfo  ',input), intern = TRUE),pattern = "Band ",value = TRUE)))

  if (!exists("path_run")) path_run = tempdir()
  retStack<- list()
  for (colMod in colorspace) {
    outName<- file.path(R.utils::getAbsolutePath(path_run),paste0(colMod,"_",basename(input)))
    #outName<-file.path(R.utils::getAbsolutePath(dirname(input)),paste0(colMod,"_",basename(input)))

    command<-paste0("convert")
    command<-paste(command, input)
    command<-paste(command, " -colorspace ", colMod)
    command<-paste(command, " -compress ",compress)
    command<-paste(command, " -depth ", depth)
    command<-paste(command,  outName)
     
    if (verbose) {
      
      
      system(command)}
    else{
      ret<-system(command,intern = TRUE,ignore.stdout = TRUE,ignore.stderr = TRUE)}

    
    pr<-raster::crs(raster::projection(raster::raster(input)))
    if (!is.na(pr@projargs)) {
    ex<-raster::extent(raster::raster(input))  
    r2<-raster::stack(outName)
    raster::projection(r2) <- pr
    raster::extent(r2) <-ex}
    else {
    r2<-raster::stack(outName)
    }
    if (retRaster) retStack[[colMod ]]<-assign(paste0(tools::file_path_sans_ext(basename(outName))),r2)
  }
  return(retStack)
}

