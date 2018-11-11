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

#' @examples
#' \dontrun{
#'
#' ##- required packages
#' require(uavRst)
#' setwd(tempdir())
#' ##- set locale
#' tmp<-Sys.setlocale('LC_ALL','C')
#' ##- get some typical data as provided by the authority
#' utils::download.file(url="http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip",
#'                      destfile="testdata.zip")
#' unzip("testdata.zip",junkpaths = TRUE,overwrite = TRUE)

#' ##- original color space
#' raster::plotRGB(raster::stack("4490600_5321400.tif"))

#' ##- change colorspace from RGB to HSI
#' r2 <- colorspace(input="4490600_5321400.tif",colorspace="HSI")
#' 
#' ##- visualize it
#' raster::plotRGB(r2)
#' 
#' ##- reset locale
#' tmp<-Sys.setlocale(category = "LC_ALL", locale = "de_DE.-8")
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
    if (!is.na(pr@projargs)) {
    ex<-raster::extent(raster::raster(input))  
    r2<-raster::stack(outName)
    raster::projection(r2) <- pr
    raster::extent(r2) <-ex}
    else {
    r2<-raster::stack(outName)
    }
    if (retRaster) retStack[[paste0(tools::file_path_sans_ext(basename(outName)))]]<-assign(paste0(tools::file_path_sans_ext(basename(outName))),r2)
  }
  return(retStack[[1]])
}

