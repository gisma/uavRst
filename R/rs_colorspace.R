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
#' # get temporary runtime folder
#'   setwd(tempdir())
#'   
#' # download some typical data as provided by the authority
#'  url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#'  res <- curl::curl_download(url, "testdata.zip")
#'  unzip(res,junkpaths = TRUE,overwrite = TRUE)
#'  
#' # change colorspace from RGB to CIElab  
#'  colorspace(input="4490600_5321400.tif",colorspace="CIELab")
#'  
#' }

colorspace<- function(input=NULL,
                      colorspace =c("CIELab","CMY","Gray","HCL","HSB","HSI","Log","XYZ","YUV"),
                      compress="None",
                      verbose=FALSE,
                      retRaster=TRUE){
  
  
  retStack<-list()
  #convert 2017_05_20_RGB_DEFS17_16_OrthoMosaic.tif -colorspace cmyk -compress LZW to.tif
  
  #if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (colMod in colorspace) {
    
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
    
    if (retRaster) retStack[[band]]<-assign(paste0(tools::file_path_sans_ext(basename(outName)),"band_",band),raster::stack(outName))
  }
  return(retStack)
}

