
#' Calculates color space transformations of the images
#' 
#' @note Imagemagick is used for the calculation of the transformations. Please provide a valid Tiff file  
#' @param input GeoTiff containing RGB data channels
#' @param colorspace argument to determine colorspace see \href{https://www.systutorials.com/qa/1954/how-to-convert-tiff-images-from-rgb-color-cmyk-color-on-linux}{transform RGB colorspace}
#' @param compress compression type
#' @param verbose be quiet
#' @export imageMagickconvert
#' @examples 
#' #' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' imageMagickconvert(input=paste0(getwd(),"4490600_5321400.tif"),colorspace="CIELab")
#' }

imageMagickconvert<- function(input=NULL,
                        colorspace =c("CIELab","CMY","Gray","HCL","HSB","HSI","Log","XYZ","YUV"),
                        compress="None",
                        verbose=FALSE){
  
  
  retStack<-list()
  #convert 2017_05_20_RGB_DEFS17_16_OrthoMosaic.tif -colorspace cmyk -compress LZW to.tif
  
  #if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (colMod in colorspace) {
    
    outName<-paste0(colMod,"_",basename(input))
    
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
    
    #if (retRaster) retStack[[band]]<-assign(paste0(tools::file_path_sans_ext(basename(outName)),"band_",band),raster::stack(outName))
  }
  #return(retStack)
}

