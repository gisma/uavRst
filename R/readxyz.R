if (!isGeneric('readxyz')) {
  setGeneric('readxyz', function(x, ...)
    standardGeneric('readxyz'))
}
#' Read and Convert xyz DEM/DSM Data as typically provided by the Authorities
#' 
#' @description
#' Read xyz data and generate a raster  \code{Raster*} object.  
#' 
#' @param txtFn ASCII tect file with xyz values

#' @return 
#' A \code{RasterLayer} 
#' 
#' @author
#' Chris Reudenbach
#' 
 
#' @examples

#' 
#' # get some typical authority generated data
#' url<-"http://www.ldbv.bayern.de/file/zip/10430/DGM_1_ascii.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,files = grep(".tif", unzip(res,list = TRUE)$Name,value = TRUE),junkpaths = TRUE,overwrite = TRUE)
#' readxyz(file.path(getwd(),basename(grep(".tif", unzip(res,list = TRUE)$Name,value = TRUE))))

#' 
#' @export readxyz
#' 

readxyz <- function(xyzFN=NUL,  epsgCode ="25832"){
# read data 
xyz<-data.table::fread(xyzFN)
cat("write it to",paste0(dirname(xyzFN),"/",tools::file_path_sans_ext(basename(xyzFN)),".tif"),"\n")
cat("this will probably take a while...\n")
r <- raster::rasterFromXYZ(xyz,crs=sp::CRS(paste0("+init=epsg:",epsgCode)))
# write it to geotiff
raster::writeRaster(r, paste0(dirname(xyzFN),"/",tools::file_path_sans_ext(basename(xyzFN)),".tif"),overwrite=TRUE)
cat("...finished\n")
 }