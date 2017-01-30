if (!isGeneric('xyz2geoTIFF')) {
  setGeneric('xyz2geoTIFF', function(x, ...)
    standardGeneric('xyz2geoTIFF'))
}
#' Read and Convert xyz DEM/DSM Data as typically provided by the Authorities
#' 
#' @description
#' Read xyz data and generate a raster  \code{Raster*} object.  
#' 
#' @param txtFn ASCII tect file with xyz values

#' @return 
#' a geoT
#' 
#' @author
#' Chris Reudenbach
#' 

#' @examples

#' 
#' # get some typical data as provided by the authority
#' url<-"http://www.ldbv.bayern.de/file/zip/10430/DGM_1_ascii.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,files = grep(".tif", unzip(res,list = TRUE)$Name,value = TRUE),junkpaths = TRUE,overwrite = TRUE)
#' 
#' xyz2geoTIFF(file.path(getwd(),basename(grep(".g01dgm", unzip(res,list = TRUE)$Name,value = TRUE))))
#' 
#' plot(raster(paste0(getwd(),"/",tools::file_path_sans_ext(basename(file.path(getwd(),basename(grep(".g01dgm", unzip(res,list = TRUE)$Name,value = TRUE))))),".tif")))
#' 
#' @export xyz2geoTIFF
#' 

xyz2geoTIFF <- function(xyzFN=NUL,  epsgCode ="25832"){
  # read data 
  xyz<-data.table::fread(xyzFN)
  cat("write it to",paste0(dirname(xyzFN),"/",tools::file_path_sans_ext(basename(xyzFN)),".tif"),"\n")
  cat("this will probably take a while...\n")
  r <- raster::rasterFromXYZ(xyz,crs=sp::CRS(paste0("+init=epsg:",epsgCode)))
  # write it to geotiff
  raster::writeRaster(r, paste0(dirname(xyzFN),"/",tools::file_path_sans_ext(basename(xyzFN)),".tif"),overwrite=TRUE)
  cat("...finished\n")
}