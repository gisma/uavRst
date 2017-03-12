if (!isGeneric('readGPX ')) {
  setGeneric('readGPX ', function(x, ...)
    standardGeneric('readGPX '))
}

#' Read GPX file
#' 
#' Read a GPX file. By default, it reads all possible GPX layers, and only returns shapes for layers that have any features.
#' 
#' @param file a GPX filename (including directory)
#' @param layers vector of GPX layers. Possible options are \code{"waypoints"}, \code{"tracks"}, \code{"routes"}, \code{"track_points"}, \code{"route_points"}. By dedault, all those layers are read.
#' @return  if the layer has any features a sp object is returned.
#' @export readGPX
#' @note cloned from tmap
#' 

readGPX <- function(file, layers=c("waypoints", "tracks", "routes", "track_points", "route_points")) {
  if (!all(layers %in% c("waypoints", "tracks", "routes", "track_points", "route_points"))) stop("Incorrect layer(s)", call. = FALSE)
  
  # check if features exist per layer
  suppressWarnings(hasF <- sapply(layers, function(l) {
    ogrInfo(dsn = file, layer=l)$have_features
  }))
  
  if (!any(hasF)) stop("None of the layer(s) has any features.", call. = FALSE)
  
  res <- lapply(layers[hasF], function(l) {
    readOGR(dsn = file, layer=l, verbose=FALSE)
  })
  names(res) <- layers[hasF]
  
  if (sum(hasF)==1) {
    res[[1]]
  } else {
    res
  }
}


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




rasterCheckAdjustProjection <- function(x) {
  llcrs <- "+proj=longlat +datum=WGS84 +no_defs"
  
  is.fact <- raster::is.factor(x)[1]
  
  non_proj_waning <-
    paste("supplied", class(x)[1], "has no projection information!", "\n",
          "provide a correctly georeferenced data raster object or 'GDAL File")
  
  if (is.fact) {
    x <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(llcrs)),
      method = "ngb")
    x <- raster::as.factor(x)
  } else {
    x <- raster::projectRaster(
      x, raster::projectExtent(x, crs = sp::CRS(llcrs)),
      method = "bilinear")
  }
  
  return(x)
  
}

# Check projection of objects according to their keywords -------

compareProjCode <- function(x) {
  proj <- datum <- nodefs <- "FALSE"
  allWGS84 <- as.vector(c("+init=epsg:4326", "+proj=longlat", "+datum=WGS84", "+no_defs", "+ellps=WGS84", "+towgs84=0,0,0"))
  s <- as.vector(strsplit(x," "))
  for (i in seq(1:length(s[[1]]))) {
    
    if (s[[1]][i] == "+init=epsg:4326") {
      proj <- datum <- nodefs <- "TRUE"
    }
    if (s[[1]][i] == "+proj=longlat") {
      proj <- "TRUE"
    }
    if (s[[1]][i] == "+no_defs") {
      nodefs <- "TRUE"
    }
    if (s[[1]][i] == "+datum=WGS84") {
      datum <- "TRUE"
    }
  }
  if (proj == "TRUE" & nodefs == "TRUE" &  datum == "TRUE") {
    ret <- TRUE
  } else {
    ret = FALSE
  }
  return(ret)
}

# Check and potentially adjust projection of objects to be rendered -------

checkAdjustProjection <- function(x) {
  
  if (class(x)[1] %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    x <- rasterCheckAdjustProjection(x)
  }
  
  return(x)
}

# create an spatiallineobject from 2 points
# optional export as shapefile
makeLine <- function(Lon,Lat,ID,export=FALSE){  
  line <- SpatialLines(list(Lines(Line(cbind(Lon,Lat)), ID = ID)))
  sp::proj4string(line) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  if (export) {
    writeLinesShape(home,"home.shp")
    writeLinesShape(start,"start.shp")
  }
  return(line)
}

getmaxposFromLine <- function(dem,line){
  mask <- dem
  raster::values(mask) <- NA
  #...update it with the altitude information of the flightline
  mask  <- raster::rasterize(line,mask)
  mask2 <- mask*dem
  # and find the position of the max altitude
  idx = raster::which.max(mask2)
  maxPos = raster::xyFromCell(mask2,idx)
  return(maxPos)
}

get_posValueFromPoly <- function(dem,lN,max = TRUE, min = FALSE, use_expression = FALSE, expression = NULL){
  dcs <- rgdal::readOGR(path_run,lN,verbose = FALSE)
  ids <- unique(dcs@data$ID)
 
  ret_exp_xy <-  mclapply(ids,function(x){
    cat("calculate id: ",x,"\n")
    mask <- dem
    raster::values(mask) <- NA
    rn <- as.character(x)
    raster::writeRaster(mask,paste0(rn,"poly.tif"),overwrite = TRUE)
    gdalUtils::gdal_rasterize(src_datasource = paste0(path_run,lN,'.shp'),
                              dst_filename =   paste0(path_run,rn,'poly.tif'),
                              where = paste0('ID=',x),
                              burn = 1,
                              l = lN)
    
      #system(paste0('gdal_rasterize -where ID=',x,' -l ',lN,' -burn 1 ',lN,'.shp ',rn,'poly.tif'), intern = TRUE, ignore.stdout = TRUE)
    maskR <- raster::raster(paste0(path_run,rn,"poly.tif"))
    exR <- maskR * dem
    if (max) idx = which.max(exR)
    else if (min) idx  = which.min(exR)
    else if (use_expression) idx  = exR[eval(paste(exR,expression)) ]
    ret_expPos = xyFromCell(exR,idx)
    df <- data.frame(x = ret_expPos[1], y = ret_expPos[2], id = x)
    system(paste0("rm ",path_run, rn,"poly.tif"))
    return(df)
    },mc.cores = detectCores())#,mc.cores = detectCores()
  ret_expPos <- do.call("rbind", ret_exp_xy)
  #ret_expPos <- ret_expPos[ret_expPos$id >= 0,]
  sp::coordinates(ret_expPos) <- ~x+y
  sp::proj4string(ret_expPos) <- dem@crs
  mask <- dem
  values(mask) <- NA
  seeds <- raster::rasterize(ret_expPos,mask)
  # writeRaster(mask,"seeds.tif",overwrite=TRUE)
  # gdalUtils::gdalwarp(paste0(path_run,"seeds.tif"), 
  #                     paste0(path_run,"seeds.sdat"), 
  #                     overwrite = TRUE,  
  #                     of = 'SAGA',
  #                     verbose = FALSE)
  seeds[seeds >= 0] <- 1
  return(seeds)
}

G2Tiff <- function(runDir = NULL, layer = NULL, returnRaster = FALSE) {
  
  rgrass7::execGRASS("r.out.gdal",
                     flags     = c("c","overwrite","quiet"),
                     createopt = "TFW=YES,COMPRESS=LZW",
                     input     = layer,
                     output    = paste0(runDir,"/",layer,".tif")
  )
  if (returnRaster) return(raster::raster(paste0(runDir,"/",layer,".tif")))
}

Tiff2G <- function(runDir = NULL, layer = NULL) {
  rgrass7::execGRASS('r.external',
                     flags  = c('o',"overwrite","quiet"),
                     input  = paste0(layer,".tif"),
                     output = layer,
                     band   = 1
  )
}

OGR2G <- function(runDir = NULL, layer = NULL) {
  # import point locations to GRASS
  rgrass7::execGRASS('v.in.ogr',
                     flags  = c('o',"overwrite","quiet"),
                     input  = paste0(layer,".shp"),
                     output = layer
  )
}

G2OGR <- function(runDir = NULL, layer = NULL){
  rgrass7::execGRASS("v.out.ogr",
                     flags  = c("overwrite","quiet"),
                     input  = layer,
                     type   = "line",
                     output = paste0(layer,".shp")
  )
}

#' Build package manually
#' 
#' @description 
#' This function was specifically designed to build a package from local source 
#' files manually, i.e., without using the package building functionality 
#' offered e.g. by RStudio. 
#' @details NOTE the default setting are focussing HRZ environment at Marburg University
#' 
#' 
#' @param dsn 'character'. Target folder containing source files; defaults to 
#' the current working directory.
#' @param pkgDir 'character'. Target folder containing the result ing package of the invoked build process. According to Marburg University pools the default is set to pkgDir="H:/Dokumente". If you want to use it in a different setting you may set pkgDir to whatever you want.
#' @param document 'logical'. Determines whether or not to invoke 
#' \code{\link{roxygenize}} with default roclets for documentation purposes.  
#' @param ... Further arguments passed on to \code{\link[devtools]{build}}. 
#' 
#' @seealso 
#' \code{\link{roxygenize}}, \code{\link[devtools]{build}},\code{\link{install.packages}}.
#' 
#' @author 
#' Florian Detsch, Chris Reudenbach
#' 
#' 
#' @examples
#' \dontrun{
#' ## when in a package directory, e.g. '~/satellite' 
#' winUniMrBuild()
#' }
#' 
#' @export winUniMrBuild
#' @name winUniMrBuild
winUniMrBuild <- function(dsn = getwd(), pkgDir="H:/Dokumente",document = TRUE, ...) {
  
  ## reset 'dsn' to 'H:/...'  
  if (length(grep("students_smb", dsn)) > 0) {
    lst_dsn <- strsplit(dsn, "/")
    chr_dsn <- unlist(lst_dsn)[3:5]
    dsn <- paste0("H:/", paste(chr_dsn, collapse = "/"))
  }
  
  ## if 'document = TRUE', create documentation 
  if (document) {
    cat("\nCreating package documentation...\n")
    roxygen2::roxygenize(package.dir = dsn, 
                         roclets = c('rd', 'collate', 'namespace'))
  }
  
  ## build package
  cat("\nBuilding package...\n")
  
  devtools::build(pkg = dsn, path = dirname(dsn), ...)
  
  
  ## install package
  cat("Installing package...\n")
  pkg <- list.files(dirname(pkgDir), full.names = TRUE,
                    pattern = paste0(basename(dsn), ".*.tar.gz$"))
  pkg <- pkg[length(pkg)]
  
  install.packages(pkg, repos = NULL)
  
  return(invisible(NULL))
}
SAGA2R <- function(fn,ext) {
  gdalUtils::gdalwarp(paste0(path_run,fn,".sdat"), 
                      paste0(path_run,fn,".tif"), 
                      overwrite = TRUE,  
                      verbose = FALSE)
  x<-raster::raster(paste0(path_run,fn,".tif"))
  x@extent <- ext
  # convert to SAGA
  return(x)
}

R2SAGA <- function(x,fn) {
  
  raster::writeRaster(x,paste0(path_run,fn,".tif"),overwrite = TRUE)
  # convert to SAGA
  gdalUtils::gdalwarp(paste0(path_run,fn,".tif"), 
                      paste0(path_run,fn,".sdat"), 
                      overwrite = TRUE,  
                      of = 'SAGA',
                      verbose = FALSE)
}