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
#' extract for all polygons the position of the maximum value of the applied raster(s)
#' @description
#' extract for all polygons the position of the maximum value
#' @export
#' 
get_max_posFromPoly <- function(dem,lN, poly_split=TRUE){
  # read raster input data 
  if (split) system(paste0("rm -rf ",paste0(path_tmp,"split")))
  dem <- raster::raster(dem)
  fn <- spatial.tools::create_blank_raster(reference_raster=dem,filename = paste0(path_tmp,lN,"raw"))
  mask <- raster::raster(fn)
  # maskx <- velox::velox(mask)
  # chmx <- velox::velox(dem)
  
  # read vector input data the sf way
  sf_dcs <- sf::st_read(paste0(path_run,lN,".shp"),quiet = TRUE)
  dcs <- as(sf_dcs, "Spatial")

  # retrieve unique NAME 
  ids <- unique(dcs@data$NAME)
  
  if (poly_split) {
    cat(":: split polygons...\n")
    dir.create(paste0(path_tmp,"split"),recursive=TRUE)
    
    # split polygon with respect to the NAME attribute
    parallel::mclapply(ids,function(x){
      rn <- as.character(x)
      gdalUtils::ogr2ogr(src_datasource_name = paste0(path_run,lN,".shp"),
                         dst_datasource_name = paste0(path_tmp,"split/",lN,"_",rn,".shp"),
                         where = paste0("NAME='",rn,"'")
                         , nln = rn)
    },
    mc.cores = parallel::detectCores())
  }
  # parallel retrival of maxpos
  

  ret_max_pos <-  parallel::mclapply(ids,function(x) {
    
    # assign vars
    #maskx <- velox::velox(mask)
    #chmx <- velox::velox(dem)
     
    rn <- as.character(x)

    # create temp folder and assign it to raster
    dir.create(paste0(path_tmp,rn),recursive=TRUE)
    raster::rasterOptions(tmpdir=paste0(path_tmp,rn)) 
    
    # read single polygon sf is even in this construct times faster
    sf_shp <- sf::st_read(paste0(path_tmp,"split/",lN,"_",rn,".shp"),quiet = TRUE)
    shp <- as(sf_shp, "Spatial")
    
    # reclass VALUE to 1
    shp@data$VALUE <-1
    
    # crop raster acccording to the polygon
    #maskx$crop(c(sp::bbox(shp)[1],sp::bbox(shp)[3],sp::bbox(shp)[2],sp::bbox(shp)[4]))
    #chmx$crop(c(sp::bbox(shp)[1],sp::bbox(shp)[3],sp::bbox(shp)[2],sp::bbox(shp)[4]))
    
    # rastrize mask
    maskx$rasterize(shp,field = "VALUE",band = 1)
    
    # re-convert to raster format
    m1 <- maskx$as.RasterLayer(band=1)
    #d1 <- chmx$as.RasterLayer(band=1)
    
    # get maxpos of crown area
    m1 <-raster::crop(m1,c(sp::bbox(shp)[1],sp::bbox(shp)[3],sp::bbox(shp)[2],sp::bbox(shp)[4]))
    d1 <-raster::crop(m1,c(sp::bbox(shp)[1],sp::bbox(shp)[3],sp::bbox(shp)[2],sp::bbox(shp)[4]))
    max_pos <- raster::xyFromCell(d1,which.max(m1 * dem))
    
    # write it to a df
    df <- data.frame(x = max_pos[1], y = max_pos[2], id = rn)
   
    # get rid of temp raster files
    system(paste0("rm -rf ",paste0(path_tmp,rn)))
    
    
    return(df)},    mc.cores = parallel::detectCores()
  )
  
  # create a spatial point data frame
  max_pos <- as.data.frame(do.call("rbind", ret_max_pos))
  sp::coordinates(max_pos) <- ~x+y
  sp::proj4string(max_pos) <- as.character(dem@crs)
  max_pos@data$id<- as.numeric(max_pos@data$id)
  # create seeds file for rasterizing max_pos
  # re-convert to raster format
  fn <- spatial.tools::create_blank_raster(reference_raster=dem,filename = paste0(path_tmp,lN,"raw"))
  mask <- raster::raster(fn)
  seeds <- raster::rasterize(max_pos,mask,field="id")
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

fun_multiply <- function(x)
{
  # Note that x is received by the function as a 3-d array:
  band1 <- x[,,1]
  band2 <- x[,,2]
  result <- band1*band2
  # The output of the function should also be a 3-d array,
  # even if it is a single band:
  result <- array(result,dim=c(dim(x)[1],dim(x)[2],1))
  
  return(result)
}
fun_whichmax <- function(mask,value) { 
raster::xyFromCell(value,which.max(mask * value))
}
