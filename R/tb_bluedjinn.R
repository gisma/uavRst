# karim is the blue djinn
if (!isGeneric('h_read_gpx ')) {
  setGeneric('h_read_gpx ', function(x, ...)
    standardGeneric('h_read_gpx '))
}

#' Read GPX file
#' 
#' Read a GPX file. By default, it reads all possible GPX layers, and only returns shapes for layers that have any features.
#' if the layer has any features a sp object is returned.
#' @param file a GPX filename (including directory)
#' @param layers vector of GPX layers. Possible options are \code{"waypoints"}, \code{"tracks"}, \code{"routes"}, \code{"track_points"}, \code{"route_points"}. By dedault, all those layers are read.

#' @export h_read_gpx
#' @note cloned from tmap
#' 

h_read_gpx <- function(file, 
                       layers=c("waypoints", "tracks", "routes", "track_points", "route_points")
) {
  if (!all(layers %in% c("waypoints", "tracks", "routes", "track_points", "route_points"))) stop("Incorrect layer(s)", call. = FALSE)
  
  # check if features exist per layer
  suppressWarnings(hasF <- sapply(layers, function(file,l) {
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


if (!isGeneric('xyz2tif')) {
  setGeneric('xyz2tif', function(x, ...)
    standardGeneric('xyz2tif'))
}
#' Read and Convert xyz DEM/DSM Data as typically provided by the Authorities
#' 
#' @description
#' Read xyz data and generate a raster  \code{Raster*} object.  
#' 
#' @param xyzFN ASCII tect file with xyz values
#' @param epsgCode "25832"

#' 
#' 

#' @examples

#' \dontrun{
#' # get some typical data as provided by the authority
#' url<-"http://www.ldbv.bayern.de/file/zip/10430/DGM_1_ascii.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,
#'       files = grep(".tif", unzip(res,list = TRUE)$Name,value = TRUE),
#'       junkpaths = TRUE,
#'       overwrite = TRUE)
#' 
#' xyz2tif(file.path(getwd(),
#'           basename(grep(".g01dgm", 
#'           unzip(res,list = TRUE)$Name,value = TRUE))))
#' 
#' plot(raster::raster(paste0(getwd(),"/",file_path_sans_ext(basename(file.path(getwd(),
#' basename(grep(".g01dgm", unzip(res,list = TRUE)$Name,value = TRUE))))),".tif")))
#' }
#' @export xyz2tif
#' 

xyz2tif <- function(xyzFN=NULL,  epsgCode ="25832"){
  # read data 
  xyz<-data.table::fread(xyzFN)
  cat("write it to",paste0(dirname(xyzFN),"/",tools::file_path_sans_ext(basename(xyzFN)),".tif"),"\n")
  cat("this will probably take a while...\n")
  r <- raster::rasterFromXYZ(xyz,crs=sp::CRS(paste0("+init=epsg:",epsgCode)))
  # write it to geotiff
  raster::writeRaster(r, paste0(dirname(xyzFN),"/",tools::file_path_sans_ext(basename(xyzFN)),".tif"),overwrite=TRUE)
  cat("...finished\n")
}




h_raster_adjust_projection <- function(x) {
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

h_comp_ll_proj4 <- function(x) {
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


# 
# 
#' create an spatiallineobject from 2 points
#' @description
#' create an spatiallineobject from 2 points, optional export as shapefile
#' @param p1 coordinate of first point
#' @param p2 coordinate of second point
#' @param proj4 proj4 string
#' @param ID id of line
#' @param export write shafefile default = F 
#' @export
#' 
sp_line <- function(p1,
                    p2,
                    ID,
                    proj4="+proj=longlat +datum=WGS84 +no_defs",
                    export=FALSE) {   
  line <- SpatialLines(list(Lines(Line(cbind(p1,p2)), ID = ID)))
  sp::proj4string(line) <- CRS(proj4)
  if (export) {
    writeLinesShape(line,paste0(ID,"home.shp"))
  }
  return(line)
}
#' create an spatialpointobject from 1 points
#' @description
#' create an spatialpointobject from 1 points, optional export as shapefile
#' @param lat lat of first point
#' @param lon lon of first point
#' @param ID name of point
#' @param proj4 proj4 string
#' @param export write shafefile default = F 
#' @export
#' 
sp_point <- function(lon,
                     lat,
                     ID="point",
                     proj4="+proj=longlat +datum=WGS84 +no_defs",
                     export=FALSE) {
  point = cbind(lon,lat)
  point = sp::SpatialPoints(point)
  point = SpatialPointsDataFrame(point, as.data.frame(ID))
  sp::proj4string(point) <- CRS(proj4)
  if (export) {
    writeLinesShape(ID,paste0(ID,".shp"))
  }
  return(point)
}

#' applies a line to a raster and returns the position of the maximum value
#' @description
#'  applies a line to a raster and returns the position of the maximum value
#' @param dem raster object
#' @param line  sp object
#' @export
#' 
line_extract_maxpos <- function(dem,line){
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
#' @param x path and name of a GDAL raster file  
#' @param lN layer name of shape file
#' @param poly_split split polygon in single file default is TRUE
#' extract for all polygons the position of the maximum value
#' @export extractMaxPosPoly
#' 
extractMaxPosPoly <- function(x,lN, poly_split=TRUE){
  # read raster input data 
  if (poly_split) {system(paste0("rm -rf ",paste0(path_tmp,"split")))}
  dem <- raster::raster(x)
  fn <- spatial.tools::create_blank_raster(reference_raster=dem,filename = paste0(path_tmp,lN,"raw"))
  mask <- raster::raster(fn)
  maskx <- velox::velox(mask)
  # chmx <- velox::velox(dem)
  
  # read vector input data the sf way
  sf_dcs <- sf::st_read(paste0(path_run,lN,".shp"),quiet = TRUE)
  dcs <-  methods::as(sf_dcs, "Spatial")
  
  # retrieve unique NAME 
  ids <- unique(dcs@data$NAME)
  
  if (poly_split) {
    cat("     split polygons...\n")
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
  
  cat("     max height coords search...\n")
  cat("     analyze",length(ids) ,"polygons\n")
  cat("     assuming 5 cm resolution and an average of 15 sqm per polygon\n     the analysis will approx run until",format(Sys.time() + length(ids), " %X "),"\n")
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
    # TODO which(mat == max(mat), arr.ind=TRUE)
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
  raster::writeRaster(seeds,paste0(path_run,"seeds.tif"),overwrite=TRUE)
  return(list(seeds,max_pos))
}


#' converts GRASS raster to geotiff
#' @description converts GRASS raster to geotiff
#' @param runDir path of working directory
#' @param layer name GRASS raster
#' @param returnRaster return GRASS raster as an R raster object default = FALSE

#' @export
#' 
h_grass2tif <- function(runDir = NULL, layer = NULL, returnRaster = FALSE) {
  link2GI::linkGRASS7()
  rgrass7::execGRASS("r.out.gdal",
                     flags     = c("c","overwrite","quiet"),
                     createopt = "TFW=YES,COMPRESS=LZW",
                     input     = layer,
                     output    = paste0(runDir,"/",layer,".tif")
  )
  if (returnRaster) return(raster::raster(paste0(runDir,"/",layer,".tif")))
}


#' converts OGR to GRASS vector 
#' @description converts OGR to GRASS vector 
#' @param runDir path of working directory
#' @param layer name GRASS raster
#' @export
h_shape2grass <- function(runDir = NULL, layer = NULL) {
  # import point locations to GRASS
  rgrass7::execGRASS('v.in.ogr',
                     flags  = c('o',"overwrite","quiet"),
                     input  = paste0(layer,".shp"),
                     output = layer
  )
}

#' converts GRASS vector to shape file
#' @description converts GRASS vector to shape file
#' @param runDir path of working directory
#' @param layer name GRASS raster
#' @export
h_grass2shape <- function(runDir = NULL, layer = NULL){
  rgrass7::execGRASS("v.out.ogr",
                     flags  = c("overwrite","quiet"),
                     input  = layer,
                     type   = "line",
                     output = paste0(layer,".shp")
  )
}


#' converts SAGA raster to R raster object
#' @description converts SAGA raster to R raster object
#' @param fn filname without extension
#' @param ext extent of the raster in R notation
#' @export
saga2r<- function(fn,ext) {
  gdalUtils::gdalwarp(paste0(path_run,fn,".sdat"), 
                      paste0(path_run,fn,".tif"), 
                      overwrite = TRUE,  
                      verbose = FALSE)
  x<-raster::raster(paste0(path_run,fn,".tif"))
  x@extent <- ext
  # convert to SAGA
  return(x)
}

#' converts SAGA raster to R raster object
#' @description converts SAGA raster to R raster object
#' @param x raster object
#' @param fn filname without extension
#' @export
r2saga <- function(x,fn) {
  
  raster::writeRaster(x,paste0(path_run,fn,".tif"),overwrite = TRUE)
  # convert to SAGA
  
  
  gdalUtils::gdalwarp(paste0(path_run,fn,".tif"), 
                      paste0(path_run,fn,".sdat"), 
                      overwrite = TRUE,  
                      of = 'SAGA',
                      verbose = FALSE)
}

h_fun_multiply <- function(x)
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
h_fun_whichmax <- function(mask,value) { 
  raster::xyFromCell(value,which.max(mask * value))
}


#'calculate decriptive stats of raster values of underlying a polygon
#'
#'@description
#' calculate statitiscs of polygon based raster extraction. returns a spatialpolygon dataframe containing decriptive statistics
#'
#'@author Chris Reudenbach
#'
#'@param x  spatial raster object
#'@param spdf   spatial point dataframe
#'@param count  0 1 switch
#'@param min    0 1 switch
#'@param max    0 1 switch
#'@param sum    0 1 switch
#'@param range  0 1 switch
#'@param mean   0 1 switch
#'@param var    0 1 switch 
#'@param stddev 0 1 switch
#'@param quantile number of quantile
#'

#'
#'
#'@export xpolystat
#'@examples
#'\dontrun{
#' # Tree segementation based on a CHM
#' polyStat <- xpolystat(c("chm","dah"),
#'                       spdf = "tree_crowns.shp")
#'}
#'
xpolystat <- function(x = NULL,
                      spdf = NULL,
                      count = 1,
                      min = 1,
                      max = 1,
                      sum = 1,
                      range = 1,
                      mean = 1,
                      var = 1, 
                      stddev = 1,
                      quantile = 10)   {
  
  #cat(":: run statistics...\n")
  # calculate chm statistics for each crown 
  
  saga <- link2GI::linkSAGA()
  sagaCmd<-saga$sagaCmd
  
  if (class(spdf)!="character")     {
    rgdal::writeOGR(obj    = spdf,
                    layer  = "spdf", 
                    driver = "ESRI Shapefile", 
                    dsn    = path_run, 
                    overwrite_layer = TRUE)
    spdf<-"spdf.shp"
  }
  
  for (i in seq(1:length(x))) {
    cat(":: calculate ",x[i], " statistics\n")
    ret <-  system(paste0(sagaCmd, " shapes_grid 2 ",
                          " -GRIDS ",path_run,x[i],".sgrd",
                          " -POLYGONS ",path_run,spdf,
                          " -NAMING 1",
                          " -METHOD 2",
                          " -COUNT ", count,
                          " -MIN  ", min,
                          " -MAX ", max,
                          " -SUM ",sum,
                          " -RANGE ",range,
                          " -MEAN  ", mean,
                          " -VAR ",var,
                          " -STDDEV ",stddev,
                          " -QUANTILE ",quantile,
                          " -PARALLELIZED 1",
                          " -RESULT ",path_run,x[i],"Stat.shp"),
                   intern = TRUE)
    
    stat1 <- rgdal::readOGR(path_run,paste0(x[i],"Stat"), verbose = FALSE)
    names(stat1) <- gsub(names(stat1),pattern = "\\.",replacement = "")
    
    if (i == 1) {
      stat <- stat1
      
    } else  {
      stat@data <- cbind(stat@data,stat1@data[4:length(names(stat1))])
      #stat <- stat1
    }
  }
  
  rgdal::writeOGR(obj = stat,
                  layer = "polyStat",
                  driver = "ESRI Shapefile",
                  dsn = path_run,
                  overwrite_layer = TRUE)
  return(stat)
}

#' creates all link2GI links
#' @description converts SAGA raster to R raster object
#' @param links character links
#' @param linkItems character list of c("saga","grass7","otb","gdal")
#' @param simple boolean true  make all
#' @param sagaArgs sagaArgs
#' @param grassArgs grassArgs 
#' @param otbArgs otbArgs 
#' @param gdalArgs gdalArgs 

#' @export
linkBuilder <- function(links=NULL, 
                        simple = TRUE,
                        linkItems = c("saga","grass7","otb","gdal"),
                        sagaArgs = "default",
                        grassArgs = "default",
                        otbArgs =   "default", 
                        gdalArgs =  "quiet = TRUE, 
                                     returnPaths = TRUE"
                        
                        
){
  if (sagaArgs == "default") sagaArgs <- "default_SAGA = NULL, searchLocation = 'default', ver_select = FALSE, quiet = TRUE, returnPaths = TRUE"
  if (grassArgs == "default") grassArgs <- "x = NULL, default_GRASS7 = NULL, search_path = NULL, ver_select = FALSE, gisdbase_exist = FALSE, gisdbase = NULL,
                                     location = NULL, spatial_params = NULL, resolution = NULL, quiet = TRUE, returnPaths = FALSE"
  if (otbArgs == "default") otbArgs <- "bin_OTB = NULL, root_OTB = NULL, type_OTB = NULL, searchLocation = NULL, ver_select = FALSE"
  if (gdalArgs == "default") gdalArgs <- "quiet = TRUE, returnPaths = TRUE"
  if (is.null(links) && (simple)){
    link<-list()
    for (links in linkItems) {
      link[[links]]<-assign(links,eval(parse(text=paste("link2GI::link",toupper(links),"(returnPaths = T)",sep = "")))) 
    }
    
  } else if (is.null(links)) {
    link<-list()
    for (links in linkItems) {
      link[[links]]<-assign(links,eval(parse(text=paste("link2GI::link",toupper(links),"(",eval(parse(text=paste0(links,"Args"))),")",sep = "")))) 
    }
    
  }
  return(link)
}


# fill holes
fillGaps<- function (folder,layer){
  cat(":: fill data gaps using gdal_fillnodata... \n")
  
  # fill data holes
  if (Sys.info()["sysname"] == "Windows"){
    ret <- system2(command = "gdal_fillnodata.py ",args = 
                     paste0(folder,"/",layer,".tif ",
                            folder,"/",layer,".tif "))
    
  } else {
    ret <- system(paste0("gdal_fillnodata.py ", folder,layer,".tif ",
                         folder,layer,".tif "),intern = TRUE)
  }
  
  # write filled data back to GRASS
  rgrass7::execGRASS('r.in.gdal',  flags=c('o',"overwrite"), input=paste0(folder,"/",layer,".tif"),  output=layer, band=1)
}


# creates names and ranges from a simple list for zrange cuts
makenames<-function(zr ) {
  class<-list()
  zrange<-list()
  for ( i in 1:(length(zr[[1]]))) {
    if (i == length(zr[[1]])){
      class[[i]]<-c(paste0('class',zr[[1]][1],zr[[1]][length(zr[[1]])]))
      zrange[[i]]<-c(zr[[1]][1], zr[[1]][length(zr[[1]])])  
    } else {
      class[[i]]<-c(paste0('class',zr[[1]][i],zr[[1]][i+1]))
      zrange[[i]]<-c(zr[[1]][i],zr[[1]][i+1])
    }
  }
  return(list(unlist(class),zrange))
}

extractTrainPixelValues<- function(imgStack=NULL,trainData=NULL,responseCol=NULL){
  #extract training Area pixel values
  dfTpv = data.frame(matrix(vector(), nrow = 0, ncol = length(names(imgStack)) + 1))   
  for (i in 1:length(unique(trainData[[responseCol]]))){
    category <- unique(trainData[[responseCol]])[i]
    cat("\n extracting cat: ",levels(category)[i]," no: ",i," of: ",length(unique(trainData[[responseCol]])))
    categorymap <- trainData[trainData[[responseCol]] == category,]
    dataSet <- raster::extract(imgStack, categorymap)
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfTpv <- rbind(dfTpv, df)
  }
  names(dfTpv)<-gsub(names(dfTpv),pattern = "\\.",replacement = "_")
  return(dfTpv)
}

# calculate mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

### getPopupStyle creates popup style =================================================
getPopupStyle <- function() {
  # htmlTemplate <- paste(
  #   "<html>",
  #   "<head>",
  #   "<style>",
  #   "#popup",
  #   "{font-family: Arial, Helvetica, sans-serif;width: 20%;border-collapse: collapse;}",
  #   "#popup td {font-size: 1em;border: 0px solid #85ADFF;padding: 3px 20px 3px 3px;}",
  #   "#popup tr.alt td {color: #000000;background-color: #F0F5FF;}",
  #   "#popup tr.coord td {color: #000000;background-color: #A8E6A8;}",
  #   "div.scrollableContainer {max-height: 200px;max-width: 100%;overflow-y: auto;overflow-x: auto;margin: 0px;background: #D1E0FF;}",
  #   "</style>",
  #   "</head>",
  #   "<body>",
  #   "<div class='scrollableContainer'>",
  #   "<table class='popup scrollable'>",
  #   "<table id='popup'>")
  # return(htmlTemplate)
  fl <- system.file("templates/popup.brew", package = "mapview")
  pop <- readLines(fl)
  end <- grep("<%=pop%>", pop)
  return(paste(pop[1:(end-2)], collapse = ""))
}

# Split rgb
gdalsplit<-function(fn){
  directory<-dirname(fn)  
  for (i in seq(1:3)){
    gdalUtils::gdal_translate(fn,paste0(directory,"/b",i,".tif"),b=i)
  }
}