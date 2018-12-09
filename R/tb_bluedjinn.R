#' Read a GPX file.

#' @description Read a GPX file. By default, it reads all possible GPX layers, and only returns shapes for layers that have any features.
#' if the layer has any features a sp object is returned.
#'
#' @param file a GPX filename (including directory)
#' @param layers vector of GPX layers. Possible options are \code{"waypoints"}, \code{"tracks"}, \code{"routes"}, \code{"track_points"}, \code{"route_points"}. By dedault, all those layers are read.
#' @keywords internal

#' @export read_gpx
#' @note adapted from \code{\link[tmaptools]{read_GPX}}
#'

read_gpx <- function(file,
                     layers=c("waypoints", "tracks", "routes", "track_points", "route_points")
) {
  if (!all(layers %in% c("waypoints", "tracks", "routes", "track_points", "route_points"))) stop("Incorrect layer(s)", call. = FALSE)
  
  # check if features exist per layer
  suppressWarnings(hasF <- sapply(layers, function(file,l) {
    rgdal::ogrInfo(dsn = file, layer=l)$have_features
  }))
  
  if (!any(hasF)) stop("None of the layer(s) has any features.", call. = FALSE)
  
  res <- lapply(layers[hasF], function(l) {
    rgdal::readOGR(dsn = file, layer=l, verbose=FALSE)
  })
  names(res) <- layers[hasF]
  
  if (sum(hasF)==1) {
    res[[1]]
  } else {
    res
  }
}


#' Read and Convert xyz DEM/DSM Data as typically provided by the Authorities.
#'
#' @description
#' Read xyz data and generate a Raster* object.
#'
#' @param xyzFN ASCII tect file with xyz values
#' @param epsgCode "25832"
#'
#' @export xyz2tif

#' @examples
#'\dontrun{
#' ##- libraries
#' require(uavRst)
#' owd <- getwd()
#' setwd(tempdir())
#' ##- get typical xyz DEM data in this case from the Bavarian authority 
#' utils::download.file("http://www.ldbv.bayern.de/file/zip/10430/DGM_1_ascii.zip",
#'                     "testdata.zip")
#' file<- unzip("testdata.zip",list = TRUE)$Name[2]
#' unzip("testdata.zip",files = file,  overwrite = TRUE)
#' ##- show structure
#' head(read.table(file))
#' ##- create tiff file
#' ##- NOTE  for creating a geotiff you have to provide the correct EPSG code from the meta data
#' xyz2tif(file,epsgCode = "31468")
#'
#' ##- visualize it
#' raster::plot(raster::raster(file))
#' setwd(owd)
#' }


#'

xyz2tif <- function(xyzFN=NULL,  epsgCode ="25832"){
  # read data
  xyz<-data.table::fread(xyzFN)
  cat("this will probably take a while...\n")
  r <- raster::rasterFromXYZ(xyz,crs=sp::CRS(paste0("+init=epsg:",epsgCode)))
  # write it to geotiff
  raster::writeRaster(r, paste0(dirname(xyzFN),"/",tools::file_path_sans_ext(basename(xyzFN)),".tif"),overwrite=TRUE)
  cat("...finished\n")
}



# adjust projection of objects according to their keywords 
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

# Check projection of objects according to their keywords
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




#' applies a line to a raster and returns the position of the maximum value.
#' @description applies a line to a raster and returns the position of the maximum value
#' @param dem raster object
#' @param line  sp object
#' @keywords internal
#' @export
#'
line_maxpos <- function(dem,line){
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


#'  extract for all polygons the position of the maximum value of the applied raster(s).
#' @description
#' extract for all polygons the position of the maximum value
#' @param fileName path and name of a GDAL raster file
#' @param layerName layer name of shape file
#' @param polySplit split polygon in single file, default is TRUE
#' extract for all polygons the position of the maximum value
#' @param cores number of cores used
#' @keywords internal
#' @export poly_maxpos
#'
poly_maxpos <- function(fileName,layerName, polySplit=TRUE, cores=1){
  # read raster input data
  if (polySplit) {system(paste0("rm -rf ",paste0(path_tmp,"split")))}
  dem <- raster::raster(fileName)
  fn <- spatial.tools::create_blank_raster(reference_raster=dem,filename = paste0(path_tmp,basename(layerName),"raw"))
  mask <- raster::raster(fn)
  maskx <- velox::velox(mask)
  # chmx <- velox::velox(dem)
  
  # read vector input data the sf way
  sf_dcs <- sf::st_read(paste0(layerName,".shp"),quiet = TRUE)
  dcs <-  methods::as(sf_dcs, "Spatial")
  
  # retrieve unique NAME
  ids <- unique(dcs@data$NAME)
  
  if (polySplit) {
    cat("     split polygons...\n")
    cat("     analyze",length(ids) ,"polygons\n")
    cat("     calculaton time is approx.:  ",floor(length(ids)/180)," min\n")
    dir.create(paste0(path_tmp,"split"),recursive=TRUE)
    
    # split polygon with respect to the NAME attribute
    parallel::mclapply(ids,function(x){
      rn <- as.character(x)
      gdalUtils::ogr2ogr(src_datasource_name = paste0(layerName,".shp"),
                         dst_datasource_name = paste0(path_tmp,"split/",basename(layerName),"_",rn,".shp"),
                         where = paste0("NAME='",rn,"'")
                         , nln = rn)
    },
    mc.cores = cores)
  }
  
  # parallel retrival of maxpos
  cat("     max height coords search...\n")
  ret_max_pos <-  parallel::mclapply(ids,function(x) {
    rn <- as.character(x)
    # create temp folder and assign it to raster
    dir.create(paste0(path_tmp,rn),recursive=TRUE)
    raster::rasterOptions(tmpdir=paste0(path_tmp,rn))
    
    # read single polygon sf is even in this construct times faster
    sf_shp <- sf::st_read(paste0(path_tmp,"split/",basename(layerName),"_",rn,".shp"),quiet = TRUE)
    shp <- methods::as(sf_shp, "Spatial")
    
    # reclass VALUE to 1
    shp@data$VALUE <-1
    
    # rasterize mask
    maskx$rasterize(shp,field = "VALUE",band = 1)
    
    # re-convert to raster format
    m1 <- maskx$as.RasterLayer(band=1)
    
    # get maxpos of crown area
    m1 <-raster::crop(m1,c(sp::bbox(shp)[1],sp::bbox(shp)[3],sp::bbox(shp)[2],sp::bbox(shp)[4]))
    d1 <-raster::crop(m1,c(sp::bbox(shp)[1],sp::bbox(shp)[3],sp::bbox(shp)[2],sp::bbox(shp)[4]))
    max_pos <- raster::xyFromCell(d1,which.max(m1 * dem))
    
    # write it to a df
    df <- data.frame(x = max_pos[1], y = max_pos[2], id = rn)
    
    # get rid of temp raster files
    system(paste0("rm -rf ",paste0(path_tmp,rn)))
    
    
    return(df)},    mc.cores = parallel::detectCores()-1
  )
  
  # create a spatial point data frame
  max_pos <- as.data.frame(do.call("rbind", ret_max_pos))
  sp::coordinates(max_pos) <- ~x+y
  sp::proj4string(max_pos) <- as.character(dem@crs)
  max_pos@data$id<- as.numeric(max_pos@data$id)
  # create seeds file for rasterizing max_pos
  # re-convert to raster format
  fn <- spatial.tools::create_blank_raster(reference_raster=dem,filename = paste0(path_tmp,basename(layerName),"raw"))
  mask <- raster::raster(fn)
  seeds <- raster::rasterize(max_pos,mask,field="id")
  seeds[seeds >= 0] <- 1
  #raster::writeRaster(seeds,file.path(R.utils::getAbsolutePath(path_run),"seeds.tif"),overwrite=TRUE)
  return(list(seeds,max_pos))
}



#'  converts GRASS raster to Geotiff
#' @description converts GRASS raster to Geotiff
#' @param runDir path of working directory
#' @param layerName name GRASS raster
#' @param returnRaster return GRASS raster as an R raster object, default = FALSE
#' @keywords internal


#'
grass2tif <- function(runDir = NULL, layerName = NULL, returnRaster = FALSE) {
  link2GI::linkGRASS7()
  rgrass7::execGRASS("r.out.gdal",
                     flags     = c("c","overwrite","quiet"),
                     createopt = "TFW=YES,COMPRESS=LZW",
                     input     = layerName,
                     output    = paste0(runDir,"/",layerName,".tif")
  )
  if (returnRaster) return(raster::raster(paste0(runDir,"/",layerName,".tif")))
}



#' converts OGR to GRASS vector
#' @description converts OGR to GRASS vector
#' @param runDir path of working directory
#' @param layeName name GRASS raster
#' @keywords internal

shape2grass <- function(runDir = NULL, layerName = NULL) {
  # import point locations to GRASS
  rgrass7::execGRASS('v.in.ogr',
                     flags  = c('o',"overwrite","quiet"),
                     input  = paste0(layerName,".shp"),
                     output = layerName
  )
}


#'  converts GRASS vector to shape file
#' @description converts GRASS vector to shape file
#' @param runDir path of working directory
#' @param layerName name GRASS raster
#' @keywords internal

grass2shape <- function(runDir = NULL, layerName = NULL){
  rgrass7::execGRASS("v.out.ogr",
                     flags  = c("overwrite","quiet"),
                     input  = layerName,
                     type   = "line",
                     output = paste0(layerName,".shp")
  )
}





# multiply two raster ----
funMultiply <- function(x)
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

# returns maxpos of a raster ----
funWhichmax <- function(mask,value) {
  raster::xyFromCell(value,which.max(mask * value))
}



#' Calculate descriptive statistics of raster as segemented by polygons 
#'
#'@description
#' calculate statitiscs of polygon based raster extraction. Returns a spatialpolygon dataframe containing decriptive statistics
#'
#'@author Chris Reudenbach
#'
#'@param rasternames vector of raster* objects default is NULL
#'@param spdf   spatial polygon dataframe default is NULL
#'@param count  0 1 switch
#'@param min    0 1 switch
#'@param max    0 1 switch
#'@param sum    0 1 switch
#'@param range  0 1 switch
#'@param mean   0 1 switch
#'@param var    0 1 switch
#'@param stddev 0 1 switch
#'@param quantile number of quantile
#'@param proj projection string
#'@param path_run run time folder for all kind of calculations, by default tempdir() 
#'@param giLinks list of GI tools cli pathes, default is NULL
#'@param parallel run it parallel default is 1

#'
#'
#'@export poly_stat
#'@examples
#'\dontrun{
#' # required packages
#' require(uavRst)
#' require(link2GI)
#'
#' # create and check the links to the GI software
#' giLinks<-uavRst::linkAll(linkItems = c("saga","gdal"))
#' if (giLinks$saga$exist) {
#' 
#' # get the rgb image, chm and training data
#' url <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/tutorial_data.zip"
#' utils::download.file(url, file.path(tempdir(),"tutorial_data.zip"))
#' unzip(zipfile = file.path(tempdir(),"tutorial_data.zip"), exdir = tempdir())
#' 
#' polyStat <- poly_stat("chm_3-3_train1",
#'                       spdf = "rgb_3-3_train1.shp",
#'                       giLinks=giLinks)
#'                       
#' raster::plot(polyStat)
#'}
#' ##+}
#'
poly_stat <- function(rasternames = NULL,
                      spdf = NULL,
                      count = 1,
                      min = 1,
                      max = 1,
                      sum = 1,
                      range = 1,
                      mean = 1,
                      var = 1,
                      stddev = 1,
                      quantile = 10,
                      parallel = 1,
                      proj = "+proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs",
                      path_run = tempdir(),
                      giLinks =NULL )   {

  #cat(":: run statistics...\n")
  # calculate chm statistics for each crown
  if (is.null(giLinks)){
    giLinks <- list()
    giLinks$saga <- link2GI::linkSAGA()
    giLinks$gdal <- link2GI::linkGDAL()
  }
  
  gdal <- giLinks$gdal
  saga <- giLinks$saga
  sagaCmd<-giLinks$saga$sagaCmd
  
  if (file.exists(file.path(R.utils::getAbsolutePath(path_run),paste0(rasternames[1],".tif"))))
    proj<- raster::crs(raster::raster(file.path(R.utils::getAbsolutePath(path_run),paste0(rasternames[1],".tif"))))
  # else if (file.exists(paste0(path_run,rasternames[1],".sdat"))) 
  #   proj<- sp::CRS(raster::raster(paste0(path_run,rasternames[1],".sdat")))
  if (class(rasternames[1]) %in% c("RasterLayer","RasterStack", "RasterBrick"))  
    for (item in rasternames) {raster2sdat(rastobj = item ,path_run = R.utils::getAbsolutePath(path_run))}
  else if (class(rasternames[1]) %in% c("character") & file.exists(file.path(R.utils::getAbsolutePath(path_run),paste0(rasternames[1],".tif"))))
    for (item in rasternames) {
      gdalUtils::gdalwarp(file.path(R.utils::getAbsolutePath(path_run),paste0(item,".tif")),
                          file.path(R.utils::getAbsolutePath(path_run),paste0(item,".sdat")),
                          overwrite = TRUE,
                          of = 'SAGA',
                          verbose = FALSE)
    }
  
if (class(spdf)=="SpatialPolygonsDataFrame")     {
  rgdal::writeOGR(obj    = spdf,
                  layer  = "spdf",
                  driver = "ESRI Shapefile",
                  dsn    = file.path(R.utils::getAbsolutePath(path_run)),
                  overwrite_layer = TRUE)
  spdfshp<-"spdf.shp"
} else spdfshp <-spdf

for (i in seq(1:length(rasternames))) {
  cat(":: calculate ",rasternames[i], " statistics\n")
  #saga_cmd shapes_grid 2 -GRIDS=/tmp/RtmpK0j1RP/run/rgb_3-3_train1.sgrd -POLYGONS=/tmp/RtmpK0j1RP/run/rgb_3-3_train1.shp -NAMING=1 -METHOD=2 -PARALLELIZED=1 -RESULT=/tmp/RtmpK0j1RP/run/rgb_3-3_train1.shp -COUNT=1 -MIN=1 -MAX=1 -RANGE=1 -SUM=1 -MEAN=1 -VAR=1 -STDDEV=1 -QUANTILE=0
  
  # saga <- giLinks$saga
  # sagaCmd<-saga$sagaCmd
  # invisible(env<-RSAGA::rsaga.env(path = saga$sagaPath))
  # RSAGA::rsaga.geoprocessor(lib = "shapes_grid", module = 2, 
  #                           param = list(GRIDS = file.path(R.utils::getAbsolutePath(path_run),paste0(x[i],".sgrd")),
  #                                        POLYGONS = file.path(R.utils::getAbsolutePath(path_run),spdf),
  #                                        NAMING = 1,
  #                                        METHOD = 2,
  #                                        COUNT =  count,
  #                                        MIN  =  min,
  #                                        MAX =  max,
  #                                        SUM  = sum,
  #                                        RANGE = range,
  #                                        MEAN  =  mean,
  #                                        VAR = var,
  #                                        STDDEV = stddev,
  #                                        QUANTILE = quantile,
  #                                        PARALLELIZED = parallel,
  #                                        RESULT = file.path(R.utils::getAbsolutePath(path_run),basename(x[i]),"Stat.shp")
  #                                        ),
  #                           show.output.on.console = FALSE,invisible = TRUE,
  #                           env = env)
  # tmp<-shp2so(file.path(R.utils::getAbsolutePath(paste0(path_run,"/",basename(spdf)))))
  # tmp <- rgdal::readOGR(dsn = file.path(R.utils::getAbsolutePath(path_run)),
  #                       layer = tools::file_path_sans_ext(basename(spdf)), 
  #                       verbose = FALSE)
  # rgdal::writeOGR(obj    = tmp,
  #                 layer  = tools::file_path_sans_ext(basename(spdf)),
  #                 driver = "ESRI Shapefile",
  #                 dsn    = file.path(R.utils::getAbsolutePath(path_run)),
  #                 overwrite_layer = TRUE,verbose = FALSE)
  # 
  ret <-  system(paste0(sagaCmd, " shapes_grid 2 ",
                        " -GRIDS ",file.path(R.utils::getAbsolutePath(path_run),paste0(rasternames[i],".sgrd")),
                        " -POLYGONS ",file.path(R.utils::getAbsolutePath(path_run),spdfshp),
                        " -NAMING 0",
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
                        " -PARALLELIZED ",parallel,
                        " -RESULT ",file.path(R.utils::getAbsolutePath(path_run),paste0(basename(rasternames[i]),"Stat.shp"))),
                 intern = TRUE)
  stat1<-shp2so(file.path(R.utils::getAbsolutePath(paste0(path_run,"/",paste0(basename(rasternames[i]),"Stat")))))
  # stat1 <- rgdal::readOGR(dsn = file.path(R.utils::getAbsolutePath(path_run)),
  #                         layer = paste0(basename(rasternames[i]),"Stat"), 
  #                         verbose = TRUE)
  
  names(stat1) <- gsub(names(stat1),pattern = "\\.",replacement = "")
  #TODO
  if (nchar(rasternames[i]) > 6 ) rasternames[i] <-substr(rasternames[i],1,6)
  if (i<10)  tmp_names <- gsub(names(stat1),pattern = paste0("G0",i),replacement = rasternames[i])
  else if (i<10)  tmp_names <- gsub(names(stat1),pattern = paste0("G",i),replacement = rasternames[i])
  names(stat1)<-substr(tmp_names , 1,10)
  #raster::shapefile(stat1,file.path(R.utils::getAbsolutePath(path_run),"polystat"),overwrite = TRUE)
  
  
  if (i == 1) {
    stat <- stat1
    
  } else  {
    stat@data <- cbind(stat@data,stat1@data[4:length(names(stat1))])
    #stat <- stat1
  }
}
sp::proj4string(stat)<-proj
so2shp(stat,paste0(basename(rasternames[i]),"Stat"),file.path(R.utils::getAbsolutePath(paste0(path_run))))

return(stat)
}


#'  convenient function to establish all link2GI links
#' @description brute force search, find and linkl of all link2GI link functions
#'
#' @note You may also use the full parameterization of the \code{link2GI} package, but you are strungly advaced to use the \code{link2GI} functions in a direct way.
#' @param links character. links
#' @param linkItems character. list of c("saga","grass7","otb","gdal")
#' @param simple logical. true  make all
#' @param sagaArgs character. full string of sagaArgs
#' @param grassArgs character. grassArgs full string of grassArgs
#' @param otbArgs character. full string of otbArgs
#' @param gdalArgs character. full string of gdalArgs
#'
#'@examples
#'\dontrun{
#' # required packages
#' require(uavRst)
#' require(link2GI)
#'
#' # search, find and create the links to all supported  GI software
#' giLinks<-uavRst::linkAll()
#'

#'}

#' @export
linkAll <- function(links=NULL,
                    simple = TRUE,
                    linkItems = c("saga","grass7","otb","gdal"),
                    sagaArgs = "default",
                    grassArgs = "default",
                    otbArgs =   "default",
                    gdalArgs =  c("quiet = TRUE,
                                     returnPaths = TRUE")
)  {
  cat("linking ", links,"\n")
  if (sagaArgs == "default") sagaArgs <- "default_SAGA = NULL, searchLocation = 'default', ver_select = FALSE, quiet = TRUE, returnPaths = TRUE"
  if (grassArgs == "default") grassArgs <- "x = NULL, default_GRASS7 = NULL, search_path = NULL, ver_select = FALSE, gisdbase_exist = FALSE, gisdbase = NULL,
                                     location = NULL, spatial_params = NULL, resolution = NULL, quiet = TRUE, returnPaths = FALSE"
  if (otbArgs == "default") otbArgs <- "bin_OTB = NULL, root_OTB = NULL, type_OTB = NULL, searchLocation = NULL, ver_select = FALSE"
  if (gdalArgs == "default") gdalArgs <- "quiet = TRUE, returnPaths = TRUE"
  if (is.null(links) && (simple)){
    link<-list()
    for (links in linkItems) {
      cat("linking ", links,"\n")
      if (links=="gdal") 
        link[[links]]<-assign(links,eval(parse(text=paste("link2GI::link",toupper(links),"(returnPaths = T)",sep = ""))))
      else
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


# creates names and ranges from a simple list for zrange cuts ---
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

# extract pixel values according to an overlay and response name ---
extractTrainPixelValues <- function(imgStack=NULL,trainData=NULL,responseCol=NULL){
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

# calculate the mode ---
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

### getPopupStyle creates popup style  -------
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
#' Split multiband image to single band SAGA files
#' @description Split multiband image to single band SAGA files. If a reference file is given, it performs a resample if necessary to avoid the numerical noise problem of SAGA extent.
#' @param fn character. filename
#' @param bandname character. list of bandnames c("red","green","blue")
#' @param startBand numerical. first band to export
#' @param startBand numerial. last band to export
#' @param refFn character. reference image for resampling
#' @param returnRaster logical. return as raster
#' @name split2SAGA

#' @keywords internal
#'@export
split2SAGA<-function(fn=NULL,
                     bandName=NULL,
                     startBand= 1,
                     endBand =3,
                     refFn=NULL,
                     returnRaster=FALSE){
  flist<-list()
  for (i in seq(startBand:endBand)){
    outFn<-file.path(R.utils::getAbsolutePath(path_run),paste0(bandName[i],".sdat"))
    #raster::writeRaster(raster::raster(fn),outFn,overwrite = TRUE,NAflag = 0,process="text")
    if (!is.null(refFn))
      r<-raster::raster(refFn)
    else
      r<-raster::raster(fn)
    res<-gdalUtils::gdal_translate(src_dataset = fn[[i]]@file@name,
                                   dst_dataset = outFn,
                                   tr= paste0(raster::xres(r)," ",
                                              raster::xres(r)),
                                   b = as.character(i),
                                   of = "SAGA",
                                   a_nodata = 0,
                                   a_srs = as.character(r@crs) )
    r<-raster::writeRaster(raster::resample(raster::raster(outFn),raster::raster(refFn)),
                           filename	= outFn,
                           NAflag = 0,
                           format="SAGA",
                           overwrite=TRUE,progress="text")
    flist<-append(flist, r)
  }
  if (returnRaster) return(flist)
}

#' colorize the cat outputs
#'@description colorize the cat outputs
#'@export
#'@keywords internal
getCrayon<-function(){
  head <- crayon::black $ bgGreen
  err  <- crayon::red $ bold
  note <- crayon::blue $ bold
  ok   <- crayon::green $ bold
  return(list(note,err,ok,head))
}
#' create name vector corresponding to the training image stack
#' create vector containing the names of the image stack claculated using \code{\link{calc_ext}}
#' @param rgbi character. codes of the RGB indices
#' @param bandNames character.  band names
#' @param stat character.  stat codes
#' @param morpho character.  morpho codes
#' @param edge character.  edge codes
#' @param rgbTrans character.  rgbTrans codes
#' @param dem charater. dem codes
#' @keywords internal
#'
#' @export make_bandnames

make_bandnames <- function(rgbi    = NA,
                           bandNames = NA,
                           stat    = FALSE,
                           morpho  = NA,
                           edge    = NA ,
                           rgbTrans = NA,
                           dem =    NA){
  if (!is.na(rgbi[1])) bandNames <- append(c("red","green","blue"),rgbi)
  if (!is.na(bandNames[1])) {
    if(bandNames[1] == "simple"){
      bandNames <- c("Energy", "Entropy", "Correlation",
                     "Inverse_Difference_Moment", "Inertia",
                     "Cluster_Shade", "Cluster_Prominence",
                     "Haralick_Correlation")
    } else if(bandNames[1] == "advanced"){
      bandNames <- c("Hara_Mean", "Hara_Variance", "Dissimilarity",
                     "Sum_Average",
                     "Sum_Variance", "Sum_Entropy",
                     "Difference_of_Variances",
                     "Difference_of_Entropies",
                     "IC1", "IC2")
    } else if(bandNames[1] == "higher"){
      bandNames <- c("Short_Run_Emphasis",
                     "Long_Run_Emphasis",
                     "Grey-Level_Nonuniformity",
                     "Run_Length_Nonuniformity",
                     "Run_Percentage",
                     "Low_Grey-Level_Run_Emphasis",
                     "High_Grey-Level_Run_Emphasis",
                     "Short_Run_Low_Grey-Level_Emphasis",
                     "Short_Run_High_Grey-Level_Emphasis",
                     "Long_Run_Low_Grey-Level_Emphasis",
                     "Long_Run_High_Grey-Level_Emphasis")
    } else if(bandNames[1] == "all"){
      bandNames <- c("Energy", "Entropy", "Correlation",
                     "Inverse_Difference_Moment", "Inertia",
                     "Cluster_Shade", "Cluster_Prominence",
                     "Haralick_Correlation",
                     "Hara_Mean", "Hara_Variance", "Dissimilarity",
                     "Sum_Average",
                     "Sum_Variance", "Sum_Entropy",
                     "Difference_of_Variances",
                     "Difference_of_Entropies",
                     "IC1", "IC2",
                     "Short_Run_Emphasis",
                     "Long_Run_Emphasis",
                     "Grey-Level_Nonuniformity",
                     "Run_Length_Nonuniformity",
                     "Run_Percentage",
                     "Low_Grey-Level_Run_Emphasis",
                     "High_Grey-Level_Run_Emphasis",
                     "Short_Run_Low_Grey-Level_Emphasis",
                     "Short_Run_High_Grey-Level_Emphasis",
                     "Long_Run_Low_Grey-Level_Emphasis",
                     "Long_Run_High_Grey-Level_Emphasis")
    }
  }
  if (stat == TRUE)  {
    bandNames    = c("Stat_Mean","Stat_Variance", "Skewness", "Kurtosis")
  }
  if (!is.na(dem))  {
    bandNames    =  dem
  }
  
  if (!is.na(morpho))  {
    bandNames    =  morpho
  }
  
  if (!is.na(edge))  {
    bandNames    =  edge
  }
  if (!is.na(rgbTrans))  {
    if (rgbTrans %in% c("Gray"))
      bandNames    =  bandNames <- c(paste0(rgbTrans,"_b1"))
    else
      bandNames    =  bandNames <- c(paste0(rgbTrans,"_b1"),paste0(rgbTrans,"_b2"),paste0(rgbTrans,"_b3"))
  }
  return(bandNames)
  
}

# returns the saga items from a list --
issagaitem <- function(x)
{
  if (x %in%  c("SLOPE","ASPECT","C_GENE","C_PROF","C_PLAN","C_TANG","C_LONG","C_CROS","C_MINI","C_MAXI","C_TOTA","C_ROTO","MTPI") ) return(TRUE) else return(FALSE)
}

# returns the gdal items from a list ---
isgdaldemitem <- function(x)
{
  if (x %in%  c("hillshade","slope", "aspect","TRI","TPI","Roughness")) return(TRUE) else return(FALSE)
}

#' clips a tif files according to a given extent.
#'
#' clips a tif files according to a given extent
#' @param rasterFiles character. vector containing a list of rasterfiles to be clipped
#' @param ext extent
#' @param outPath character. subfolder of current runtime folder. clipped files will be stored there
#' @param prefix character. prefic string that is added to the filenames
#'@export
#'@keywords internal
cutTif<- function(rasterFiles = NULL,
                  ext=NULL,
                  outPath="cut",
                  prefix="cut") {
  #rasterFiles <- list.files(pattern="[.]tif$", path="/home/creu/proj/geopat/data/modis_carpathian_mountains/study_area/modis_ndvi/2002", full.names=TRUE)
  te=paste(raster::extent(ext)[1],' ',
           raster::extent(ext)[3],' ',
           raster::extent(ext)[2],' ',
           raster::extent(ext)[4])
  if (!file.exists(file.path(R.utils::getAbsolutePath(path_run), outPath))) dir.create(file.path(file.path(R.utils::getAbsolutePath(path_run), outPath)), recursive = TRUE,showWarnings = FALSE)
  for (rasterFile in rasterFiles) {
    system(paste0("gdal_translate -projwin ", te, " -of GTiff ",rasterFile, " ", file.path(R.utils::getAbsolutePath(path_run)),outPath,"/",prefix,basename(rasterFile)))
  }
}

searchLastools <- function(MP = "~",
                           quiet=TRUE) {
  if (MP=="default") MP <- "~"
  MP<-path.expand(MP)
  if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv())
  # trys to find a osgeo4w installation at the mounting point  disk returns root directory and version name
  # recursive dir for otb*.bat returns all version of otb bat files
  if (!quiet) cat("\nsearching for lastools windows binaries - this may take a while\n")
  if (!quiet) cat("For providing the path manually see ?searchLastools \n")
  raw_LAS <- system2("find", paste(MP," ! -readable -prune -o -type f -iname 'lasview.exe' -print"),stdout = TRUE)
  if (!grepl(MP,raw_LAS)[[1]]) stop("\n At ",MP," no LAStool binaries found")
  # trys to identify valid otb installations and their version numbers
  LASbinaries <- lapply(seq(length(raw_LAS)), function(i){
    
    # TODO strip version from OTB /usr/bin/otbcli_BandMath -version
    # "This is the BandMath application, version 6.0.0"
    
    # if the the tag "OSGEO4W64" exists set installation_type
    root_dir <- data.frame(binDir = substr(raw_LAS[i],1, gregexpr(pattern = "lasview.exe", raw_LAS[i])[[1]][1] - 1))
    # put the existing GISBASE directory, version number  and installation type in a data frame
    #data.frame(binDir = root_dir,lastoolCmd = paste0(root_dir,"lasview.exe"), stringsAsFactors = FALSE)
  }) # end lapply
  # bind the df lines
  otbInstallations <- do.call("rbind", LASbinaries )
  
  
  return(LASbinaries)
}

# if (substr(Sys.getenv("COMPUTERNAME"),1,5) == "PCRZP") {
#   gdalUtils::gdal_setInstallation(search_path = shQuote("C:/Program Files/QGIS 2.14/bin/"))
# } else {
#   ## (gdalUtils) check for a valid GDAL binary installation on your system
#   if (!quiet) gdalUtils::gdal_setInstallation(verbose = TRUE)
#   else gdalUtils::gdal_setInstallation()
# }


#'@title Checks if running on a specified computer domain
#'@name setHomePath
#'@description  Checks if the computer name belongs to a specified group i.e. aq network domain Marburg Universitys computer pools
#'@param homeDir full path  to the real folder location the project
#'@param prefixPC contains  an arbitrary part of the computer name. It always starts with the first letter.
#'@author CR
#'@keywords internal
#'@examples
#' \dontrun{
#' # add path
#' setHomePath("saga",prefixPC="PCRZP")
#' }
#'@export setHomePath
setHomePath<- function(homeDir="F:/MPG", prefixPC="PCRZP") {
  if (!exists("GiEnv")) GiEnv <- new.env(parent=globalenv()) 
  if (substr(Sys.getenv("COMPUTERNAME"),1,nchar(prefixPC)) == substr(prefixPC,1,nchar(prefixPC))) {
    projHomeDir <- shQuote(homeDir)
    return(path.expand(projHomeDir))
  } else {
    return(path.expand("~/edu"))  
  }
} 

#' returns the status of saga modules
#' @param module character name of module to be checked
#' @param file character. filename to be checked
#' @param listname character. name of log list

#'@keywords internal
#'@export
fileProcStatus <- function(module=NULL,file= NULL,listname=NULL){
  if (!exists("path_run")) path_run = tempdir()
  assign(listname,list())
  if (file.exists(file.path(R.utils::getAbsolutePath(path_run),file))) {
    eval(parse(text=paste0(listname,"$",module," <- TRUE")))
    return(eval(parse(text=listname)))
  }
  else {
    eval(parse(text=paste0(listname,"$",module," <- FALSE")))
    return(eval(parse(text=listname)))
    cat(eval(parse(text=listname)))
  }
}
#' writes shapefiles from  sf or sp* objects
#' @param spobj spatial object of type sp* or sf
#' @param name character. name of object to be created
#' @param path_run character. path used for runtime operations

#'@keywords internal
#'@export
so2shp<-function(spobj,
                 name="spobj",
                 path_run = tempdir()){
  
  if(class(spobj[[1]])  %in% c("SpatialPolygons","SpatialPolygonsDataFrame","SpatialPointsDataFrame","SpatialLinesDataFrame","SpatialLines","SpatialPoints"))  {
    raster::shapefile(spobj,file.path(R.utils::getAbsolutePath(path_run),paste0(name,".shp")))
  } else if(class(spobj)[[1]]  == "sf")  {
    # read vector input data the sf way
    sf::st_write(obj=spobj,dsn= path_run ,layer=paste0(name,".shp"),driver="ESRI Shapefile",quiet = TRUE, delete_dsn=TRUE)
    
    
  }
}
#' reads shapefiles as sf or sp objects
#' @param fn filename optionally with path
#' @param type character. type of object to be created. "sp" is default can be set to "sf"
#' @param path_run character. path used for runtime operations
#'@keywords internal
#'@export
shp2so<-function(fn,
                 type="sp",
                 path_run = tempdir()){
  fname<- tools::file_path_sans_ext(basename(fn))
  if (nchar(dirname(fn))>1) path_run <- dirname(fn)
  
  
  
  if(type=="sp")  {
    spobj<-raster::shapefile(file.path(R.utils::getAbsolutePath(path_run),paste0(fname,".shp")))
  } else if(class(spobj)[[1]]  == "sf")  {
    # read vector input data the sf way
    spobj<-sf::st_read(dsn= file.path(path_run ,layer=paste0(fname,".shp")),quiet = TRUE)
    
    
  }
  return(spobj)
}
#' writes raster* objects to the SAGA format
#' @param rastobj raster* object
#' @param path_run character. path used for runtime operations
#'@keywords internal
raster2sdat<-function(rastobj, 
                      path_run = tempdir()
){
  if (class(rastobj) %in% c("RasterLayer")) {
    raster::writeRaster(rastobj,file.path(R.utils::getAbsolutePath(path_run),paste0(names(rastobj),".sdat")),overwrite = TRUE,NAflag = 0)    
  } else if (class(rastobj) %in% c("RasterStack", "RasterBrick")) {
    i=1
    for (item in names(rastobj)){
      raster::writeRaster(rastobj[[i]],
                          file.path(R.utils::getAbsolutePath(path_run),paste0(item,".sdat")),
                          overwrite = TRUE,
                          NAflag = 0)
      i=i+1
    }
  }
}


#' converts SAGA raster to R raster object
#' @description converts SAGA raster to R raster object
#' @param fn filname without extension
#' @param ext extent of the raster in R notation
#' @param path_run character. path used for runtime operations
#' @keywords internal
#' @export
saga2r<- function(fn,ext,path_run=tempdir()) {
  gdalUtils::gdalwarp(paste0(path_run,fn,".sdat"), 
                      paste0(path_run,fn,".tif"), 
                      overwrite = TRUE,  
                      verbose = FALSE)
  x<-raster::raster(paste0(path_run,fn,".tif"))
  x@extent <- ext
  # convert to SAGA
  return(x)
}

#' converts R raster* objects to SAGA raster
#' @param x raster object
#' @param fn filname name without extension
#' @param path_run character. path used for runtime operations
#' @keywords internal
#' @export
r2saga <- function(x,fn,path_run=tempdir()) {
  fname<- tools::file_path_sans_ext(basename(fn))
  dirname<-dirname(fn)
  if (nchar(dirname)>1) path_run<-dirname
  
  raster::writeRaster(x,file.path(R.utils::getAbsolutePath(path_run),paste0(fn,".sdat")),bylayer=TRUE,overwrite = TRUE,NAflag = 0)
  
}