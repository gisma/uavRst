#'@name initRGIS
#'
#'@title Initializes RSAGA, rgrass7 and gdalUtil
#'
#'@description Function that initializes environment, pathes, SAGA, GRASS7 and GDAL support via gdalUtils.
#'             *NOTE* you probably have to customize some settings according to your personal installation folders.
#'@details The concept is very straightforward but for an all days usage pretty helpful. You need to provide a GDAL conform raster file, a \link{raster} object or you may download SRTM data with \link{getGeoData}. This settings will be used to initialize a temporary but static \href{https://cran.r-project.org/web/packages/rgrass7}{rgrass7} environment. Additionally \href{https://cran.r-project.org/web/packages/RSAGA}{RSAGA} and \href{https://cran.r-project.org/web/packages/gdalUtils}{gdalUtils} are initialized and checked. During the rsession you will have full access to GRASS7 and SAGA via the wrapper packages. .
#'
#'@usage initRGIS(fname.control,)
#'
#'@param root.dir  project directory
#'@param working.dir runtime directory
#'@param fndem name of a (DEM) GDAL format raster file
#'@param rasterParam a rasterobject link
#'
#'@author Chris Reudenbach
#'\email{reudenbach@@gis-ma.org}
#'
#'
#'@return initRGIS initializes the usage of GRASS7 RSAGA 2.12+ and gdalUtil\cr
#' it returns the RSAGA env, the GRASS env and the gdalUtils status
#'@export initRGIS
#'
#'@examples
#'#### Examples how to use RSAGA and GRASS bindings from R
#'
#' # full call using getGeodata
#' envGIS<- initRGIS <- function(root.dir=tempdir(),working.dir='cost',fndem=getGeoData("SRTM",xtent=c(49,49,11,11)))
#'
#' # initialisation with an existing raster object
#' envGIS<- initRGIS("~/proj/tester",'cost',fndem=NULL,rasterParam = outraster)
#'
#' # init with an local GDAL file
#' envGIS<- initRGIS("~/proj/tester",'cost',"~/proj/runbeetle/srtm_43_01.tif")

#'
#'

initRGIS <- function(root.dir=tempdir(),working.dir='cost',fndem=NULL, rasterParam=NULL){
  cat("GRASS init...")
  # create directory
  if (!file.exists(file.path(root.dir, working.dir))){
    dir.create(file.path(root.dir, working.dir),recursive = TRUE)
  }
  # setting System Temporary folder to the current working directory
  Sys.setenv(TMPDIR=file.path(root.dir, working.dir))
  Sys.setenv(GRASS_ADDON_PATH="~/.grass7/addons")

  # (R) set R working directory
  setwd(file.path(root.dir, working.dir,"tmp"))

  # analyse input for extent and projection
  if (is.null(fndem) && is.null(rasterParam)) {
    stop("no information from raster data neither rasterParam ")
  } else if (!is.null(fndem)) {
     rasterParam<-raster::raster(fndem)
    resolution <- res(rasterParam)[1]
    proj4 <- as.character(rasterParam@crs)
    ymax<-rasterParam@extent@ymax
    ymin<-rasterParam@extent@ymin
    xmax<-rasterParam@extent@xmax
    xmin<-rasterParam@extent@xmin
    fn<-rasterParam@file@name
    if (fn == "") {fn<-fndem@file@name}
    if (fn == "") {fn<-paste0(file.path(root.dir,"data"),"/",names(fndem),".tif")}
  }

  if (class(rasterParam) == "SpatialPoints" ){
    s<-as.character(rasterParam@proj4string)
    s2<-(strsplit(s,split = " "))
    proj4<- paste(s2[[1]][2:length(unlist(s2))], collapse = ' ')
    xmax<-rasterParam@bbox[3]
    xmin<-rasterParam@bbox[1]
    ymax<-rasterParam@bbox[4]
    ymin<-rasterParam@bbox[2]
    resolution<-0.0008333333
    fn<'no_raster_provided'
  } else{
    resolution <- res(rasterParam)[1]
    proj4 <- as.character(rasterParam@crs)
    ymax<-rasterParam@extent@ymax
    ymin<-rasterParam@extent@ymin
    xmax<-rasterParam@extent@xmax
    xmin<-rasterParam@extent@xmin
      }

  # (R) set pathes  of SAGA/GRASS modules and binaries depending on OS WINDOWS is set to GIS2GO
  if(Sys.info()["sysname"] == "Windows"){
    os.saga.path<-  'C:/MyApps/GIS_ToGo/QGIS_portable_Chugiak_24_32bit/QGIS/apps/saga'
    saga.modules<-  'C:/MyApps/GIS_ToGo/QGIS_portable_Chugiak_24_32bit/QGIS/apps/saga/modules'
    grass.gis.base<-'C:/MyApps/GIS_ToGo/GRASSGIS643/bin'
  }else {
    os.saga.path<-  '/usr/bin'
    saga.modules<-  '/usr/lib/x86_64-linux-gnu/saga/'
    grass.gis.base<-'/usr/lib/grass70'
  }

  # create temporary GRASS location
  rgrass7::initGRASS(gisBase=grass.gis.base,
                     home=tempdir(),
                     mapset='PERMANENT',
                     override=TRUE
                     )

  # assign projection
  rgrass7::execGRASS('g.proj',
                     flags=c('c','quiet'),
                     proj4=proj4
                     )

  # assign extent
  rgrass7::execGRASS('g.region',
                     flags=c('quiet'),
                     n=as.character(ymax),
                     s=as.character(ymin),
                     e=as.character(xmax),
                     w=as.character(xmin),
                     res=as.character(resolution)
                     )

  
  # assign extent
 # rgrass7::execGRASS('g.region',
#                     flags=c("p","quiet"),
#                     raster=basename(tools::file_path_sans_ext(fndem))
  #)
  envGRASS<-rgrass7::gmeta()

  ## (gdalUtils) check for a valid GDAL binary installation on your system
  gdalUtils::gdal_setInstallation()
  valid.install<-!is.null(getOption("gdalUtils_gdalPath"))
  if (!valid.install){
    stop('no valid GDAL/OGR found')
  }else {
    gdal<-'gdalUtils status is ok'
  }

  # (RSAGA) set SAGA environment
  envRSAGA=RSAGA::rsaga.env(check.libpath=FALSE,
                            check.SAGA=FALSE,
                            workspace=file.path(root.dir, working.dir),
                            os.default.path=os.saga.path,
                            modules=saga.modules)


  result=list(envRSAGA,envGRASS,gdal,fn,file.path(root.dir, working.dir))
  names(result)=c('saga','grass','gdal','fn','runDir')
  return(result)
}