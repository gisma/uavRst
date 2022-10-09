#' calculates most important DEM parameters
#'
#' @note please provide a GeoTiff file
#' @param dem  character filname to GeoTiff containing one channel DEM
#' @param item character list containing the keywords of the DEM parameter to be calculated. Default parameter are c("hillshade", "slope", "aspect", "TRI", "TPI", "Roughness", "SLOPE", "ASPECT", "C_GENE", "C_PROF", "C_PLAN", " C_TANG"," C_LONG", "C_CROS", "C_MINI", "C_MAXI", "C_TOTA", "C_ROTO", "MTPI")
#' @param verbose logical. be quiet
#' @param morphoMethod  numeric. saga morphometric method  see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_0.html}{SAGA GIS Help}. GDAL parameters see also: \href{https://www.gdal.org/gdaldem.html}{gdaldem}
#' @param minScale  numeric. in scale for multi scale TPI see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_28.html}{SAGA GIS Help}
#' @param maxScale  numeric. max scale for multi scale TPI see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_28.html}{SAGA GIS Help}
#' @param numScale  numeric. number of scale for multi scale TPI see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_28.html}{SAGA GIS Help}
#' @param retRaster boolean if TRUE a raster stack is returned
#' @param gdalLinks    list. of GDAL tools cli pathes
#' @param sagaLinks    list. of SAGA tools cli pathes
#' @param gdalLinks     list. GDAL tools cli paths 
#' @return raster* object
#' @export morpho_dem
#' @examples
#'\dontrun{
#' ##- required packages
#' require(link2GI)
#' setwd(tempdir())
#' ## check if OTB exists
#' gdal <- link2GI::linkGDAL()
#' saga <- link2GI::linkSAGA()
#' if (gdal$exist & saga$exist) {
#' data("mrbiko")
#' proj = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
#' mrbiko <- raster::projectRaster(mrbiko, crs = proj,method = "ngb",res = 20)
#' raster::writeRaster(mrbiko,"dem.tif",overwrite=TRUE)
#' r<-morpho_dem(dem="dem.tif",c("hillshade", "slope", "aspect", "TRI", "TPI",
#'                               "Roughness", "SLOPE", "ASPECT",  "C_GENE", "C_PROF",
#'                               "C_PLAN", " C_TANG"," C_LONG", "C_CROS"),
#'                               gdalLinks= gdal,sagaLinks=saga)
#' r_st=raster::stack(r)
#' names(r_st)=names(r)
#' raster::plot(r_st)
#' }
#' }


morpho_dem<- function(dem,
                      item=c("hillshade","slope", "aspect","TRI","TPI","Roughness","SLOPE","ASPECT", "C_GENE","C_PROF","C_PLAN"," C_TANG"," C_LONG","C_CROS","C_MINI","C_MAXI","C_TOTA","C_ROTO","MTPI"),
                      verbose=FALSE,
                      morphoMethod = 6,
                      minScale = 1,
                      maxScale = 8,
                      numScale = 2,
                      retRaster = TRUE,
                      gdalLinks = NULL,
                      sagaLinks = NULL) {
  
  
  if (nchar(dirname(dem))>0){
    dem<basename(dem)
    path_run <- dirname(dem)
  }
  else path_run <- tempdir()
  
  retStack<-list()
  dem<-basename(dem)
  if (is.null(sagaLinks))   saga<- link2GI::linkSAGA()
  else saga<-sagaLinks
  
  if (is.null(gdalLinks)){
    gdal<-link2GI::linkGDAL()
  } else gdal<- gdalLinks
  path_gdal<- gdal$path
  
  sagaCmd<-saga$sagaCmd
  
  
  issaga <- Vectorize(issagaitem)
  isgdal <- Vectorize(isgdaldemitem)
  saga_items<-item[issaga(item)]
  gdal_items<-item[isgdal(item)]
  
  
  if (Sys.info()["sysname"]=="Windows") saga$sagaPath <- utils::shortPathName(saga$sagaPath)
  invisible(env<-RSAGA::rsaga.env(path =saga$sagaPath))
  s<-raster::raster(file.path(path_run,dem))
  y<-raster::yres(s)
  x<-raster::xres(s)
  
  g<- link2GI::linkGDAL()
  system(paste0(g$path,'gdalwarp -overwrite -q ',
                "-te ",  extent(s)[1],' ',
                extent(s)[3],' ',
                extent(s)[2],' ',
                extent(s)[4],' ',
                "-tr ",x,' ', y,' ',
                file.path(path_run,dem),' ',
                file.path(path_run,"dem2.tif"),' '
  )
  )
  
  for (item in gdal_items){
    message(getCrayon()[[1]](":::: processing ",item,"\n"))
    
    system(paste0(g$path,'gdaldem ',item,' ',
                  "-q -compute_edges",' ',
                  file.path(R.utils::getAbsolutePath(path_run),paste0("dem2.tif")),' ',
                  output = file.path(R.utils::getAbsolutePath(path_run),paste0(item,".tif"))
    )
    )
    
    if (retRaster) retStack[[item]]<-assign(item,raster::stack(file.path(R.utils::getAbsolutePath(path_run),paste0(item,".tif"))))
  }
  
  if (length(saga_items>0))  {
    rdem<-raster::raster(file.path(R.utils::getAbsolutePath(path_run),'dem2.tif'))
    raster::writeRaster(rdem,file.path(R.utils::getAbsolutePath(path_run),"SAGA_dem.sdat"),overwrite = TRUE,NAflag = 0)
    # claculate the basics SAGA morphometric params
    message(getCrayon()[[1]](":::: processing ",saga_items,"\n"))
    if (length(saga_items>0) )  { #&& !("MTPI" %in% saga_items)
      RSAGA::rsaga.geoprocessor(lib = "ta_morphometry", module = 0,
                                param = list(ELEVATION = file.path(R.utils::getAbsolutePath(path_run),"SAGA_dem.sgrd"),
                                             UNIT_SLOPE = 1,
                                             UNIT_ASPECT = 1,
                                             SLOPE =  file.path(R.utils::getAbsolutePath(path_run),"SLOPE.sgrd"),
                                             ASPECT = file.path(R.utils::getAbsolutePath(path_run),"ASPECT.sgrd"),
                                             C_GENE = file.path(R.utils::getAbsolutePath(path_run),"C_GENE.sgrd"),
                                             C_PROF = file.path(R.utils::getAbsolutePath(path_run),"C_PROF.sgrd"),
                                             C_PLAN = file.path(R.utils::getAbsolutePath(path_run),"C_PLAN.sgrd"),
                                             C_TANG = file.path(R.utils::getAbsolutePath(path_run),"C_TANG.sgrd"),
                                             C_LONG = file.path(R.utils::getAbsolutePath(path_run),"C_LONG.sgrd"),
                                             C_CROS = file.path(R.utils::getAbsolutePath(path_run),"C_CROS.sgrd"),
                                             C_MINI = file.path(R.utils::getAbsolutePath(path_run),"C_MINI.sgrd"),
                                             C_MAXI = file.path(R.utils::getAbsolutePath(path_run),"C_MAXI.sgrd"),
                                             C_TOTA = file.path(R.utils::getAbsolutePath(path_run),"C_TOTA.sgrd"),
                                             C_ROTO = file.path(R.utils::getAbsolutePath(path_run),"C_ROTO.sgrd"),
                                             METHOD = morphoMethod),
                                show.output.on.console = FALSE, invisible = TRUE,
                                env = env)}
    if ("MTPI" %in% saga_items){
      if (RSAGA::rsaga.get.version(env = env) >= "3.0.0") {
        
        # calculate multiscale p
        # Topographic Position Index (TPI) calculation as proposed by Guisan et al. (1999).SAGA
        # This implementation calculates the TPI for different scales and integrates these into
        # one single grid. The hierarchical integration is achieved by starting with the
        # standardized TPI values of the largest scale, then adding standardized values from smaller
        # scales where the (absolute) values from the smaller scale exceed those from the larger scale.
        RSAGA::rsaga.geoprocessor(lib = "ta_morphometry", module = 28,
                                  param = list(DEM = file.path(R.utils::getAbsolutePath(path_run),"SAGA_dem.sgrd"),
                                               SCALE_MIN = minScale,
                                               SCALE_MAX = maxScale,
                                               SCALE_NUM = numScale,
                                               TPI = file.path(R.utils::getAbsolutePath(path_run),"MTPI.sgrd")),
                                  show.output.on.console = FALSE,invisible = TRUE,
                                  env = env)
        
      } else {message(getCrayon()[[2]]("\nPlease install SAGA >= 3.0.0\n Run without MTPI...\n"))
        saga_items<-saga_items[  !(saga_items %in% "MTPI")]
        
      }
    }
    for (item in saga_items){
      message(getCrayon()[[1]](":::: converting ",item,"\n"))
      ritem<-raster::raster(file.path(R.utils::getAbsolutePath(path_run),paste0(item,".sdat")))
      raster::writeRaster(ritem,file.path(R.utils::getAbsolutePath(path_run),paste0(item,".tif")), overwrite = TRUE, NAflag = 0)
      if (retRaster) retStack[[item]]<-assign(item,raster::stack(file.path(R.utils::getAbsolutePath(path_run),paste0(item,".tif"))))
    }
    
    
  }
  if (retRaster) return(retStack)
}

# if necessary creates additional folders for the resulting files
getOutputDir<- function (outDir){
  if (!is.null(outDir)) {
    otbOutputDir<-outDir
    if (!file.exists(paste0(otbOutputDir, "/texture/"))) dir.create(file.path(paste0(otbOutputDir, "/texture/")), recursive = TRUE,showWarnings = FALSE)
  } else {
    otbOutputDir<-paste0(getwd(),"/texture/")
    if (!file.exists(paste0(otbOutputDir, "/texture/"))) dir.create(file.path(paste0(otbOutputDir, "/texture/")), recursive = TRUE,showWarnings = FALSE)
  }
  return(otbOutputDir)
}
