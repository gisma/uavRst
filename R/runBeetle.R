#'@name runBeetle
#'@title Wrapper function to analyse the phylogenetic spread of running beetle
#'  via a cost analysis based on morphometry and derived potential surface wetness.
#'
#'@description The phylogenetic spreading of the tibetan carabidae seems to be
#'correlated to valley linkage and available humidity. Long term humidity is
#'bound to precipitation and morphometry. It is obvious that the estimation of
#'spreeading speed and range is in no case a simple euclidian one. runBeetle
#'provides a first better estimation using a cost or friction analysis assuming
#'that the spread is following natural lines of wetness e.g. valleys, humidity
#'gradients...whatever  I'am not a beetle ask the beetle
#'\href{http://www.zoologie.uni-rostock.de/mitarbeiter/joachimschmidt/joachimschmidtpubl}{beetle
#'guy}.
#'
#'@usage runBeetle(rootDir,workingDir="cost",inputData=NULL,costType="tci",
#'  externalCostRaster=NULL,internalCost=TRUE)

#'
#'@author Chris Reudenbach
#' \cr
#' \emph{Maintainer:} Chris REudenbach \email{reudenbach@@uni-marburg.de}

#'
#'@references Schmidt, J., Böhner, J., Brandl, R. & Opgenoorth, L. (in review):
#'  Mass elevation and lee effect override latitudinal effects in determining
#'  the distribution ranges of species: Ground beetles from the Himalaya-Tibet
#'  Orogen. – PLoS ONE.
#'
#' \href{https://grass.osgeo.org/grass7/}{GRASS70}
#' \href{https://sourceforge.net/projects/saga-gis/files/}{SAGA GIS}
#'
#'
#'@param rootDir  project directory\cr
#'@param workingDir working directory\cr
#'@param inputData location data containing obligatory the cols lon,lat and
#'  optional a code col. The format has to be a data frame see example.
#'@param costType used if internalCost = TRUE.  default is "tci"  you can choose
#'  "tci" "dem.filled" or "accu" see details for more information
#'@param externalCostRaster default = NULL you may provide a GTiff file with the
#'  name cost.tif NOTE you have to set internalCost = FALSE
#'@param internalCost default = TRUE switches to external provided GTiff file
#'  wich has to be named cost.tifx
#'@param dump default = FALSE if TRUE export r.terraflow products to GTiff
#'@param usedump default = FALSE instead of running  r.terraflow again use
#'  products products to GTiff
#'
#'@details  The core of the analysis is an isotropic/anisotropic least cost path
#'  calculation. By default the cost surface is assumed to be a local derivate
#'  of the morphometry with respect to the potential soil humidity. A perfect
#'  approch to derive such information is the use of a Digital Elevation Model
#'  DEM and some corresponding derivates as the Topographic Convergence Index.\cr
#'
#'  If you choose "tci"  (default) the cost surface provides an estimation of
#'  rainwater runoff availability to plants based on specific catchment area (A)
#'  and local slope (b) such that TCI = ln(A/tan b) (Beven & Kirkby, 1979). This
#'  seems be pretty straightforward and fairly suitable for the beetles
#'  "behaviour.\cr
#'
#'  If you chosse "accu" a typical accumulation cost grid from the original DEM
#'  will be used\cr
#'
#'  If you choose "dem.filled" a typical accumulation cost grid from the
#'  hydrologically corrected DEM will be used\cr
#'
#'  For the r.walk algorithm the non accumulated data is used.

#'@return runBeetle returns:\cr
#' (a) a dataframe with the (a) euclidian distances, (b) cost distance (isotropic cost surface) and (c) the walk distance (anisotropic cost surface)\cr
#  Additionally for each source point and cost analysis type (walk/drain) a seperate shapefile containing the same information and geometry is created
#'
#'
#'@export runBeetle
#'@examples
#'
#'\dontrun{
#'#### NOTE: You obligatory need GRASS70
#'
#' library(doParallel)
#' library(foreach)
#' library(raster)
#' library(sp)
#' library(gdalUtils)
#' library(rgdal)
#'
#' ### NOTE: the area is specifified and automatically downloaded (SRTM) by the input data extent plus a "zone"
#' ### read a csv file
#' fn<-system.file("bug.csv", package="biomix")
#' beetleLocs<-read.csv2(fn,header = TRUE,sep = ',',dec = '.',stringsAsFactors=FALSE)
#'
#' ### use some arbitrary locations from scratch
#' beetleLocs <- as.list(c("86.83", "28.20", "100","84.58", "28.67", "200" ,"83.87", "28.80", "300"))
#' beetleLocs <- data.frame(matrix(as.numeric(unlist(beetleLocs)), nrow=3, byrow=T),stringsAsFactors=FALSE)
#' colnames(beetleLocs)<-c("lon","lat","code")
#' beetleDist<-runBeetle(rootDir = "/home/creu/proj/beetle" ,inputData = beetleLocs, usedump=TRUE, walk=TRUE)
#' }


runBeetle <-function(rootDir,
                     workingDir="cost",
                     inputData=NULL,
                     costType="tci",
                     internalCost=TRUE,
                     walk=FALSE,
                     dump=FALSE,
                     usedump=FALSE,
                     memSize=300,
                     externalCostRaster=NULL){

  if (usedump){
    internalCost<-FALSE
    externalCostRaster<-paste0(file.path(rootDir, workingDir),"/cost.tif")
    envGIS<- initRGIS(root.dir=rootDir,working.dir=workingDir,fndem=externalCostRaster)
    cat("i will take the existing results from a former r.terraflow analysis. \n")
    Tiff2G(runDir=envGIS$runDir,layer="cost")
    Tiff2G(runDir=envGIS$runDir,layer="accu")
    Tiff2G(runDir=envGIS$runDir,layer="filled")
    Tiff2G(runDir=envGIS$runDir,layer="accudir")
    Tiff2G(runDir=envGIS$runDir,layer="tci")
    Tiff2G(runDir=envGIS$runDir,layer="dem")
  }
  if (internalCost){
    usedump<-FALSE
  }

  # check if input data is correct
  if (class(inputData) == "data.frame"){
    tmp<-inputData

    # drop all attributes except lon lat
    keeps <- c("lon","lat")
    tmp<-tmp[keeps]

    # make it spatial
    coordinates(tmp)<- ~lon+lat
    proj4string(tmp)<- CRS("+proj=longlat +datum=WGS84")

    # get extent for data retrieval
    xtent<-extent(tmp)

    # remove duplicate locations
    uniqueLocations <-remove.duplicates(tmp, zero = 0.0, remove.second = TRUE, memcmp = TRUE)

    # sort df
    uniqueLocations<-uniqueLocations[order(uniqueLocations$lon, decreasing=TRUE),]
    cat ("dataframe cleaned and converted\n")
  } else {stop(" you did not provide a data frame")}

  allP<-list()
  # fill it with starting points better
  for (i in seq(1,length(tmp$lon)) ){
    allP[[i]]<-   c(uniqueLocations$lon[i],uniqueLocations$lat[i])
  }
  ### end pointlist
  if (internalCost) {
    # getData using the getGeoData function
    # initialize the GRASS SAGA and extent settings
    envGIS<- initRGIS(root.dir=rootDir,working.dir=workingDir,fndem=getGeoData("SRTM",xtent=xtent))

    # import DEM to GRASS
    rgrass7::execGRASS('r.external',
                       flags=c('o',"overwrite","quiet"),
                       input=envGIS$fn,  #mosaicSRTM@file@name,
                       output='dem',
                       band=1
    )


    # The Topographic Convergence Index (TCI) provides an estimation
    # of rainwater runoff availability to plants based on specific catchment
    # area (A) and local slope (b) such that TCI = ln(A/tan b) (Beven & Kirkby, 1979)
    # this seems so be most suitable for the beetles
    ### terraflow provides all basic parameters of an hydrological coorrected DEM
    ### TODO accumulated flows (costs) vs. tci  up to now tci is used for walk and accu for drain
    cat("
       ########## starting r.terraflow   ###########

       By default the  Topographic Convergence Index (Beven & Kirkby, 1979) is used.
       ln[A/tan(slope)], A= upslope contributing area
       NOTE: be patient even if massive optimized it is TIME CONSUMING!\n")

    rgrass7::execGRASS('r.terraflow',
                       flags=c("overwrite"),
                       elevation="dem",
                       filled="filled",
                       direction="accudir",
                       swatershed="watershed",
                       accumulation="accu",
                       tci="tci",
                       memory=memSize,
                       stats="demstats.txt")
    # forks in cost types NOTE all will be accumulated in gcost
    if (costType == "tci"){
      cat('TCI is assumed to be costLayer')
      offset<- getMinMaxG('tci')[2]
      rgrass7::execGRASS('r.mapcalc',
                         flags=c("overwrite"),
                         expression=paste('"cost = (tci * -1) + ',offset,'"'))
    } else if (costType == "dem"){
      cat('dem.filled is assumed to be costLayer')
      offset<- getMinMaxG('filled')[2]
      rgrass7::execGRASS('r.mapcalc',
                         flags=c("overwrite"),
                         expression=paste('"cost = (filled * -1) + ',offset,'"'))
    }

  }
#   else if (!is.null(externalCostRaster))
#   { # setup GRASS etc with external data
#     envGIS<- initRGIS(root.dir=rootDir,working.dir=workingDir,fndem=externalCostRaster)
#   }


  # export
  if (dump){
    G2Tiff(runDir=envGIS$runDir,layer="dem")
    G2Tiff(runDir=envGIS$runDir,layer="cost")
    G2Tiff(runDir=envGIS$runDir,layer="accu")
    G2Tiff(runDir=envGIS$runDir,layer="filled")
    G2Tiff(runDir=envGIS$runDir,layer="accudir")
    G2Tiff(runDir=envGIS$runDir,layer="tci")
    ### create a shapefile with the point data and the enhancend extent of the
    #  used DEM raster (+zone) as dummy for grass get raster infos
    r <- raster::raster(envGIS$fn)
    # create spdf with all informations
    rawShp<-SpatialPointsDataFrame(data.frame(inputData$lon,inputData$lat),data.frame(inputData$code), proj4string = CRS("+init=epsg:4326"))
    # assign extent of raster to shp
    rawShp@bbox <-t(bbox(r))
    writeOGR(rawShp, envGIS$runDir, "pointList", driver="ESRI Shapefile", overwrite_layer=TRUE)



  }


  ##### Main loop
  # calulate for each point a accumulated cost raster
  # NOTE the used data is totalcost from above!!!


    #lst_all <- foreach(i = 1:ceiling(length(allP)*0.5)) %dopar% {
    #costDistTmp<-data.frame(num=rep(NA, 5), txt=rep("", 5), stringsAsFactors=FALSE)
    costDist<-list()
    for (i in seq(1,length(allP))){
      startP<-allP[i]
      allP<-allP[-i]
      if (!is.null(unlist(startP))){
        accuCalc(envGIS=envGIS,currentP=startP,memory=memSize,dump=dump,walk=walk)
        costDist[[i]]<-gcost(envGIS$runDir,startP,allP)
      }
    }


  ### todo
  # matrix of results
  # merge with ids
  # merge geometrical infos into shapes
  #dem<-mapview(raster(envGIS$fn))
  #dem + point<-mapview(pointList)


  cat('Thats it')
  mergedCostDist = Reduce(function(...) merge(..., all=T), costDist)
  return(mergedCostDist)
}


