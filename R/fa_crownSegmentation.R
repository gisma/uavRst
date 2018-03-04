
#'@title seeded region growing tree crown segmentation based on 'SAGA GIS'
#'
#'@description
#' Tree segmentation based on a CHM, basically returns a  vector data sets with the tree crown geometries and a bunch of corresponding indices. After the segementation itself the results are hole filled and optionally filtered by a majority filter in the 3*3 surrounding.
#'
#'@author Chris Reudenbach
#'
#'@param treePos  spatial raster object
#'@param minTreeAlt  numeric. The minimum height value for a \code{chm} pixel to be considered as part of a crown segment.
#' All \code{chm} pixels beneath this value will be masked out. Note that this value should be lower than the minimum
#' height of \code{treePos}.
#'@param minTreeAltParam default is "chmQ20"
#'@param chm Canopy height model in \code{raster} format. Should be the same that was used to create
#' the input for \code{treePos}. 
#'@param leafsize       integer. bin size of grey value sampling range from 1 to 256 see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param normalize      integer. logical switch if data will be normalized (1) see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param neighbour      integer. von Neumanns' neighborhood (0) or Moore's (1) see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param method         integer. growing algorithm for feature space and position (0) or feature space only (1), see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param thVarSpatial   numeric. Variance in Feature Space  see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param thVarFeature   numeric. Variance in Position Space  see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param thSimilarity   mumeric. Similarity Threshold see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param seed_params    character. a list of raster data that is used for the segmentation. The canopy height model \code{c("chm")} is mandantory. see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/imagery_segmentation_3.html}{SAGA GIS Help}
#'@param giLinks        list. of GI tools cli pathes  
#'@param majority_radius numeric. kernel size for the majority filter out spurious pixel
#'@export 
#'@examples
#'\dontrun{
#' # crown segmentation based on a CHM
#'  chmSegmentationFT(x = rasterobj,  "nameofSAGAFile")
#'}
#'
#'

chmSegmentation <- function(treePos = NULL,
                            chm = NULL,
                            minTreeAlt         =2,
                            minTreeAltParam = "chmQ20",
                            leafsize       = 256,
                            normalize      = 0,
                            neighbour      = 1,
                            method         = 0,
                            thVarFeature   = 1.,
                            thVarSpatial   = 1.,
                            thSimilarity   = 0.002,
                            seed_params    = c("chm"),
                            majority_radius    = 3.000,
                            giLinks = NULL) {
  proj<- raster::crs(treePos)
  if (class(treePos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    raster::writeRaster(treePos,file.path(path_run,"treePos.sdat"),overwrite = TRUE,NAflag = 0)
  }
  if (class(chm) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    chm[chm<minTreeAlt] = -1
    raster::writeRaster(chm,file.path(path_run,"chm.sdat"),overwrite = TRUE,NAflag = 0)
  }
  
  
  cat("::: run main segmentation...\n")
  # create correct param list s
  #seed_params<-c("HI","GLI")
  if (is.null(giLinks)){
    giLinks <- linkBuilder()
  }
  
  gdal <- giLinks$gdal
  saga <- giLinks$saga
  sagaCmd<-saga$sagaCmd
  RSAGA::rsaga.env(path =saga$sagaPath,modules = saga$sagaModPath)
  
  param_list <- paste0(path_run,seed_params,".sgrd;",collapse = "")
  
  # Start final segmentation algorithm as provided by SAGA's seeded Region Growing segmentation (imagery_segmentation 3)
  # TODO sensitivity analysis of the parameters
  
  
  RSAGA::rsaga.geoprocessor(lib = "imagery_segmentation", module = 3,
                            param = list(SEEDS = paste(path_run,"treePos.sgrd", sep = ""),
                                         FEATURES = param_list,
                                         SEGMENTS = paste(path_run,"crowns.shp", sep = ""),
                                         LEAFSIZE = leafsize,
                                         NORMALIZE = normalize,
                                         NEIGHBOUR = neighbour, 
                                         METHOD   =  method,
                                         SIG_1    =  thVarFeature,
                                         SIG_2    =  thVarSpatial,
                                         THRESHOLD = thSimilarity),
                            intern = TRUE)
  
  # fill holes inside the crowns (simple approach)
  # TODO better segmentation
  if (majority_radius > 0){
    outname<- "sieve_pre_tree_crowns.sdat"
    ret <- system(paste0("gdal_sieve.py -8 ",
                         path_run,"crowns.sdat ",
                         path_run,outname,
                         " -of SAGA"),
                  intern = TRUE)
    # apply majority filter for smoothing the extremly irregular crown boundaries 
    ret <- system(paste0(sagaCmd, " grid_filter 6 ",
                         " -INPUT "   ,path_run,"sieve_pre_tree_crowns.sgrd",
                         " -RESULT "  ,path_run,"crowns.sgrd",
                         " -MODE 0",
                         " -RADIUS "  ,majority_radius,
                         " -THRESHOLD 0.0 "),
                  intern = TRUE)
  }
  
  
  # convert filtered crown clumps to shape format 
  ret <- system(paste0(sagaCmd, " shapes_grid 6 ",
                       " -GRID "     ,path_run,"crowns.sgrd",
                       " -POLYGONS " ,path_run,"crowns.shp",
                       " -CLASS_ALL 1" ,
                       " -CLASS_ID 1.0",
                       " -SPLIT 1"),
                intern = TRUE)
  
  
  crowns <- rgdal::readOGR(path_run,"crowns", verbose = FALSE)
  #crowns<-tree_crowns[tree_crowns$VALUE > 0,]
  sp::proj4string(crowns)<-proj
  
  # extract chm stats by potential crown segments
  statRawCrowns <- uavRst::xpolystat(c("chm"),
                                     spdf = crowns)
  
  rgdal::writeOGR(obj = statRawCrowns,
                  dsn = path_run,
                  layer = "crowns",
                  driver= "ESRI Shapefile",
                  overwrite=TRUE)
  # simple filtering of crownareas based on tree height min max area and artifacts at the analysis/image borderline
  tree_crowns <- uavRst::fa_basicTreeCrownFilter(crownFn = paste0(path_run,"statRawCrowns.shp"),
                                                 minTreeAlt = minTreeAlt,
                                                 minCrownArea = 0,
                                                 maxCrownArea = 250,
                                                 minTreeAltParam = "chmQ20" )[[2]]
  
  options(warn=0)
  cat("segmentation finsihed...\n")
  return(tree_crowns)
} 



#' fast and straightforward watershed segmentation based on 'ForestTools'
#' @description  'ForestTools' segmentation of individual tree crowns based on a canopy height model and initial seeding points (trees). Very fast algorithm based on the imagr watershed algorithm.
#' Andrew Plowright: R package \href{https://CRAN.R-project.org/package=ForestTools}{'ForestTools'}
#' @param treePos \code{SpatialPointsDataFrame}. The point locations of treetops. The function will generally produce a
#' number of crown segments equal to the number of treetops.
#' @param chm Canopy height model in \code{raster} format. Should be the same that was used to create
#' the input for \code{treePos}.
#' @param minTreeAlt numeric. The minimum height value for a \code{CHM} pixel to be considered as part of a crown segment.
#' All \code{chm} pixels beneath this value will be masked out. Note that this value should be lower than the minimum
#' height of \code{treePos}.
#' @param format string. Format of the function's output. Can be set to either 'raster' or 'polygons'.
#' @param verbose quiet (1)
#' 
#' @import ForestTools
#' 
#' @export 
#' @examples 
#' \dontrun{
#'  crownsFT <- chmSegmentationFT(chm = kootenayCHM,
#'                                  treePos = tpos 
#'                                 format = "polygons", 
#'                                 minTreeAlt = 1.5, 
#'                                 verbose = FALSE)
#'                                 
#' }


chmSegmentationFT <- function(treePos = NULL, 
                              chm = NULL,
                              minTreeAlt = 2,
                              format = "polygons",
                              verbose = FALSE) {
  
  if (class(treePos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
    treePos <- raster::rasterToPoints(treePos,spatial = TRUE)
  } else {
    r<-raster::raster(treePos)
    treePos <- raster::rasterToPoints(treePos,spatial = TRUE)
  }
  
  # Crown segmentation
  crownsFT <- ForestTools::SegmentCrowns(treetops = treePos, 
                                         CHM = chm, 
                                         format = format, 
                                         minHeight = minTreeAlt, 
                                         verbose = verbose)
  
  # Writing Shapefile
  rgdal::writeOGR(obj = crownsFT,
                  dsn = paste0(path_output, "crowns_FT"),
                  layer = "crowns_FT",
                  driver= "ESRI Shapefile",
                  overwrite=TRUE)
  
  return(crownsFT)
}

#' watershed segmentation based on 'rLiDAR'
#' @description  'rLiDAR' segmentation of individual tree crowns based on a canopy height model and initial seeding points (trees). Generic segmentation algorithm
#' Carlos A. Silva et all.: R package \href{https://CRAN.R-project.org/package=rLiDAR}{rLiDAR}\cr
#' 
#' @param treePos numeric. \code{matrix} or \code{data.frame} with three columns (tree xy coordinates and height).
#' number of crown segments equal to the number of treetops.
#' @param chm Canopy height model in \code{raster} or \code{SpatialGridDataFrame} file format. Should be the same that was used to create
#' the input for \code{treePos}.
#' @param maxCrownArea numeric. A single value of the maximum individual tree crown radius expected. Default 10.0 m.
#' height of \code{treePos}.
#' @param exclusion numeric. A single value from 0 to 1 that represents the percent of pixel exclusion.
#' @import rLiDAR
#' @export 
#' @examples 
#' \dontrun{
#'  crownsRL <- chmSegmentationRL(chm, treePos, maxCrownArea, exclusion)
#' }


chmSegmentationRL <- function(treePos = NULL, 
                              chm = NULL,
                              maxCrownArea = 150,
                              exclusion = 0.2) {
  
  # if (class(treePos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
  #   treePos <- raster::rasterToPoints(treePos,spatial = TRUE)
  # } else {
  #   r<-raster::raster(treePos)
  #   treePos <- raster::rasterToPoints(treePos,spatial = TRUE)
  # }
  
  maxcrown <- sqrt(maxCrownArea/ pi)
  # Crown segmentation
  
  xyz <- as.data.frame(raster::rasterToPoints(treePos))
  names(xyz)<- c("x","y","height")
  canopy<-rLiDAR::ForestCAS(chm = chm, 
                            loc = xyz, 
                            maxcrown = maxcrown, 
                            exclusion =exclusion)
  canopy[[1]]@proj4string <- chm@crs
  # Writing Shapefile
  rgdal::writeOGR(obj = canopy[[1]],
                  dsn = paste0(path_output, "crowns_LR"),
                  layer = "crowns_LR",
                  driver= "ESRI Shapefile",
                  overwrite=TRUE)
  
  return(canopy[[1]])
}


#' decision tree method to grow individual tree crowns based on 'itcSegment'
#' @description Segmentation of individual tree crowns as polygons based on a LiDAR derived canopy height model. 
#' Michele Dalponte: R package \href{https://CRAN.R-project.org/package=itcSegment}{itcSegment}.
#'  M. Dalponte, F. Reyes, K. Kandare, and D. Gianelle, 
#'  "Delineation of Individual Tree Crowns from ALS and Hyperspectral data: a comparison among four methods," 
#'  European Journal of Remote Sensing, Vol. 48, pp. 365-382, 2015.
#' 
#' @examples 
#' \dontrun{
#'  tree_crowns_shp <- tree_crown_segmentation(chm = NULL, 
#'                        rs_code = "25832",
#'                        movWindow = 5,
#'                        th_local_max = 5,
#'                        max_crown_diam = 20)
#'                        }


#' @param chm Canopy height model in \code{raster} or \code{SpatialGridDataFrame} file format. Should be the same that was used to create
#' the input for \code{treePos}.
#' @param maxCrownArea numeric. A single value of the maximum individual tree crown radius expected. Default 10.0 m.
#' height of \code{treePos}.
#' @param EPSG_code The EPSG code of the reference system of the CHM raster image.
#' @param movingWin Size (in pixels) of the moving window to detect local maxima.
#' @param minTreeAlt Height threshold (m) below a pixel cannot be a local maximum. Local maxima values are used to define tree tops.
#' @param TRESHSeed seeding threshold
#' @param TRESHCrown crowns threshold
#' @import itcSegment
#' @export chmSegmentationITC
#' @examples 
#' \dontrun{
#'  crownsITC <- chmSegmentationITC(chm, treePos, maxCrownArea, exclusion)
#' }


chmSegmentationITC <- function(chm =NULL,
                               EPSG_code =3064,
                               movingWin = 7,
                               TRESHSeed = 0.45,
                               TRESHCrown = 0.55,
                               minTreeAlt = 2,
                               maxCrownArea = 100) {
  
  # if (class(treePos) %in% c("RasterLayer", "RasterStack", "RasterBrick")) {
  #   chm <- raster::raster(chm)
  # } 
  
  maxcrown <- sqrt(maxCrownArea/ pi)*2
  
  crown_polygon <- itcSegment::itcIMG(imagery = chm,
                                      epsg = EPSG_code,
                                      TRESHSeed =  0.45,
                                      TRESHCrown = 0.55,
                                      searchWinSize = movingWin,
                                      th = minTreeAlt,
                                      DIST = maxcrown,
                                      ischm = TRUE)
  
  rgdal::writeOGR(crown_polygon,
                  dsn = paste0(path_output, "crowns_itc", "localMax", minTreeAlt, "_crownDiam", maxCrownArea),
                  layer = "result",
                  driver= "ESRI Shapefile",
                  overwrite=TRUE)
  
  
  
  return(crown_polygon)
}





#'@name chmSegmentationFU
#'@title Create Tree Segmentation based on wrapping 'Fusion'
#'@description Method to create a Tree Segmentation using different FUSION tools. 
#'  The Tree Segmentation can be created from LAS-files and creates raster and shapefile output.

#'@param lasDir filename (.las-file) or list of filenames (.txt-file)
#'@param grid_size integer. Cellsize for creating .dtm of las-point-cloud
#'@param fusionPercentile integer. n-th percentile of canopy heigth for heightbreak of Tree Segmentation (see Details)
#'@param movingWin integer. Cellsize for focal window (see Details)
#'@param focalStatFun function. The function focalStatFun should take multiple numbers and return a single number.
#' For example mean, modal, min or max
#'@param proj4 character proj4 string
#'@param path character. Set working directory [Example: path = "D:/TreeSeg/"]
#'@param fusionCmd  character. Directory for FUSION tools [Example: fusionCmd = "D:/FUSION/"]
#'@param extent cutting extent

#'
#'@details The "Create Tree Segmentation" (chmSegmentationFU) function works with a watershed segmentation algorithm (Vicent & Soille 1991). Basis for the calculation of "basins" is a canopy height model (CHM) and not point data files. From the input (.las-files) a canopy surface model using LIDAR point cloud ist created by the CanopyModel application from FUSION in .dtm format. The tool uses the returns with the highest elevation to compute the surface model. The applied /ground switch produces a canopy height model (CHM) by subtracting the earth surface model from the return elevation. A focal window with different arguments is used to calculate focal ("moving window") values for the neighborhood of focal cells. The Tree Segmentation is created by the chmSegmentationFU aplication from FUSION which used the filtered canopy heigth model and the heightbreak to calculate basins. The tool produces a metrics file in .csv-format, a shapefile containing high points and basin metrics, also another shapefile containing basin (crown) outline and a .tif raster map including basins.\cr\cr
#'\itemize{
#'\item{
#'  lasList: lasList has to be a .las-file or wildcard of .txt-file including different .las-files
#'  }
#'\item{
#'  grid_size: Grid size of input raster (CanopyHeigthModel) in Tree Segmantation (default=1) !!BE AWARE!!. Main parameter for chmSegmentationFU, adapt to tree species (size, crown size, etc.) necessary.
#'  }
#'\item{
#'  fusionPercentile: default=37 (95th percentile), 38 (99th percentile), 35 (80th percentile), for more information see FUSION GridMatrix \url{https://w3.ual.es/GruposInv/ProyectoCostas/FUSION_manual.pdf}
#'  }
#'\item{
#'  movingWin: Focal uses a matrix of weights for the neighborhood of the focal cells. movingWin ist the matrix of moving window. If movingWin is 3 a 3x3 window is used (default=3). For more details see: focal \url{https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/focal}
#'  }
#'\item{
#'  focalStatFun: Focal can use different function options like mean, modal, min or max for the calculation (default="mean")
#'  }
#'\item{
#'  proj4: Set projection with EPSG-Code (default="epsg:25832" --> ETRS89 / UTM zone 32N) for more information see: \url{http://spatialreference.org/}
#'  }
#'\item{
#'  path: Creates a /output and /results folder. In /output all .tif-results and in /results are the .shp and basinmap.tif are saved.
#'  }
#'\item{
#'  fusionCmd: Directory for FUSION tools. FUSION tools can be downloaded here: \url{http://forsys.sefs.uw.edu/fusion/fusionlatest.html}
#'  }
#'}
#'@author Santowski, A., Schupp, A. & C. Weber (2018), adapted to Linux and some other package issues Chris Reudenbach
#'
#'@references{
#'  McGaughey (2006): FUSION/LDV: Software for LIDAR Data Analysis and Visualization.
#'  
#'  Vincent & Soille (1991): Watersheds in digital space: An efficient algorithm based on immersion simulations. IEEE Transactions on Pattern Analysis and Machine Intelligence, 13, 583-598. 
#'}
#'@import sp
#'@import raster
#'@import rgdal
#'@seealso{
#'  focal \url{https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/focal} 
#'  
#'  FUSION \url{https://w3.ual.es/GruposInv/ProyectoCostas/FUSION_manual.pdf}
#'  
#'  EPSG \url{http://spatialreference.org/}
#'}
#'@examples
#' \dontrun{
#'  ## NOT RUN 
#'  ## i=list.files(path,pattern=".las$", full.names=FALSE)
#'  ## chmSegmentationFU(lasList=i,
#'                       grid_size=c(1,3,5),
#'                       fusionPercentile=37,
#'                       movingWin=3,
#'                       focalStatFun="mean",
#'                       proj4="+init=epsg:25832",
#'                       path,
#'                       fusionCmd)
#'}
#'@export
#'
chmSegmentationFU <- function(lasDir =NULL,
                              grid_size = 0.5,
                              fusionPercentile=37,
                              movingWin=3,
                              focalStatFun="mean",
                              proj4="+init=epsg:25832",
                              path = getwd(),
                              fusionCmd = "C:/FUSION/",
                              extent=NULL){
  
  output=""
  results="output\\"
  dir.create(file.path(path,output), showWarnings = FALSE)
  dir.create(file.path(dirname(path),results), showWarnings = FALSE)
  
  #### fusion settings
  #---> add the location for the Fusion binary folder
  if (Sys.info()["sysname"] == "Windows") {
    fusionCmd<-fusionCmd
  } else if (Sys.info()["sysname"] == "Linux") {
    # check if wine is installed
    if (substr(system2("wine",args = "--help",stderr = TRUE, stdout = TRUE,)[1],1 ,6 ) == "Usage:") {
      fusionCmd <-"LC_CTYPE=de_DE.utf8 wine ~/.wine/dosdevices/c:/FUSION/"
      path<- gsub(path,pattern = "/",replacement = "\\\\")
      path<-paste0(path,"\\")
    }
  }
  
  # check las / laz files laz will be preferred
  lasFileNames <- list.files(pattern = "[.]las$", path = lasDir, full.names = TRUE)
  lazFileNames <- list.files(pattern = "[.]laz$", path = lasDir, full.names = TRUE)
  if (length(lazFileNames) > 0 ) {
    extFN <- substr(extension(basename(lazFileNames[1])),2,4)
    noF <- length(lazFileNames)
    file.copy(paste0(lazFileNames),getwd())
    lasList <- basename(lazFileNames)
  }
  else if (length(lasFileNames) > 0) {
    extFN <- substr(extension(basename(lasFileNames[1])),2,4)
    noF <- length(lasFileNames)
    file.copy(paste0(lasFileNames),getwd())
    lasList <- basename(lasFileNames)
  }
  else stop("no valid las or laz files found...\n")
  
  dellist=list()
  for (lasFile in lasList){
    system(paste0(fusionCmd,"catalog.exe ",path,lasFile," ",
                  lasFile,".html"))
    dellist=append(dellist,paste0(lasFile,".html"))
    
    # calculate extents etc...
    
    #--> Fusion catalog if not allready exists
    if (is.null(extent)){
      command<-fusionCmd
      command<-paste0(command, "catalog.exe")
      command<-paste0(command," ", lasFile )
      command<-paste0(command," ", lasFile,".html"   )
      system(command)
      #--> extract extent info
      info <- utils::read.csv(paste0(lasFile,".csv"))
      #fix extent
      info2<-missingExtents(info)
      #TODO  fix error in las files if (as.numeric(info[[2]][3])) fixLas()
      #--> define extent for further calculation
      extent<-paste(as.numeric(info2$MinX),as.numeric(info2$MinY),as.numeric(info2$MaxX),as.numeric(info2$MaxY))
      extnumeric<-c(as.numeric(info2$MinX),as.numeric(info2$MinY),as.numeric(info2$MaxX),as.numeric(info2$MaxY))
    } else {
      extent<- paste(extent@xmin,extent@ymin,extent@xmax,extent@ymax)
    }
    
    system(paste0(paste0(fusionCmd,"clipdata.exe", " /class:2 ",lasFile," ",lasFile,"_groundpts.las "), extent))
    dellist=append(dellist,paste0(lasFile,"_groundpts.las "))
    lasFile<- paste0(lasFile,"_groundpts.las")
    for (curWin in movingWin){
      for (actual_grid_size in grid_size){
        system(paste0(fusionCmd,"gridsurfacecreate.exe ", lasFile,"_",actual_grid_size,"m_gridsurf.dtm "
                      ,actual_grid_size," M M 1 32 0 0 ",lasFile))
        dellist=append(dellist,paste0(lasFile,"_",actual_grid_size,"m_gridsurf.dtm "))
        system(paste0(fusionCmd, "CanopyModel.exe ","/ground:",lasFile,"_",actual_grid_size,"m_gridsurf.dtm ",
                      "/smooth:",curWin," ","/peaks ",
                      lasFile,"_",actual_grid_size,"m_CHM.dtm ",actual_grid_size," M M 1 32 0 0 ",lasFile))
        dellist=append(dellist,paste0(lasFile,"_",actual_grid_size,"m_CHM.dtm "))
        system(paste0(fusionCmd,"DTM2ASCII.exe ",lasFile,"_",actual_grid_size,"m_CHM.dtm ",lasFile,"_",
                      actual_grid_size,"m_CHM.asc"))
        dellist=append(dellist,paste0(lasFile,"_",actual_grid_size,"m_CHM.asc"))
        tempo=raster::raster(paste0(lasFile,"_",actual_grid_size, "m_CHM.asc"))
        raster::projection(tempo) <- sp::CRS("+init=epsg:25832")
        raster::writeRaster(tempo,paste0(lasFile,"_",actual_grid_size,"m_CHM."),format="GTiff",overwrite=TRUE)
        system(paste0(fusionCmd,"gridmetrics.exe ",lasFile,"_",actual_grid_size,"m_gridsurf.dtm ",0," " ,
                      1," ",lasFile,"_",actual_grid_size,"m_stattab.csv ",lasFile))
        for (l in fusionPercentile){
          system(paste0(fusionCmd,"csv2grid.exe ",lasFile,"_",actual_grid_size,
                        "m_stattab_all_returns_intensity_stats.csv ",l," ",lasFile,"_",actual_grid_size,"m_",l,
                        "fusionPercentile.asc"))
          dellist=append(dellist,paste0(lasFile,"_",actual_grid_size,"m_",l,"fusionPercentile.asc"))
          tempo=raster::raster(paste0(lasFile,"_",actual_grid_size,"m_",l, "fusionPercentile.asc"))
          raster::projection(tempo) <- sp::CRS(proj4)
          tempo=raster::focal(tempo,w=matrix(1,curWin,curWin),fun=focalStatFun)
          raster::writeRaster(tempo,paste0(lasFile,"_",actual_grid_size,"m_",l,"fusionPercentile."),format="GTiff",overwrite=TRUE)
          system(paste0(fusionCmd, "TreeSeg.exe ","/shape ",
                        lasFile,"_",actual_grid_size,"m_CHM.dtm ",lasFile,"_",actual_grid_size,"m_",l,"fusionPercentile.tif ",
                        lasFile,"_",actual_grid_size, "_treeseg_",l,"fusionPercentile.csv "))
          dellist=append(dellist,paste0(lasFile,"_",actual_grid_size,"_treeseg_",l,"fusionPercentile", "_Basin_Map.asc"))
          dellist=append(dellist,paste0(lasFile,"_",actual_grid_size,"_treeseg_",l,"fusionPercentile", "_Max_Height_Map.asc"))
          tempo_basin=raster::raster(paste0(lasFile,"_",actual_grid_size,"_treeseg_",l,"fusionPercentile", "_Basin_Map.asc"))
          raster::projection(tempo_basin) <- sp::CRS(proj4)
          raster::writeRaster(tempo_basin,paste0(lasFile,"_",actual_grid_size,"_",l,"_Basin_Map."),format="GTiff",overwrite=TRUE)
          tempo=raster::raster(paste0(lasFile,"_",actual_grid_size,"_treeseg_",l,"fusionPercentile", "_Max_Height_Map.asc"))
          raster::projection(tempo) <- sp::CRS(proj4)
          raster::writeRaster(tempo,paste0(lasFile,"_",actual_grid_size,"_",l,"_Max_Height_Map."),format="GTiff",overwrite=TRUE)
          dellist=append(dellist,paste0(lasFile,"_",actual_grid_size,"_treeseg_",l,"fusionPercentile","_Polygons.shp"))
          dellist=append(dellist,paste0(lasFile,"_",actual_grid_size,"_treeseg_",l,"fusionPercentile","_Polygons.dbf"))
          dellist=append(dellist,paste0(lasFile,"_",actual_grid_size,"_treeseg_",l,"fusionPercentile","_Polygons.shx"))
          shapes_basin <- rgdal::readOGR(paste0(lasFile,"_",actual_grid_size,"_treeseg_",l,"fusionPercentile","_Polygons.shp"))
          raster::projection(shapes_basin) <- sp::CRS(proj4)
          rgdal::writeOGR(shapes_basin,dsn=paste0(lasFile,"_",actual_grid_size,"m_",l,"fusionPercentile","_Polygons_class", ".shp"),
                          driver = "ESRI Shapefile",layer="treesize")
          dellist=append(dellist,paste0(lasFile,"_",actual_grid_size,"_treeseg_",l,"fusionPercentile","_HighPoints.shp"))
          dellist=append(dellist,paste0(lasFile,"_",actual_grid_size,"_treeseg_",l,"fusionPercentile","_HighPoints.dbf"))
          dellist=append(dellist,paste0(lasFile,"_",actual_grid_size,"_treeseg_",l,"fusionPercentile","_HighPoints.shx"))
          shapes_hpoints <- rgdal::readOGR(paste0(lasFile,"_",actual_grid_size,"_treeseg_",l,"fusionPercentile","_HighPoints.shp"))
          raster::projection(shapes_hpoints) <- sp::CRS(proj4)
          rgdal::writeOGR(shapes_hpoints,dsn=paste0(lasFile,"_",actual_grid_size,"m_",l,"fusionPercentile","_HighPoints_class", ".shp"),
                          driver = "ESRI Shapefile",layer="treepoint")
        }
      }
    }
  }
  
  randa=list.files(".",pattern=".txt$", full.names=T)
  dellist=append(dellist,randa)
  randb=list.files(".",pattern=".csv$", full.names=T)
  dellist=append(dellist,randb)
  for (i in dellist){
    file.remove(i)
  }
  dellist=list()
  return(list(shapes_basin,shapes_hpoints,tempo_basin))
}
