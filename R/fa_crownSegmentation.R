
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
#' @param chm Canopy height model in \link[raster]{raster} format. Should be the same that was used to create
#' the input for \code{treePos}.
#'@param leafsize       integer. bin size of grey value sampling range from 1 to 256 
#'@param normalize      integer.  logical switch if data will be normalized (1) 
#'@param neighbour      integer.  von Neumanns' neighborhood (0) or Moore's (1) 
#'@param method         integer. growing algorithm for feature space and position (0) or feature space only (1)
#'@param thVarSpatial   numeric. spatial variance 
#'@param thVarFeature   numeric. spatial variance 
#'@param thSimilarity   mumeric. similarity threshold 
#'@param seed_params    vector. of characters corresponding with the used attributes. The altitude values from surface model \code{c("chm")} is mandantory. 
#'@param giLinks        list. of GI tools cli pathes  
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
  
  
  param_list <- paste0(path_run,seed_params,".sgrd;",collapse = "")
  
  # Start final segmentation algorithm as provided by SAGA's seeded Region Growing segmentation (imagery_segmentation 3)
  # TODO sensitivity analysis of the parameters
  ret <- system(paste0(sagaCmd, " imagery_segmentation 3 ",
                       " -SEEDS "    ,path_run,"treePos.sgrd",
                       " -FEATURES '", param_list,
                       "' -SEGMENTS ",path_run,"crowns.shp",
                       " -LEAFSIZE " ,leafsize,
                       " -NORMALIZE ",normalize,
                       " -NEIGHBOUR ",neighbour, 
                       " -METHOD "   ,method,
                       " -SIG_1 "    ,thVarFeature,
                       " -SIG_2 "    ,thVarSpatial,
                       " -THRESHOLD ",thSimilarity),
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
#' @param treePos \link[sp]{SpatialPointsDataFrame}. The point locations of treetops. The function will generally produce a
#' number of crown segments equal to the number of treetops.
#' @param chm Canopy height model in \link[raster]{raster} format. Should be the same that was used to create
#' the input for \code{treePos}.
#' @param minTreeAlt numeric. The minimum height value for a \code{CHM} pixel to be considered as part of a crown segment.
#' All \code{chm} pixels beneath this value will be masked out. Note that this value should be lower than the minimum
#' height of \code{treePos}.
#' @param format string. Format of the function's output. Can be set to either 'raster' or 'polygons'.
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
#' @param chm Canopy height model in \link[raster]{raster} or \link[raster]{SpatialGridDataFrame} file format. Should be the same that was used to create
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
  canopy[[1]]@proj4string <- chmR@crs
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
#' @param treePos numeric. \code{matrix} or \code{data.frame} with three columns (tree xy coordinates and height).
#' number of crown segments equal to the number of treetops.
#' @param chm Canopy height model in \link[raster]{raster} or \link[raster]{SpatialGridDataFrame} file format. Should be the same that was used to create
#' the input for \code{treePos}.
#' @param maxCrownArea numeric. A single value of the maximum individual tree crown radius expected. Default 10.0 m.
#' height of \code{treePos}.
#' @param EPSG The EPSG code of the reference system of the CHM raster image.
#' @param movingWin Size (in pixels) of the moving window to detect local maxima.
#' @param minTreeAlt Height threshold (m) below a pixel cannot be a local maximum. Local maxima values are used to define tree tops.
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

#'@param lasList filename (.las-file) or list of filenames (.txt-file)
#'@param cellcz integer. Cellsize for creating .dtm of las-point-cloud
#'@param perc integer. n-th percentile of canopy heigth for heightbreak of Tree Segmentation (see Details)
#'@param fowsz integer. Cellsize for focal window (see Details)
#'@param fowfi function. The function fowfi should take multiple numbers and return a single number.
#' For example mean, modal, min or max
#'@param proj character proj4 string
#'@param path character. Set working directory [Example: path = "D:/TreeSeg/"]
#'@param Fp  character. Directory for FUSION tools [Example: Fp = "D:/FUSION/"]
#'
#'@details The "Create Tree Segmentation" (chmSegmentationFU) function works with a watershed segmentation algorithm (Vicent & Soille 1991). Basis for the calculation of "basins" is a canopy height model (CHM) and not point data files. From the input (.las-files) a canopy surface model using LIDAR point cloud ist created by the CanopyModel application from FUSION in .dtm format. The tool uses the returns with the highest elevation to compute the surface model. The applied /ground switch produces a canopy height model (CHM) by subtracting the earth surface model from the return elevation. A focal window with different arguments is used to calculate focal ("moving window") values for the neighborhood of focal cells. The Tree Segmentation is created by the chmSegmentationFU aplication from FUSION which used the filtered canopy heigth model and the heightbreak to calculate basins. The tool produces a metrics file in .csv-format, a shapefile containing high points and basin metrics, also another shapefile containing basin (crown) outline and a .tif raster map including basins.\cr\cr
#'\itemize{
#'\item{
#'  lasList: lasList has to be a .las-file or wildcard of .txt-file including different .las-files
#'  }
#'\item{
#'  cellsz: Grid size of input raster (CanopyHeigthModel) in Tree Segmantation (default=1) !!BE AWARE!!. Main parameter for chmSegmentationFU, adapt to tree species (size, crown size, etc.) necessary.
#'  }
#'\item{
#'  perc: default=37 (95th percentile), 38 (99th percentile), 35 (80th percentile), for more information see FUSION GridMatrix \url{https://w3.ual.es/GruposInv/ProyectoCostas/FUSION_manual.pdf}
#'  }
#'\item{
#'  fowsz: Focal uses a matrix of weights for the neighborhood of the focal cells. Fowsz ist the matrix of moving window. If fowsz is 3 a 3x3 window is used (default=3). For more details see: focal \url{https://www.rdocumentation.org/packages/raster/versions/2.6-7/topics/focal}
#'  }
#'\item{
#'  fowfi: Focal can use different function options like mean, modal, min or max for the calculation (default="mean")
#'  }
#'\item{
#'  proj: Set projection with EPSG-Code (default="epsg:25832" --> ETRS89 / UTM zone 32N) for more information see: \url{http://spatialreference.org/}
#'  }
#'\item{
#'  path: Creates a /output and /results folder. In /output all .tif-results and in /results are the .shp and basinmap.tif are saved.
#'  }
#'\item{
#'  Fp: Directory for FUSION tools. FUSION tools can be downloaded here: \url{http://forsys.sefs.uw.edu/fusion/fusionlatest.html}
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
#'@examples{
#'  ## NOT RUN 
#'  ## i=list.files(path,pattern=".las$", full.names=FALSE)
#'  ## chmSegmentationFU(lasList=i,cellsz=c(1,3,5),perc=37,fowsz=3,fowfi="mean",proj="+init=epsg:25832",path,Fp)
#'}
#'@export
#'
chmSegmentationFU=function(lasDir =NULL,
                           cellsz = 0.5,
                           perc=37,
                           fowsz=3,
                           fowfi="mean",
                           proj="+init=epsg:25832",
                           path = getwd(),
                           Fp = "C:/FUSION/",
                           extent=NULL){
  
  output=""
  results="output\\"
  dir.create(file.path(path,output), showWarnings = FALSE)
  dir.create(file.path(dirname(path),results), showWarnings = FALSE)
  
  #### fusion settings
  #---> add the location for the Fusion binary folder
  if (Sys.info()["sysname"] == "Windows") {
    Fp<-Fp
  } else if (Sys.info()["sysname"] == "Linux") {
    # check if wine is installed
    if (substr(system2("wine",args = "--help",stderr = TRUE, stdout = TRUE,)[1],1 ,6 ) == "Usage:") {
      Fp <-"LC_CTYPE=de_DE.utf8 wine /home/creu/.wine/dosdevices/c:/FUSION/"
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
  for (i in lasList){
    system(paste0(Fp,"catalog.exe ",path,i," ",
                  i,".html"))
    dellist=append(dellist,paste0(i,".html"))
    
    # calculate extents etc...
    
    #--> Fusion catalog if not allready exists
    if (is.null(ext)){
      command<-Fp
      command<-paste0(command, "catalog.exe")
      command<-paste0(command," ", i )
      command<-paste0(command," ", i,".html"   )
      system(command)
      #--> extract extent info
      info <- read.csv(paste0(i,".csv"))
      #fix extent
      info2<-missingExtents(info)
      #TODO  fix error in las files if (as.numeric(info[[2]][3])) fixLas()
      #--> define extent for further calculation
      extent<-paste(as.numeric(info2$MinX),as.numeric(info2$MinY),as.numeric(info2$MaxX),as.numeric(info2$MaxY))
      extnumeric<-c(as.numeric(info2$MinX),as.numeric(info2$MinY),as.numeric(info2$MaxX),as.numeric(info2$MaxY))
    } else {
      extent<- paste(ext@xmin,ext@ymin,ext@xmax,ext@ymax)
    }
    
      system(paste0(paste0(Fp,"clipdata.exe", " /class:2 ",i," ",i,"_groundpts.las "), extent))
      dellist=append(dellist,paste0(i,"_groundpts.las "))
      i<- paste0(i,"_groundpts.las")
    for (k in fowsz){
      for (j in cellsz){
        system(paste0(Fp,"gridsurfacecreate.exe ", i,"_",j,"m_gridsurf.dtm "
                      ,j," M M 1 32 0 0 ",i))
        dellist=append(dellist,paste0(i,"_",j,"m_gridsurf.dtm "))
        system(paste0(Fp, "CanopyModel.exe ","/ground:",i,"_",j,"m_gridsurf.dtm ",
                      "/smooth:",k," ","/peaks ",
                      i,"_",j,"m_CHM.dtm ",j," M M 1 32 0 0 ",i))
        dellist=append(dellist,paste0(i,"_",j,"m_CHM.dtm "))
        system(paste0(Fp,"DTM2ASCII.exe ",i,"_",j,"m_CHM.dtm ",i,"_",
                      j,"m_CHM.asc"))
        dellist=append(dellist,paste0(i,"_",j,"m_CHM.asc"))
        tempo=raster::raster(paste0(i,"_",j, "m_CHM.asc"))
        raster::projection(tempo) <- sp::CRS("+init=epsg:25832")
        raster::writeRaster(tempo,paste0(i,"_",j,"m_CHM."),format="GTiff",overwrite=TRUE)
        system(paste0(Fp,"gridmetrics.exe ",i,"_",j,"m_gridsurf.dtm ",0," " ,
                      1," ",i,"_",j,"m_stattab.csv ",i))
        for (l in perc){
          system(paste0(Fp,"csv2grid.exe ",i,"_",j,
                        "m_stattab_all_returns_intensity_stats.csv ",l," ",i,"_",j,"m_",l,
                        "perc.asc"))
          dellist=append(dellist,paste0(i,"_",j,"m_",l,"perc.asc"))
          tempo=raster::raster(paste0(i,"_",j,"m_",l, "perc.asc"))
          raster::projection(tempo) <- sp::CRS(proj)
          tempo=raster::focal(tempo,w=matrix(1,k,k),fun=fowfi)
          raster::writeRaster(tempo,paste0(i,"_",j,"m_",l,"perc."),format="GTiff",overwrite=TRUE)
          system(paste0(Fp, "TreeSeg.exe ","/shape ",
                        i,"_",j,"m_CHM.dtm ",i,"_",j,"m_",l,"perc.tif ",
                        i,"_",j, "_treeseg_",l,"perc.csv "))
          dellist=append(dellist,paste0(i,"_",j,"_treeseg_",l,"perc", "_Basin_Map.asc"))
          dellist=append(dellist,paste0(i,"_",j,"_treeseg_",l,"perc", "_Max_Height_Map.asc"))
          tempo=raster::raster(paste0(i,"_",j,"_treeseg_",l,"perc", "_Basin_Map.asc"))
          raster::projection(tempo) <- sp::CRS(proj)
          raster::writeRaster(tempo,paste0(i,"_",j,"_",l,"_Basin_Map."),format="GTiff",overwrite=TRUE)
          tempo=raster::raster(paste0(i,"_",j,"_treeseg_",l,"perc", "_Max_Height_Map.asc"))
          raster::projection(tempo) <- sp::CRS(proj)
          raster::writeRaster(tempo,paste0(i,"_",j,"_",l,"_Max_Height_Map."),format="GTiff",overwrite=TRUE)
          dellist=append(dellist,paste0(i,"_",j,"_treeseg_",l,"perc","_Polygons.shp"))
          dellist=append(dellist,paste0(i,"_",j,"_treeseg_",l,"perc","_Polygons.dbf"))
          dellist=append(dellist,paste0(i,"_",j,"_treeseg_",l,"perc","_Polygons.shx"))
          shapes <- rgdal::readOGR(paste0(i,"_",j,"_treeseg_",l,"perc","_Polygons.shp"))
          raster::projection(shapes) <- sp::CRS(proj)
          rgdal::writeOGR(shapes,dsn=paste0(i,"_",j,"m_",l,"perc","_Polygons_class", ".shp"),
                   driver = "ESRI Shapefile",layer="treesize")
          dellist=append(dellist,paste0(i,"_",j,"_treeseg_",l,"perc","_HighPoints.shp"))
          dellist=append(dellist,paste0(i,"_",j,"_treeseg_",l,"perc","_HighPoints.dbf"))
          dellist=append(dellist,paste0(i,"_",j,"_treeseg_",l,"perc","_HighPoints.shx"))
          shapes <- rgdal::readOGR(paste0(i,"_",j,"_treeseg_",l,"perc","_HighPoints.shp"))
          raster::projection(shapes) <- sp::CRS(proj)
          rgdal::writeOGR(shapes,dsn=paste0(i,"_",j,"m_",l,"perc","_HighPoints_class", ".shp"),
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
  }
  