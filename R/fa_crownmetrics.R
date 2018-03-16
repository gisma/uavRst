#'calculate morphometric features of polygons.
#'@description calculate morphometric features of polygons. Calculate some crown related metrics, returns the metrics as a spatialpointdataframe/spatialpolygondataframe
#'@seealso  \href{https://CRAN.R-project.org/package=Momocs}{Momocs}
#' \href{https://www.researchgate.net/profile/Paul_Rosin/publication/228382248_Computing_global_shape_measures/links/0fcfd510802e598c31000000.pdf?origin=publication_detail}{Paul Rosin}
#'
#'@param crownarea sp*  spatialpolygon object
#'@param funNames character. names of morphometrics to be calculated 
#'available are ("length","elongation","eccentricityboundingbox","solidity","eccentricityeigen",
#'"calliper","rectangularity","circularityharalick","convexity")
#'
#'@examples
#'\dontrun{
#' # required packages
#' require(uavRst)
#' require(curl)
#' require(link2GI)
#' 
#' # project folder
#' projRootDir<-tempdir()
#' 
#' # create subfolders please mind that the pathes are exported as global variables
#' paths<-link2GI::initProj(projRootDir = projRootDir,
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")
#' # overide trailing backslash issue
#'  path_run<-ifelse(Sys.info()["sysname"]=="Windows", sub("/$", "",path_run),path_run)                        
#'  setwd(path_run)                         
#'                            
#' # get the rgb image, chm and training data 
#' url <- "https://github.com/gisma/gismaData/raw/master/uavRst/data/tutorial_data.zip"
#' res <- curl::curl_download(url, paste0(path_run,"tutorial_data.zip"))
#' unzip(zipfile = res, exdir = path_run)
#' 
#' # create the links to the GI software
#' giLinks<-uavRst::get_gi()
#' 
#' polymetric <- poly_metrics(spdf = paste0(path_run,"rgb_3-3_train2.shp"))
#'}
#'@export


poly_metrics<- function(crownarea,
                        funNames = c("length","elongation","eccentricityboundingbox","solidity","eccentricityeigen","calliper","rectangularity","circularityharalick","convexity")){
  cat("calculate crown-metrics for ",nrow(crownarea)," polygons... \n")
  polys <- crownarea@polygons

  for(subfun in funNames) {
    cat("calculate ",subfun,"\n")
    crownarea@data$subfun <- unlist(lapply(seq(1:length(polys)),function(i){
      f <- eval(parse(text=paste("Momocs::coo_",subfun,sep = "")))(as.matrix(polys[[i]]@Polygons[[1]]@coords))
      assign("comp", f)
      return(unlist(comp))
    }))
    colnames(crownarea@data)[ncol(crownarea@data)]<-subfun
  }
  crownarea@proj4string <- sp::CRS("+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
  crownarea[is.na(crownarea$chmQ10)]<- 0
  crownarea@data$Area <- rgeos::gArea(crownarea,byid = TRUE)
  return(crownarea)
}
