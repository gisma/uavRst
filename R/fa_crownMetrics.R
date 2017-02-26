
# gis function
#  calculate some crown related metrics https://cran.r-project.org/web/packages/Momocs/Momocs.pdf
# https://www.researchgate.net/profile/Paul_Rosin/publication/228382248_Computing_global_shape_measures/links/0fcfd510802e598c31000000.pdf?origin=publication_detail
# return the metrics as a spatialpointdataframe/spatialpolygondataframe
# see also: https://github.com/logmoc/msc-phygeo-class-of-2016-creuden

if (!isGeneric('fa_caMetrics')) {
  setGeneric('fa_caMetrics', function(x, ...)
    standardGeneric('fa_caMetrics'))
}

#'@name fa_caMetrics
#'@title calculate morphometric features of polygons
#'
#'@export fa_caMetrics
fa_caMetrics<- function(crownarea, funNames = c("area","length","elongation","eccentricityboundingbox","solidity","eccentricityeigen","calliper","rectangularity","circularityharalick","convexity")){
  cat("calculate metrics for ",nrow(crownarea)," polygons... \n")
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
  return(crownarea)
}