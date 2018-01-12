#' classify images using raster predict
#' 
#' @param imageFiles imagestacke for classification
#' @param model classification model
#' @param  in_prefix in frefix  string
#' @param out_prefix out prefix string
#' @param bandNames band names 
#' 
#' @export predictRGB 
#' 
# predictRGB(imageFiles=imagestack,
#             model = model_final,
#             in_prefix = "index_",
#             out_prefix = "classified_",
#             bandNames = c("R","G","B","A","VARI","NDTI","TGI","GLI","NGRDI","GLAI","Mean","Variance","Skewness","Kurtosis")) 

predictRGB <- function(imageFiles=NULL,
                       model = NULL,
                       in_prefix = "index_",
                       out_prefix = "classified_",
                       bandNames = c("R","G","B","A","VARI","NDTI","TGI","GLI","NGRDI","GLAI","Mean","Variance","Skewness","Kurtosis")) {
  
  cat("\n::: start prediction aka classifikation...\n")
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  foreach(i = 1:length(imageFiles)) %dopar% {
    #for (i in 1:length(imageFiles)) {
    library(raster)  
    library(randomForest)
    library(caret)
    fn<-basename(imageFiles[i])
    path_out<-substr(dirname(imageFiles[i]),1,nchar(dirname(imageFiles[i]))-3)
    fnOut <- paste0(path_out,"output/",out_prefix,fn)
    
    img2predict<-raster::stack(imageFiles[i])
    names(img2predict)<-bandNames
    predictImg<- raster::predict(img2predict,
                                 model,
                                 progress= "text")
    raster::writeRaster(predictImg, filename = fnOut, overwrite = TRUE)
  }
  stopCluster(cl)
}
