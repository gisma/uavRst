# extract pixel values according to an overlay and response name ---
extractTrainPixelValues <- function(imgStack=NULL,trainData=NULL,responseCol=NULL){
  #extract training Area pixel values
  dfTpv = data.frame(matrix(vector(), nrow = 0, ncol = length(names(imgStack)) + 1))
  for (i in 1:length(unique(trainData[[responseCol]]))){
    category <- unique(trainData[[responseCol]])[i]
    message("\n extracting cat: ",levels(category)[i]," no: ",i," of: ",length(unique(trainData[[responseCol]])))
    categorymap <- trainData[trainData[[responseCol]] == category,]
    dataSet <- raster::extract(imgStack, categorymap)
    dataSet <- lapply(dataSet, function(x){cbind(x, class = as.numeric(rep(category, nrow(x))))})
    df <- do.call("rbind", dataSet)
    dfTpv <- rbind(dfTpv, df)
  }
  names(dfTpv)<-gsub(names(dfTpv),pattern = "\\.",replacement = "_")
  return(dfTpv)
}