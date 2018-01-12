if (!isGeneric('extractTrainData')) {
  setGeneric('extractTrainData', function(x, ...)
    standardGeneric('extractTrainData'))
}

#'@name extractTrainData
#'@title extracts training data from a raster stack
#'
#'@description
#' extracts training data from a raster stack
#'
#'@author Chris Reudenbach
#'
#'@param rasterStack  default is \code{NULL} rasterstack wcontaining all image data
#'@param trainPlots default is \code{NULL}  sp object providing training geometries
#'@param ids default is \code{c(1,2)} classification ids 
#'@param idLabel default is \code{c("yes","no")} names of ids
#'@param trainDataFn default is \code{filepath(temp(),"trainingDF.RData")} Name of the extracted training data file
#'

#'@return extractTrainData returns a dataframe with all training data
#'
#'@export extractTrainData
#'@examples
#'\dontrun{
#'
#' trainingDF <- extractTrainData(rasterStack  = trainStack,
#'                                training     = training,
#'                                ids=c(1,2),
#'                                idLabel= c("green","nogreen")) 
#'}
#'


extractTrainData<-function(rasterStack  = NULL,
                        trainPlots     = NULL,
                        ids=c(1,2),
                        idLabel= c("green","nogreen"),
                        trainDataFn=filepath(temp(),"trainingDF.RData")) {
  
  cat("\n::: extract trainPlots data...\n")
  trainingDF =  data.frame()
  #extract trainPlots Area pixel values
  for (j in 1:length(rasterStack)) {
    cat("\n::: extracting trainPlots data from image ",j," of ... ",length(rasterStack),"\n")
    categorymap<-rgeos::gUnionCascaded(trainPlots[[j]],id=trainPlots[[j]]@data$id)
    dataSet <- raster::extract(rasterStack[[j]], categorymap,df=TRUE)
    ## add filename as category
    dataSet$FN= substr(names(rasterStack[[j]][[1]]),1,nchar(names(rasterStack[[j]][[1]]))-2)
    #names(dataSet)<- names
    dataSet=dataSet[complete.cases(dataSet),]
    trainingDF<-rbind(trainingDF, dataSet)
  }
  
  ## reclassify data frame
  for (i in 1:length(ids)){
    trainingDF$ID[trainingDF$ID==i]<-label[i]
  }
  trainingDF$ID <- as.factor(trainingDF$ID)
  
  ## save dataframe
  save(trainingDF, file = trainDataFn)
  return(trainingDF)
}