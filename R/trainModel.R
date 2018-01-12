#' trains model according to trainingdata and image stacks
#' 
#' @param imageFiles imagestacke for classification
#' @param model classification model
#' @param  in_prefix in frefix  string
#' @param out_prefix out prefix string
#' @param bandNames band names 
#' trainingDF =NULL,
#' @param predictors   default is \code{c("R","G","B")}
#' @param response     default is \code{"ID"}
#' @param spaceVar     default is \code{"FN"}
#' @param names        default is \code{c("ID","R","G","B","A","FN")}
#' @param noLoc        default is \code{3}
#' @param cl_method    default is \code{"rf"}
#' @param metric_ffs   default is \code{"kappa"}
#' @param metric_caret default is \code{ "ROC"}
#' @param pVal         default is \code{ 0.5}
#' @param prefin       default is \code{"final_"}
#' @param preffs       default is \code{"ffs_"}
#' @param modelSaveName default is \code{"model.RData" }
#' 
#' @export trainModel
#' @examples  
#' result<-  trainModel(trainingDF =trainingDF,
#'                      predictors   = c("R","G","B","VARI","NDTI","TGI","GLI","NGRDI","GLAI","Mean","Variance","Skewness","Kurtosis"),
#'                      response     = "ID",
#'                      spaceVar     = "FN",
#'                      names = c("ID","R","G","B","A","VARI","NDTI","TGI","GLI","NGRDI","GLAI","Mean","Variance","Skewness","Kurtosis","FN"),
#'                      noLoc        = length(imageTrainFiles),
#'                      cl_method    = "rf",
#'                      metric_ffs   = "kappa",
#'                      metric_caret = "ROC",
#'                      pVal         = 0.5) 

trainModel<-function(   trainingDF =NULL,
                        predictors   = c("R","G","B"),
                        response     = "ID",
                        spaceVar     = "FN",
                        names        = c("ID","R","G","B","A","FN"),
                        noLoc        = 3,
                        cl_method    = "rf",
                        metric_ffs   = "kappa",
                        metric_caret = "ROC",
                        pVal         = 0.5,
                        prefin       ="final_",
                        preffs       ="ffs_",
                        modelSaveName="model.RData" ) {
  
  # create subset according to pval
  trainIndex<-caret::createDataPartition(trainingDF$ID, p = pVal,list=FALSE)
  data_train <- trainingDF[ trainIndex,]
  data_test <- trainingDF[-trainIndex,]
  # create llo 
  spacefolds <- CAST::CreateSpacetimeFolds(x=data_train,
                                           spacevar = spaceVar,
                                           k=noLoc, # of CV
                                           seed=100)
  # define control values  
  ctrl <- caret::trainControl(method="cv",
                              savePredictions = TRUE,
                              verbose=TRUE,
                              index=spacefolds$index,
                              indexOut=spacefolds$indexOut,
                              returnResamp = "all")
  # make it paralel
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)  
  ffs_model <- ffs(data_train[,predictors],
                   data_train[,response],
                   method=cl_method,
                   metric=metric_ffs,
                   trControl = ctrl,
                   withinSE=TRUE, 
                   tuneGrid = expand.grid(mtry = 2)
  )
  save(ffs_model,file = paste0(path_result,preffs,saveModelName))
  
  
  predictors <- data_train[,names(ffs_model$trainingData)[-length(names(ffs_model$trainingData))]]
  
  model_final <- train(predictors,
                       data_train[,response],
                       method = cl_method,
                       metric=metric_caret,
                       #returnResamp = "all",
                       importance =TRUE,
                       tuneLength = length(predictors),
                       trControl = ctrl)
  stopCluster(cl)
  
  save(model_final,file = paste0(path_result,prefin,saveModelName) )

  return(list(model_ffs,model_final,perf,cstat))
}