basicClassify<-function(rasterStack  = NULL,
                        training     = NULL, 
                        predictors   = c("R","G","B"),
                        response     = "ID",
                        names        = c("ID","R","G","B","A","FN"),
                        noLoc        = 3,
                        subset       = TRUE,
                        p            = 0.25,
                        proj         = NULL) {
  
  cat("\n::: start training & classification process...\n")
  trainingDF =  data.frame()
  #extract training Area pixel values
  for (j in 1:length(rasterStack)) {
    cat("\n::: extracting training data from image ",j," of ... ",length(rasterStack),"\n")
    categorymap<-rgeos::gUnionCascaded(training[[j]],id=training[[j]]@data$id)
    dataSet <- extract(rasterStack[[j]], categorymap,df=TRUE)
    ## add filename as category
    dataSet$FN= rasterStack[[j]]@filename
    names(dataSet)<- names
    dataSet=dataSet[complete.cases(dataSet),]
    trainingDF<-rbind(trainingDF, dataSet)
  }
  
  ## reclassify data frame
  trainingDF$ID[trainingDF$ID==1]="green" 
  trainingDF$ID[trainingDF$ID==2]="nogreen"
  trainingDF$ID[trainingDF$ID==3]="nogreen"
  trainingDF$ID[trainingDF$ID==4]="nogreen"
  trainingDF$ID[trainingDF$ID==5]="nogreen"
  trainingDF$ID <- as.factor(trainingDF$ID)
  
  ## save dataframe
  save(trainingDF, file = paste0(path_result,"trainingDF.RData"))
  # create subset
  if (subset) {
    trainIndex<-caret::createDataPartition(trainingDF$ID, p = 0.231,list=FALSE)
    data_train <- trainingDF[ trainIndex,]
    data_test <- trainingDF[-trainIndex,]
    }
  
  # create llo 
  spacefolds <- CAST::CreateSpacetimeFolds(x=data_train,
                                           spacevar = "FN",
                                           k=noLoc, # of CV
                                           seed=100)
  
  ctrl <- caret::trainControl(method="cv",
                              savePredictions = TRUE,
                              verbose=TRUE,
                              index=spacefolds$index,
                              indexOut=spacefolds$indexOut,
                              returnResamp = "all")
  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  ffs_model <- ffs(data_train[,predictors],
                   data_train[,response],
                   method="rf",
                   metric="Kappa",
                   trControl = ctrl,
                   withinSE=TRUE, 
                   tuneGrid = expand.grid(mtry = 2)
  )
  save(ffs_model,file = paste0(path_result,"ffs_model.RData"))
  
  
  predictors <- data_train[,names(ffs_model$trainingData)[-length(names(ffs_model$trainingData))]]
  
  model_final <- train(predictors,
                       data_train[,response],
                       method = "rf",
                       metric="AUC",
                       #returnResamp = "all",
                       importance =TRUE,
                       tuneLength = length(predictors),
                       trControl = ctrl)
  stopCluster(cl)
  
  #save(model_final,file = paste0(path_result,"model_final.RData") )
  
  perf <- model_final$pred[model_final$pred$mtry==model_final$bestTune$mtry,]
  #classificationStats(perf$pred,perf$obs) aus Rsenal
  #summary(lm(perf$pred~perf$obs))
  #plot(perf$pred,perf$obs)
  
  ###prediction
  img2predict<-list()
  classified_img<-list()
  cat("\n::: start prediction aka classifikation...\n")
  registerDoParallel(detectCores()-1)
  lst_all <- foreach(i = 1:lenth(imageFiles)) %dopar% {
    #for (i in 1:length(imageFiles)) {
    img2predict[[i]]<-stack(paste0("stack_",basename(imageFiles[i])))
    names(img2predict[[i]])<-c("R","G","B","A","VARI","NDTI","TGI","GLI","NGRDI","GLAI","Mean","Variance","Skewness","Kurtosis")
    classified_img[[i]]<- raster::predict(img2predict[[i]],
                                              model_final,
                                              progress="text")
    writeRaster(lst_all[[i]],
                filename = paste0(path_output,"classified_",basename(imageFiles[i])), 
                overwrite = TRUE)
  }
  unregister()
}

# calculate gdal derived DEM params
gdalDEMParam<- function(dem,fn,item=c("slope", "aspect","TRI","TPI","Roughness")) {
  if (is.null(item)){
    items<-c("slope", "aspect","TRI","TPI","Roughness")
  }
  s<-raster(fn)
  y<-yres(s)
  x<-xres(s)
  gdalwarp(dem,'dsm.tif',te=paste(extent(s)[1],' ',extent(s)[3],' ',extent(s)[2],' ',extent(s)[4]), tr=paste(x,' ', y),overwrite = TRUE, multi = TRUE)
  
  for (item in items){
    gdaldem(item,"dsm.tif",paste0(item,".tif"))
  }
}

# Split rgb
gdalsplit<-function(fn){
  directory<-dirname(fn)  
  for (i in seq(1:3)){
    gdal_translate(fn,paste0(directory,"/b",i,".tif"),b=i)
  }
}



# calculate orfeo HaralickTextureExtraction
otbHaraTex<- function(fn,module="otbcli_HaralickTextureExtraction",param=c("haraTex","4096",2,2,1,1,0,255,8,"higher")){
  directory<-dirname(fn)
  output<-paste0(directory, "/",param[1])
  command<-module
  command<-paste(command, "-in ", fn)
  command<-paste(command, "-out ", output)
  command<-paste(command, "-ram ",param[2])
  command<-paste(command, "-parameters.xrad ",param[3])
  command<-paste(command, "-parameters.yrad ",param[4])
  command<-paste(command, "-parameters.xoff ",param[5])
  command<-paste(command, "-parameters.yoff ",param[6])
  command<-paste(command, "-parameters.min ",param[7])
  command<-paste(command, "-parameters.max ",param[8])
  command<-paste(command, "-parameters.nbbin ",param[9])
  command<-paste(command, "-texture ",param[10])
  system(command)  
}

# calculate orfeo LocalStatisticExtraction
otbLocalStat<- function(fn,module="otbcli_LocalStatisticExtraction",param=c("localStat","4096",3)){
  directory<-dirname(fn)
  output<-paste0(directory, "/",param[1])
  command<-module
  command<-paste(command, "-in ", fn)
  command<-paste(command, "-out ", output)
  command<-paste(command, "-ram ",param[2])
  command<-paste(command, "-radius ",param[3])
  system(command)  
}


#' RGB indices 
#' 
#' @description
#' This function calculates various spectral indices from a RGB. It returns at least red green and blue as splitted channels in a stack. Additionally you can choose RGB indices.
#' \code{Raster*} object.  
#' 
#' @param rgb a \code{RasterStack} or \code{RasterBrick} object. 3
#' bands are mandatory (for RGB indices they should be: "red", "green" and "blue").
#' @param rgbi the implemented RGB indices currently \link{seealso}


#' A \code{RasterLayer} with the index calculates as:\cr
#' BI    sqrt((R**2+G**2+B*2)/3 Brightness Index\cr
#' CI    (R-G)/(R+G) Soil Colour Index\cr
#' GLI   (2*g - r - b)/(2*g + r + b) Green leaf index Vis Louhaichi et al. (2001)\cr
#' HI    (2*R-G-B)/(G-B) Primary colours Hue Index\cr
#' NDTI  (R-G)/(R+G) Normalized difference turbidity index Water\cr
#' NGRDI (G-R)/(G+R) Normalized green red difference index (sometimes GRVI) Tucker (1979)
#' RI    R**2/(B*G**3) Redness Index\cr
#' SI    (R-B)/(R+B) Spectral Slope Saturation Index\cr
#' TGI   -0.5[190(R670-R550)-120(R670 - R480)] The triangular greenness index (TGI) estimates chlorophyll concentration in leaves and canopies\cr
#' VARI  (green-red)/(green+red-blue). A Visible Atmospherically Resistant Index (VARI)\cr
#' VVI   (1-(r-30)/(r+30))*(1-(g-50)/(g+50))*(1-(b-1)/(b+1))
#  GLAI  (25 * (green - red) / (green +  red -  blue ) + 1.25 )
#' 
#' 
#' @name rgbIndices
#' @export rgbIndices
#' 
#' @references
#' 
#' Planetary Habitability Laboratory (2015): Visible Vegetation Index (VVI). Available online via \url{http://phl.upr.edu/projects/visible-vegetation-index-vvi}.\cr
#' Lacaux, J. P., Tourre, Y. M., Vignolles, C., Ndione, J. A., and Lafaye, M.: Classification of ponds from high-spatial resolution remote sensing: Application to Rift Valley Fever epidemics in Senegal, Remote Sens. Environ., 106, 66-74, 2007. \cr
#' Gitelson, A., et al.: Vegetation and Soil Lines in Visible Spectral Space: A Concept and Technique for Remote Estimation of Vegetation Fraction.  International Journal of Remote Sensing 23 (2002): 2537-2562. (VARI)\cr
#' MADEIRA, J., BEDIDI, A., CERVELLE, B., POUGET, M. and FLAY, N., 1997, Visible spectrometric indices of hematite (Hm) and goethite (Gt) content in lateritic soils: 5490 N. Levin et al. the application of a Thematic Mapper (TM) image for soil-mapping in Brasilia, Brazil. International Journal of Remote Sensing, 18, pp. 2835-2852.\cr
#' MATHIEU, R., POUGET, M., CERVELLE, B. and ESCADAFAL, R., 1998, Relationships between satellite-based radiometric indices simulated using laboratory reflectance data and typic soil colour of an arid environment. Remote Sensing of Environment, 66, pp. 17-28. \cr
#' Louhaichi, M., Borman, M.M., Johnson, D.E., 2001. Spatially located platform and aerial photography for documentation of grazing impacts on wheat. Geocarto International 16, 65-70.\cr
#' Tucker, C.J., 1979. Red and photographic infrared linear combinations for monitoring vegetation. Remote Sensing of Environment 8, 127-150.\cr
#' 
#' @seealso 
#' For a comprehensive overview of remote sensing indices have a look at: \url{http://www.indexdatabase.de/db/i.php}(A database for remote sensing indices)\cr
#' Wavelength ranges for overlapping digital camera bands are: red 580-670 nm, green 480-610 nm, and blue 400-520 nm (Hunt et al., 2005)
#' http://digitalcommons.unl.edu/cgi/viewcontent.cgi?article=2161&context=usdaarsfacpub
#' 
#' @examples
#' \notrun{
#' library(raster)
#' url <- "https://upload.wikimedia.org/wikipedia/commons/2/28/RGB_illumination.jpg"
#' dFile <- download.file(url, "Image.jpg")
#' img <- stack("Image.jpg") 
#' plotRGB(img)
#' rgbi <- rgbI(img)
#' plot(rgbI, col = gray(255:0/255))
#' }
#' 
#' 
rgbIndices<- function(red,green,blue,
                      rgbi=c("VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI","GLAI")) {
  
  ## compatibility check
  #  if (raster::nlayers(rgb) < 3)
  #    stop("Argument 'rgb' needs to be a Raster* object with at least 3 layers (usually red, green and blue).")
  
  ### processing
  
  
  ## separate visible bands
  # red <- raster::raster(rgb[[1]])
  # green <- raster::raster(rgb[[2]])
  # blue <- raster::raster(rgb[[3]])
  
  
  indices <- lapply(rgbi, function(item){
    ## calculate Visible Vegetation Index vvi
    if (item == "VVI"){
      cat("\n      calculate Visible Vegetation Index (VVI)")
      VVI <- (1 - abs((red - 30) / (red + 30))) * 
        (1 - abs((green - 50) / (green + 50))) * 
        (1 - abs((blue - 1) / (blue + 1)))
      names(VVI) <- "VVI"
      return(VVI)
      
    } else if (item == "VARI") {
      # calculate Visible Atmospherically Resistant Index (VARI)
      cat("\n      calculate Visible Atmospherically Resistant Index (VARI)")
      VARI <- (green - red) / (green + red - blue)
      names(VARI) <- "VARI"
      return(VARI)
      
    } else if (item == "NDTI") {
      ## Normalized difference turbidity index
      cat("\n      calculate Normalized difference turbidity index (NDTI)")
      NDTI <- (red - green) / (red + green)
      names(NDTI) <- "NDTI"
      return(NDTI)
      GLAI
    } else if (item == "RI") {
      # redness index
      cat("\n      calculate redness index (RI)")
      RI <- red**2 / (blue*green**3)
      names(RI) <- "RI"
      return(RI)
      
    } else if (item == "CI") {
      # CI Soil Colour Index
      cat("\n      calculate Soil Colour Index (CI)")
      CI <- (red - green) / (red + green)
      names(CI) <- "CI"
      return(CI)
      
    } else if (item == "BI") {
      #  Brightness Index
      cat("\n      calculate Brightness Index (BI)")
      BI <- sqrt((red**2 + green**2 + blue*2) / 3)
      names(BI) <- "BI"
      return(BI)
      
    } else if (item == "SI") {
      # SI Spectra Slope Saturation Index
      cat("\n      calculate Spectra Slope Saturation Index (SI)")
      SI <- (red - blue) / (red + blue) 
      names(SI) <- "SI"
      return(SI)
      
    } else if (item=="HI"){    
      # HI Primary colours Hue Index
      cat("\n      calculate Primary colours Hue Index (HI)")
      HI<-(2*red-green-blue)/(green-blue)
      names(HI) <- "HI"
      return(HI)
      
    } else if (item=="TGI"){
      # Triangular greenness index
      cat("\n      calculate Triangular greenness index (TGI)")
      TGI <- -0.5*(190*(red - green)- 120*(red - blue))
      names(TGI) <- "TGI"
      return(TGI)
      
    } else if (item=="GLI"){
      cat("\n      calculate Green leaf index (GLI)")
      # Green leaf index
      GLI<-(2*green-red-blue)/(2*green+red+blue)
      names(GLI) <- "GLI"
      return(GLI)
      
    } else if (item=="NGRDI"){
      # NGRDI Normalized green red difference index 
      cat("\n      calculate Normalized green red difference index  (NGRDI)")
      NGRDI<-(green-red)/(green+red) 
      names(NGRDI) <- "NGRDI"
      return(NGRDI)
      
    }  else if (item=="GLAI"){indices
      # NGRDI Normalized green red difference index 
      cat("\n      calculate greenish Leaf Area Index  (GLAI) (highly experimental)")
      vevi<-(green - red) / (green +  red -  blue )
      GLAI = (25 * vevi + 1.25 )
      names(GLAI) <- "GLAI"
      return(GLAI)
      
    }  
    
  })
  return(raster::stack(indices))
}

unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
