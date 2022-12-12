#' Convenient function to preprocess synthetic raster bands from a given RGB image and/or DTM/DSM data.
#' @description
#' Convenient function to preprocess synthetic raster bands from a given RGB image and/or DTM/DSM data.
#' Currently for all channels of the input images the following textures and indices can be calculated:\cr
#' RGB indices (17), color transformation (9), haralick (3 = 25 layers), statistics (4), edge (3), morphological (4), DEM Parameter (20).
#' All layers will be stacked (as an ENVI file). 
#' If wanted the raster values can be extracted to a data frames by overlaying corresponding vector data. NOTE: The vector data has to be named identically to the raster files.  This is useful
#' for for classification training purposes and covers usually step 1 of the random forest based
#' classification of UAV derived visible imagery and point clouds.

#'@details
#'
#' (01) make_syn_bands() calculation of spectral indices, basic spatial statistics and textures 
#' (02) 30_extract_training_df_RS.R extracting of training values over all channels according to training data\cr\cr
#' (03) 50_RS_LLO_rf_classification.R training and prediction using random forest and the forward feature selection method 
#'
#'@note If the function is used for stand alone extraction of the training data please provide both, the image stack containing the raster data plus the corresponding band names (usually saved as an Rdata file) and the corresponding shape file.
#'@note The workflow is intended to use the forward feature selection as described by \href{https://www.sciencedirect.com/science/article/pii/S1364815217310976}{Meyer et al. (2018)}.
#'This approach needs at least a pair of images that differ in time and/or space for a leave one location out validation mode. You may overcome this situation if you tile your image and provide for each tile separate training data.
#'If you just want to classify a single image by a single training file use the normal procedure as provided by the \code{\link[caret]{trainControl}} function.


#' @param calculateBands    logical. switch for set on calculation of syntheic bands and indices default = TRUE
#' @param patternImgFiles   character. mandantory string that exist in ech imagefile to be processes
#' @param prefixIdx         character. code string that will concatenated to the filename to identify the index file
#' @param prefixRun      character. general prefix of all result files of the current run default = "tmp"
#' @param patterndemFiles       character. mandantory if DEM data is processes. prefix of current DEM default = "dem"
#' @param prefixTrainImg    character. potential string of characters that is used in the beginning of a shape file prefixTrainImg_SHAPEFILENAME_suffixTrainImg
#' @param suffixTrainImg    character. potential string of characters that is used in the beginning of a shape file prefixTrainImg_SHAPEFILENAME_suffixTrainImg
#' @param prefixTrainGeom    character. potential string of characters that is used in the beginning of a shape file prefixTrainGeom_SHAPEFILENAME_suffixTrainGeom
#' @param suffixTrainGeom   character. potential string of characters that is used in the beginning of a shape file prefixTrainGeom_SHAPEFILENAME_suffixTrainGeom
#' @param channels          character. channels to be choosed options are c("PCA_RGBI","PCA_RGB", red", "green", "blue")  default =  c("PCA_RGBI") first principal component of the RGBI image stack
#' @param hara              logical. switch for using  HaralickTextureExtraction, default = TRUE. \cr
#' @param haraType          character. hara options, default is c("simple"), other  options are "advanced"  "higher" "all". NOTE:  "higher" takes a LOT of time
#' @param stat              logical. switch for using statistic default = TRUE the stas are mean,variance, curtosis, skewness
#' @param pardem            logical. switch for calculating dem parameter, default = FALSE
#' @param demType           character. ("hillshade","slope", "aspect","TRI","TPI","Roughness")
#' @param edge              logical. switch for using edge filtering default = TRUE
#' @param edgeType          character. edge options, default is c("gradient","sobel","touzi") all options are c("gradient","sobel","touzi")
#' @param morpho            logical. switch for using morphological filtering default = TRUE
#' @param morphoType        character. morphological options, default is c("dilate","erode","opening","closing") all options are ("dilate","erode","opening","closing")
#' @param rgbi              logical. switch for using rgbi index calcualtions default = TRUE
#' @param indices           character. RGB indices, default is c("VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI") all options are c("VVI","VARI","NDTI","RI","SCI","BI","SI","HI","TGI","GLI","NGRDI","GRVI","GLAI","HUE","CI","SAT","SHP")
#' @param kernel            numeric. size of kernel for filtering and statistics, default is  3
#' @param saga_morphoMethod  numeric. saga morphometric method
#' @param minScale  numeric. in scale for multi scale TPI
#' @param maxScale  numeric. max scale for multi scale TPI
#' @param numScale  numeric. number of scale for multi scale TPI
#' @param currentDataFolder  NULL folder to image (and shape) data
#' @param currentIdxFolder  NULL folder for saving the results
#' @param cleanRunDir  logical. TRUE logical switch for deleting the calculated tifs, default is TRUE
#' @param otbLinks     list. OTB tools cli paths
#' @param sagaLinks     list. SAGA tools cli paths
#' @param gdalLinks     list. GDAL tools cli paths 
#' @param path_run         list project pathes and more
#' @param quiet         returning system messages 
#' 
#' @return data frame containing for each drawn point the pixel values of the rasterstack data
#' @examples
#' 
#'\dontrun{
#'
#' require(uavRst)
#' require(link2GI)
#' 
#' # create and check the links to the GI software
#' sagaLinks<-link2GI::linkSAGA()
#' gdalLinks<-link2GI::linkGDAL()
#' otbLinks<-link2GI::linkOTB()
#' 
#' 
#' ##- create and set folders
#' ##- please mind that the pathes are exported as global variables
#' paths<-link2GI::initProj(projRootDir = tempdir(),
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")
#'   
#' ##- clean runtime folder
#' unlink(file.path(tempdir(),"*"), force = TRUE)
#'   
#' ##- get the tutorial data
#' url<-"https://github.com/gisma/gismaData/raw/master/uavRst/data/tutorial_data.zip"
#' utils::download.file(url,
#'                        file.path(tempdir(),"tutorial_data.zip"))
#' unzip(zipfile = file.path(tempdir(),"tutorial_data.zip"), 
#'                           exdir = R.utils::getAbsolutePath(path_run))
#'   
#'   ##- calculate some synthetic channels from the RGB image and the canopy height model
#'   ##- then extract the from the corresponding training geometries the data values aka trainingdata
#'   trainDF <- make_syn_bands(calculateBands    = TRUE,
#'                       suffixTrainGeom   = "",
#'                       prefixIdx        = "index",
#'                       patternImgFiles   = "rgb" ,
#'                       patterndemFiles   = "chm",
#'                       prefixRun         = "tutorial",
#'                       prefixTrainImg    = "",
#'                       rgbi              = TRUE,
#'                       indices           = c("TGI","CI"),
#'                       channels          = c("red"),
#'                       hara              = TRUE,
#'                       haraType          = c("simple"),
#'                       stat              = TRUE,
#'                       edge              = TRUE,
#'                       morpho            = TRUE,
#'                       pardem            = TRUE,
#'                       #demType           = c("slope", "MTPI"),
#'                       kernel            = 3,
#'                       currentDataFolder = path_run,
#'                       currentIdxFolder  = path_run,
#'                       sagaLinks = sagaLinks,
#'                       gdalLinks = gdalLinks,
#'                       otbLinks =otbLinks)
#' 
#' # use ffs_train as next step for rf classification issues
#' 
#' }
#' @export make_syn_bands


make_syn_bands<- function ( calculateBands    = TRUE,
                            prefixRun         = "temp",
                            patterndemFiles   = "dem",
                            prefixTrainImg    = "",
                            prefixTrainGeom   = "",
                            suffixTrainImg    = "",
                            suffixTrainGeom   = "",
                            prefixIdx        = "index",
                            patternImgFiles   = "",
                            channels          = c("PCA_RGB"),
                            hara              = TRUE,
                            haraType          = c("simple","advanced","higher"),
                            stat              = TRUE,
                            edge              = TRUE,
                            edgeType          = c("gradient","sobel","touzi"),
                            morpho            = TRUE,
                            morphoType        = c("dilate","erode","opening","closing"),
                            rgbi              = TRUE,
                            indices           = c("VVI","VARI","NDTI","RI","SCI","BI",
                                                  "SI","HI","TGI","GLI","NGRDI","GRVI",
                                                  "GLAI","HUE","CI","SAT","SHP") ,
                            pardem            = FALSE,
                            demType           = c("hillshade","slope", "aspect","TRI","TPI","Roughness",
                                                  "SLOPE","ASPECT", "C_GENE","C_PROF","C_PLAN","C_TANG",
                                                  "C_LONG","C_CROS","C_MINI","C_MAXI","C_TOTA","C_ROTO","MTPI"),
                            saga_morphoMethod = 6,
                            minScale = 1,
                            maxScale = 8,
                            numScale = 2,
                            kernel            = seq(3,9,2),
                            quiet             = TRUE,
                            currentDataFolder = NULL,
                            currentIdxFolder  = NULL,
                            cleanRunDir        = FALSE,
                            sagaLinks = NULL,
                            gdalLinks = NULL,
                            otbLinks = NULL,
                            path_run = NULL){
  
  if (!exists("path_run")) {
    path_run = paste0(tempdir(),"/")
    if (Sys.info()["sysname"]=="Windows") path_run = paste0(tempdir(),"\\")
  }
  if (!rgbi) hara <- stat <- edge <- morpho <- FALSE
  
  retStack<-list()
  
  if (is.null(gdalLinks))   gdal<- link2GI::linkGDAL()
  else gdal<-gdalLinks
  if (is.null(sagaLinks))   saga<- link2GI::linkSAGA()
  else  saga<- sagaLinks
  if (is.null(otbLinks))  otb<- link2GI::linkOTB()
  else  otb<- otbLinks
  
  sagaCmd<-saga$sagaCmd
  path_OTB <- otb$pathOTB
  catOK   <- getCrayon()[[3]]
  catHead <- getCrayon()[[4]]
  catErr  <- getCrayon()[[2]]
  catNote <- getCrayon()[[1]]
  
  #
  currentDataFolder<- currentDataFolder #paste0(path_data_training)
  currentIdxFolder<- currentIdxFolder # paste0(path_data_training_idx)
  
  if (((stat == TRUE) || (hara == TRUE) || (edge == TRUE) || (morpho == TRUE)) & !otb$exist) stop("OTB missing - please check")
  
  message(catHead("\n--- calculate synthetic channels ---\n"))
  
  # create list of image files to be processed
  #imageFiles <- list.files(pattern=paste0("^",prefixRun,"*","tif"), path=currentDataFolder, full.names=TRUE)
  imageFiles <-Sys.glob(paths = file.path(currentDataFolder,paste0(patternImgFiles,"*","tif")))
  demFiles <- Sys.glob(paths = file.path(currentDataFolder,paste0(patterndemFiles,"*","tif")))
  file.copy(imageFiles ,file.path(R.utils::getAbsolutePath(path_run),basename(imageFiles)))
  # create a counter for all input files to be processed
  counter<- max(length(demFiles),length(imageFiles))
  
  # create list vars
  bandNames <- flist <- dellist <- list()
  
  for (i in 1:counter){
    bandNames <- list()
    # if calc pardem
    if (pardem){
      
      #message(catNote(":::: processing dem... ",demType,"\n"))
      flist<-append(flist, Sys.glob(demFiles[i]))
      dellist <- append(dellist, file.path(R.utils::getAbsolutePath(path_run),"dem2.tif"))
      bandNames <-append(bandNames,"dem")
      morpho_dem(dem = demFiles[i],
                 item = demType,
                 saga_morphoMethod = saga_morphoMethod,
                 minScale = minScale,
                 maxScale = maxScale,
                 numScale = numScale,
                 gdalLinks=  gdal,
                 sagaLinks=  saga)
      
      flist<-append(flist, Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(demType,".tif"))))
      dellist <- append(dellist, Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(demType,".*"))))
      for (item in demType)
        bandNames <-append(bandNames,make_bandnames(dem = item))
      
    }
    
    # for all images do
    if (rgbi){
      message(catNote(":::: processing indices of...",basename(imageFiles[i]),"\n"))
      r<-terra::rast(imageFiles[i])
      # calculate and stack r,g,b and requested indices
      idx_stack=rgb_indices(r[[1]],r[[2]],r[[3]],indices)
      rgb_rgbi<-c(r[[1]],r[[2]],r[[3]],terra::rast(idx_stack))
      bandNames <- append(bandNames,make_bandnames(rgbi = indices))
      names(rgb_rgbi)<-append(c("red","green","blue"),indices)
      message(catOK("\n     save ...",paste0(path_run,"rgbi_",basename(imageFiles[i])),"\n"))
      #if (!inMemory(rgb_rgbi)) readAll(rgb_rgbi)
      terra::writeRaster(rgb_rgbi,
                         file.path(R.utils::getAbsolutePath(path_run),paste0("rgbi_",basename(imageFiles[i]))),
                         #progress = "text",
                         overwrite=TRUE,gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
      
      flist<-append(flist, file.path(R.utils::getAbsolutePath(path_run),paste0("rgbi_",basename(imageFiles[i]))))
      dellist <- append(dellist, file.path(R.utils::getAbsolutePath(path_run),paste0("rgbi_",basename(imageFiles[i]))))
    }
    # if RGB transform
    if (rgbi){
      # assign bandnumber according to name
      message("\n")
      for (filterBand in channels){
        if (filterBand=="red") bandNr <- 1
        else if (filterBand=="green") bandNr <- 2
        else if (filterBand=="blue") bandNr <- 3
        else if (filterBand=="PCA_RGB") bandNr <- 4 
        else if (filterBand=="PCA_RGBI") bandNr <- 5
        
        message(catNote(":::: calculate PCA channel...",paste0(filterBand,"_",basename(imageFiles[i])),"\n"))
        if (bandNr == 4 || bandNr == 5) {
          fbFN<-file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"_",basename(imageFiles[i])))
          pca = link2GI::parseOTBFunction("DimensionalityReduction",otb)
          if (bandNr == 5) {
            pca$input_in=file.path(R.utils::getAbsolutePath(path_run),paste0("rgbi_",basename(imageFiles[i])))
            pca$out=file.path(R.utils::getAbsolutePath(path_run),paste0("otb_pca_rgbi_",basename(imageFiles[i])))
          }
          else if (bandNr == 4) {
            pca$input_in=file.path(R.utils::getAbsolutePath(path_run),basename(imageFiles[i]))
            pca$out=file.path(R.utils::getAbsolutePath(path_run),paste0("otb_pca_",basename(imageFiles[i])))
          }
          pca$progress="true"
          pca$normalize="true"
          rpc=link2GI::runOTB(pca,gili = otb,retRaster = TRUE ,quiet = quiet)  
          terra::writeRaster(rpc[[1]],
                             fbFN,
                             # progress = "text",
                             overwrite=TRUE,gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
          
          
          # flist<-append(flist,  fbFN)
          # dellist <-append(dellist,  fbFN)
          # bandNames <-append(bandNames,make_bandnames(pca = paste0(channels,"_",filterBand)))
          
          flist<-append(flist,fbFN)
          dellist <-append(dellist,fbFN)
          bandNames <-append(bandNames,paste0(make_bandnames(pca = TRUE),"_",filterBand))
        }                                     
        else {
          # export single channel for synthetic band calculation
          # if (filterBand!="") {
          message(catNote(":::: write single channel...",paste0(filterBand,"_",basename(imageFiles[i])),"\n"))
          #if (!inMemory(rgb_rgbi[[bandNr]])) readAll(rgb_rgbi[[bandNr]])
          terra::writeRaster(rgb_rgbi[[bandNr]],
                             file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"_",basename(imageFiles[i]))),
                             #progress = "text",
                             overwrite=TRUE,gdal=c("COMPRESS=DEFLATE", "TFW=YES"))
          fbFN<-file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"_",basename(imageFiles[i])))}
        rm(rgb_rgbi) 
        rm(rpc) 
        # filterband
        for (k in kernel){
          if (stat){
            message(catNote(":::: processing stats...",fbFN," Kernelsize: ", k,"\n"))
            otb_stat(input = fbFN,
                     out = file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,k,"stat_",basename(imageFiles[i]))),
                     ram = "4096",
                     radius =  k,
                     otbLinks=  otb,
                     gdalLinks = gdal)
            add_fn= Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,k,"stat_*")))
            flist<-append(flist,add_fn)
            dellist <-append(dellist,add_fn)
            bandNames <-append(bandNames,paste0(k,make_bandnames(stat = TRUE),"_",filterBand))
          }
          # if calc edge
          if (edge){
            for (edges in edgeType){
              message(catNote(":::: processing edge... ",edges," Kernelsize: ", k,"\n"))
              otbtex_edge(input = fbFN,
                          out = file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,k,edges,basename(imageFiles[i]))),
                          touzi_xradius=k,
                          touzi_yradius=k,
                          filter = edges,
                          otbLinks=  otb,
                          gdalLinks = gdal)
              add_fn = Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,k,edges,"*")))
              flist<-append(flist,add_fn)
              dellist<-append(dellist,add_fn)
              bandNames <-append(bandNames,paste0(k,make_bandnames(edge = paste0(edges,"_",filterBand))))
            }
          }
          
          # if calc morpho
          if (morpho){
            for (morphos in morphoType){
              message(catNote(":::: processing morpho... ",morphos," Kernelsize: ", k,"\n"))
              otbtex_gray(input = fbFN,
                          out = file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,k,morphos,basename(imageFiles[i]))),
                          filter = morphos,
                          xradius=k,
                          yradius=k,
                          otbLinks=  otb,
                          gdalLinks = gdal)
              add_fn = Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,k,morphos,"*")))
              flist<-append(flist,add_fn)
              dellist<-append(dellist,add_fn)
              bandNames <-append(bandNames,paste0(k,make_bandnames(edge = paste0(k,morphos,"_",filterBand))))
            }
          }
          # if calc haralick
          if (hara){
            for (type in haraType){
              message(catNote(":::: processing haralick... ",type,"\n"))
              otbtex_hara(x = fbFN,
                          output_name=file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"hara_",k,basename(imageFiles[i]))),
                          texture = type,
                          otbLinks=  otb,
                          gdalLinks = gdal)
              flist<-append(flist,Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"hara_*",type,"*"))))
              dellist<-append(dellist,Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"hara_*",type,"*"))))
              nband<-raster::nbands(raster::raster(Sys.glob(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"hara_*",type,"*")))))
              bandNames <-append(bandNames,paste0(make_bandnames(bandNames = type,l_raster=nband),"_",filterBand))
            }
          }
          # delete single channel for synthetic channel calculation
          #file.remove(file.path(R.utils::getAbsolutePath(path_run),paste0(filterBand,"_",basename(imageFiles[i]))))
        }
      }# end of single channnel calculation
    }
    # create an alltogether stack
    if (rgbi)  tmpFN<-paste0(substr(basename(imageFiles[i]),1,nchar(basename(imageFiles[i]))-4))
    else if (length(demFiles)>= i)  tmpFN<-paste0(substr(basename(demFiles[i]),1,nchar(basename(demFiles[i]))-4))
    else return(message(catErr("\nhopefully done\n You are mixing RGB an DEM input files. You may do this but only if they are of the same extent etc. and if each image file has a corresponding dem file\n NOTE the dem filename MUST have a prefix default is 'dem_'.")))
    message(catOK("     save ...",paste0(prefixIdx, tmpFN),"\n"))
    # r<-raster::brick(raster::stack(flist)) qgis cannot read heder
    if (length(demFiles) > 0)  for (k in 1:length(demFiles)) flist[-grepl(pattern = demFiles[k],flist)]
    for (k in 1:length(imageFiles)) flist[-grepl(pattern = imageFiles[k],flist)]
    r<-raster::stack(paste0(flist))
    if (raster::nlayers(r)!=length(bandNames)) stop("\n Number of names and layers differ...\n most common case is a broken cleanup of the runtime directory!")
    names(r)<-bandNames
    
    # write file to envi
    if (nlayers(r) <= 256){
      message(catNote(":::: writing data file... ",paste0(currentIdxFolder,"/", prefixIdx,tmpFN),"\n"))
      saveRDS(r,paste0(currentIdxFolder,"/", prefixIdx,tmpFN,".rds"))
      terra::writeRaster(r,
                         paste0(currentIdxFolder,"/", prefixIdx,tmpFN,".tif"),
                         #progress ="text",
                         overwrite=TRUE,gdal=c("COMPRESS=DEFLATE", "TFW=YES"))}
    else {
      message(catErr(":::: you have more than 256 Layers writing an envi file. \n You NUST reassign the bandnames when using the envi file! \n"))
      terra::writeRaster(r,
                         paste0(currentIdxFolder,"/", prefixIdx,tmpFN),
                         format="ENVI",
                         #progress ="text",
                         #options="COMPRESS=LZW",
                         overwrite=TRUE)
      
    }
    
    rlist<- file.path(R.utils::getAbsolutePath(paste0(currentIdxFolder,"/", prefixIdx,tmpFN)))
    
    # cleanup runtime files lists...
    if (cleanRunDir) {
      message(catNote(":::: removing temp files...\n"))
      res<-file.remove(unlist(dellist))
    }
    flist <- dellist <- list()
    
  }
  
  # save bandname list we need it only once
  save(bandNames,file = paste0(currentIdxFolder,prefixRun,"bandNames.RData"))
  
  
  message(catErr(":::: resulting files...",rlist,"\n"))
  message(catErr(":::: corresponding band names... ",paste0(currentIdxFolder,prefixRun,"bandNames.RData"),"\n"))
  message(catHead("\n--- calculation of synthetic bands is finished ---\n"))
  return(r)
}