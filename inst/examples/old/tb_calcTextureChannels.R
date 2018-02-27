# Calculate selected texture parameters based on gray level properties
# The first otb function otbHaraTex has some extended comments 
# describing a straightforward wrapping concept
# textureVariables provides a good practice example for using 
# the R capabilities of paralell tasking 
# 
#' This functions calls the glcm package with standard settings 
#' @note for the use of textureVariables a glcm wrapper function 
#'       a raster* object is required
#' @param x rasterLayer or a rasterStack containing different channels
#' @param nrasters vector of channels to use from x. Default =nlayers(x)
#' @param kernelSize vector of numbers indicating the environment sizes for which the textures are calculated
#' @param stats string vector of parameters to be calculated.see \code{\link{glcm}}
#' @param n_grey number of grey values. see \code{\link{glcm}}
#' @param parallel logical value indicating whether parameters are calculated parallel or not
#' @param min_x for each channel the minimum value which can occur. If NULL then the minimum value from the rasterLayer is used.
#' @param max_x for each channel the maximum value which can occur. If NULL then the maximum value from the rasterLayer is used.
#' This functions calls the glcm function from \link{glcm} with standard settings
#' and returns list of RasterStacks containing the texture parameters for each combination of channel and kernelSize  
#' @author Hanna Meyer
#' 
#' @note More information at the texture tutorial site of
#' \link{http://www.fp.ucalgary.ca/mhallbey/more_informaton.htm}(Mryka Hall-Beyer)
#' Keep in mind that:\cr
#' Homogeneity is correlated with Contrast,  r = -0.80
#' Homogeneity is correlated with Dissimilarity, r = -0.95
#' GLCM Variance is correlated with Contrast,  r= 0.89
#' GLCM Variance is correlated with Dissimilarity,  r= 0.91
#' GLCM Variance is correlated with Homogeneity,  r= -0.83
#' Entropy is correlated with ASM,  r= -0.87
#' GLCM Mean and Correlation are more independent. For the same image,
#' GLCM Mean shows  r< 0.1 with any of the other texture measures demonstrated in this tutorial.
#' GLCM Correlation shows  r<0.5 with any other measure.
#' @export textureVariables
#' @examples 
#' \dontrun{
#' ## example on how to calculate texture from a list of channels
#' 
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' r<- raster::stack(paste0(getwd(),"4490600_5321400.tif"))
#' 
#' # call glcm wrapper
#' result <- textureVariables(r,nrasters=1:3,
#' stats=c("mean", "variance", "homogeneity"))
#' 
#' #plot the results from VIS0.6 channel:
#' raster::plot(unlist(unlist(glcm$size_3$X4490600_5321400.1)))
#' }
#' @seealso \code{\link{glcm}}

textureVariables <- function(x,
                             nrasters=1:nlayers(x),
                             kernelSize=c(3),
                             stats=c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", 
                                     "second_moment", "correlation"),
                             shift=list(c(0,1), c(1,1), c(1,0),c(1,-1)),
                             parallel=TRUE,
                             n_grey = 8,
                             min_x=NULL,
                             max_x=NULL){
  
  
  if (parallel){
    doParallel::registerDoParallel(parallel::detectCores()-1)
  }
  
  
  #set values larger than the max/min value to the max/minvalue. 
  #Otherwise NA would be used
  if(!is.null(min_x)){
    if (length(nrasters)>1){
      for (i in nrasters){
        x[[i]]=reclassify(x[[i]], c(max_x[i],Inf,max_x[i]))
        x[[i]]=reclassify(x[[i]], c(-Inf,min_x[i],min_x[i]))
      }
    } else { # only one raster
      x=reclassify(x, c(max_x,Inf,max_x))
      x=reclassify(x, c(-Inf,min_x,min_x)) 
    }
  }
  
  
  glcm_filter<-list()
  for (j in 1:length(kernelSize)){
    if (class (x)=="RasterStack"||class (x)=="RasterBrick"){  
      if (parallel){
        glcm_filter[[j]]<-foreach::foreach(i=nrasters,
                                  .packages= c("glcm","raster"))%dopar%{
                                    glcm::glcm(x[[i]], 
                                         window = c(kernelSize[j], kernelSize[j]), 
                                         shift=shift,
                                         statistics=stats,n_grey=n_grey,
                                         min_x=min_x[i],max_x=max_x[i],
                                         na_opt="center")
                                  } 
      } else {
        glcm_filter[[j]]<-foreach::foreach(i=nrasters,
                                  .packages= c("glcm","raster"))%do%{
                                    raster::mask(glcm::glcm(x[[i]], 
                                              window = c(kernelSize[j], kernelSize[j]), 
                                              shift=shift,
                                              statistics=stats,n_grey=n_grey,
                                              min_x=min_x[i],max_x=max_x[i],
                                              na_opt="center"), x[[i]])
                                  }
      }
      names(glcm_filter[[j]])<-names(x)[nrasters]
    } else {
      glcm_filter[[j]]<-raster::mask(glcm::glcm(x, window = c(kernelSize[j], kernelSize[j]), 
                                  shift=shift,
                                  statistics=stats,n_grey=n_grey,
                                  min_x=min_x,max_x=max_x,
                                  na_opt="center"), x)
    }   
  }
  doParallel::stopImplicitCluster()
  names(glcm_filter)<-paste0("size_",kernelSize)
  return(glcm_filter)
}


if ( !isGeneric("otbTexturesHaralick") ) {
  setGeneric("otbTexturesHaralick", function(x, ...)
    standardGeneric("otbTexturesHaralick"))
}

#' OTB wrapper for Haralick's simple, advanced and higher order texture features
#'@description  OTB wrapper for calculating Haralick's simple, advanced and higher order texture features on every pixel in each channel of the input image. A list of RasterStacks containing the texture parameters for each 
#' combination of channel and filter  
#' @param x A \code{\link{Raster*}} object or a \href{http://www.gdal.org/frmt_gtiff.html}{GeoTiff} containing one or more gray  value bands
#' @param output_name string pattern vor individual naming of the output file(s)
#' @param parameters.xyrad list with the x and y radius in pixel indicating the kernel sizes for which the textures are calculated
#' @param parameters.xyoff  vector containg the directional offsets. Valid combinations are: list(c(1,1),c(1,0),c(0,1),c(1,-1))
#' @param parameters.minmax   minimum/maximum gray value which can occur. 
#' @param parameters.nbbin number of gray level bins (classes)
#' @param texture type of filter "all" for all, alternative one of "simple" "advanced" "higher"
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param return_raster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @param path_output path_output

#' @references Haralick, R.M., K. Shanmugam and I. Dinstein. 1973. Textural Features for Image Classification.
#' IEEE Transactions on Systems, Man and Cybernetics. SMC-3(6):610-620.\cr
#' \href{https://www.orfeo-toolbox.org/packages/OTBSoftwareGuide.pdf}{Orfeo Toolbox Sofware Guide, 2016}\cr
#' \href{https://www.orfeo-toolbox.org//doxygen/classotb_1_1ScalarImageToTexturesFilter.html}{"simple"}:\cr
#' computes the following 8 local Haralick textures features: Energy, Entropy, Correlation, Inverse Difference Moment, Inertia, Cluster Shade, Cluster Prominence and Haralick Correlation. They are provided in this exact order in the output image. Thus, this application computes the following Haralick textures over a neighborhood with user defined radius.\cr
#' To improve the speed of computation, a variant of Grey Level Co-occurrence Matrix(GLCM) called Grey Level Co-occurrence Indexed List (GLCIL) is used. Given below is the mathematical explanation on the computation of each textures. Here \code{g( i,j)} is the frequency of element in the GLCIL whose index is \code{i,j}. GLCIL stores a pair of frequency of two pixels from the given offset and the cell index \code{(i,j)} of the pixel in the neighborhood window. Where each element in GLCIL is a pair of pixel index and it's frequency, \code{g(i,j)} is the frequency value of the pair having index is \code{i,j}.\cr\cr
#' Energy  \if{html}{\figure{form_Energy.png}{options:alt="Energy"}}\cr
#' Entropy  \if{html}{\figure{form_entropy1.png}{options:alt="Entropy"}}\cr
#' Correlation  \if{html}{\figure{form_Correlation.png}{options:alt="Correlation"}}\cr
#' Inertia (contrast)  \if{html}{\figure{form_Contrast.png}{options:alt="Inertia (Contrast)"}}\cr
#' Cluster Shade  \if{html}{\figure{form_Cluster_Shade.png}{options:alt="Cluster Shade"}}\cr
#' Cluster Prominence  \if{html}{\figure{form_Cluster_Prominence.png}{options:alt="Cluster Prominence"}}\cr
#' Haralick's Correlation  \if{html}{\figure{form_Hara_Cor.png}{options:alt="Haralick's Correlation"}}\cr\cr
#' 
#' \href{https://www.orfeo-toolbox.org//doxygen/classotb_1_1ScalarImageToAdvancedTexturesFilter.html}{"advanced"}:\cr
#' computes the following 10 texture features: Mean, Variance, Dissimilarity, Sum Average, Sum Variance, Sum Entropy, Difference of Entropies, Difference of Variances, IC1 and IC2. They are provided in this exact order in the output image. The textures are computed over a sliding window with user defined radius. To improve the speed of computation, a variant of Grey Level Co-occurrence Matrix(GLCM) called Grey Level Co-occurrence Indexed List (GLCIL) is used. Given below is the mathematical explanation on the computation of each textures. Here \code{g( i,j)} is the frequency of element in the GLCIL whose index is \code{ i,j}. GLCIL stores a pair of frequency of two pixels from the given offset and the cell index \code{( i,j)} of the pixel in the neighborhood window. (where each element in GLCIL is a pair of pixel index and it's frequency, \code{g( i,j)} is the frequency value of the pair having index is \code{ i,j}.\cr\cr
#' 
#' Mean  \if{html}{\figure{form_mean.png}{options:alt="Mean"}}\cr
#' Sum of squares: Variance  \if{html}{\figure{form_form_sum_of_squares_variance.png}{options:alt="Sum of squares: Variance"}}\cr
#' Dissimilarity \if{html}{\figure{form_Dissimilarity.png}{options:alt="Dissimilarity"}}\cr
#' Sum average \if{html}{\figure{form_Sum_average.png}{options:alt="Sum average"}}\cr
#' Sum Variance \if{html}{\figure{form_Sum_Variance.png}{options:alt="Sum Variance"}}\cr
#' Sum Entropy \if{html}{\figure{form_Sum_Entropy.png}{options:alt="Sum Entropy"}}\cr    
#' Difference variance \if{html}{\figure{form_Difference_variance.png}{options:alt="Difference variance"}}\cr    
#' Difference entropy \if{html}{\figure{form_Difference_entropy.png}{options:alt="Difference entropy"}}\cr    
#' Information Measures of Correlation IC1 \if{html}{\figure{form_Information_Measures_of_Correlation_IC1.png}{options:alt="Information Measures of Correlation IC1"}}\cr    
#' Information Measures of Correlation IC2 \if{html}{\figure{form_Information_Measures_of_Correlation_IC2.png}{options:alt="Information Measures of Correlation IC2"}}\cr\cr    
#'
#' \href{https://www.orfeo-toolbox.org//doxygen/classotb_1_1ScalarImageToHigherOrderTexturesFilter.html}{"higher"}: \cr\cr
#' computes 11 local higher order statistics textures coefficients based on the grey level run-length matrix.
#' It computes the following Haralick textures over a sliding window with user defined radius: (where p( i,j) is the element in cell  i,j of a normalized Run Length Matrix (n_r) is the total number of runs and n_p is the total number of pixels ):\cr
#' 
#' Short Run Emphasis \if{html}{\figure{form_Short_Run_Emphasis.png}{options:alt="Short_Run_Emphasis"}}\cr
#' Long Run Emphasis \if{html}{\figure{form_Long_Run_Emphasis.png}{options:alt="Long Run Emphasis"}}\cr
#' Grey-Level Nonuniformity \if{html}{\figure{form_Grey_Level_Nonuniformity.png}{options:alt="Grey-Level Nonuniformity"}}\cr
#' Run Length Nonuniformity \if{html}{\figure{form_Run_Length_Nonuniformity.png}{options:alt="Run Length Nonuniformity"}}\cr
#' Low Grey-Level Run Emphasis \if{html}{\figure{form_Low_Grey_Level_Run_Emphasis.png}{options:alt="Low Grey-Level Run Emphasis"}}\cr
#' High Grey-Level Run Emphasis \if{html}{\figure{form_High_Grey_Level_Run_Emphasis.png}{options:alt="High Grey-Level Run Emphasis"}}\cr
#' Short Run Low Grey-Level Emphasis \if{html}{\figure{form_Short_Run_Low_Grey_Level_Emphasis.png}{options:alt="Short Run Low Grey-Level Emphasis"}}\cr
#' Short Run High Grey-Level Emphasis \if{html}{\figure{form_Short_Run_High_Grey_Level_Emphasis.png}{options:alt="Short Run High Grey-Level Emphasis"}}\cr
#' Long Run Low Grey-Level Emphasis \if{html}{\figure{form_Long_Run_Low_Grey_Level_Emphasis.png}{options:alt="Long Run Low Grey-Level Emphasis"}}\cr
#' Long Run High Grey-Level Emphasis \if{html}{\figure{form_Long_Run_High_Grey_Level_Emphasis.png}{options:alt="Long Run High Grey-Level Emphasis"}}\cr

#' @author Chris Reudenbach, Thomas Nauss
#' @note 
#' The following Haralick textures are largely comparable to the results as derived by the \code{\link{glcm}} package. Find more information about the these common texture indices at the tutorial site of
#' \href{http://www.fp.ucalgary.ca/mhallbey/more_informaton.htm}{Mryka Hall-Beyer}\cr
#' 
#' @name otbTexturesHaralick
#' @export otbTexturesHaralick
#' @examples 
#' \dontrun{
#' http://www.ldbv.bayern.de/file/zip/10430/DGM_1_ascii.zip
#' # get some typical authority generated data
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,
#'       files = grep(".tif", unzip(res,list = TRUE)$Name,value = TRUE),
#'       junkpaths = TRUE,
#'       overwrite = TRUE)
#' 
#' # first initialisation of the OTB environment
#' link2GI::linkOTB()
#' 
#' # calculate all Haralick-textures
#' otbTexturesHaralick(x=file.path(getwd(),
#'                     basename(grep(".tif", 
#'                     unzip(res,list = TRUE)$Name,
#'                     value = TRUE))))
#' }
NULL



# Function using RasterBrick ---------------------------------------------------
#' OTB wrapper for Haralick's simple, advanced and higher order texture features
#'@description  OTB wrapper for calculating Haralick's simple, advanced and higher order texture features on every pixel in each channel 
#' @rdname otbTexturesHaralick
#'
setMethod("otbTexturesHaralick", 
          signature(x = "RasterBrick"), 
          function(x,
                   texture="all",
                   path_output=NULL,
                   return_raster=TRUE,
                   parameters.xyrad=list(c(1,1)),
                   parameters.xyoff=list(c(1,1)),
                   parameters.minmax=c(0,255),
                   parameters.nbbin=8,
                   channel=NULL,
                   verbose=TRUE,
                   ram="8192"){
            if (is.null(path_output)) path_output <- file.path(getwd(),"/")
            raster::writeRaster(x, file = file.path(path_output, "tmp.tif"), overwrite = TRUE)
            x <- paste0(path_output, "tmp.tif")
            tempout <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
            ret_textures <- otbTexturesHaralick(x = x,
                                                texture = texture,
                                                output_name = tempout,
                                                path_output = path_output,
                                                return_raster = TRUE,
                                                parameters.xyrad = parameters.xyrad,
                                                parameters.xyoff = parameters.xyoff,
                                                parameters.minmax = parameters.minmax,
                                                parameters.nbbin = parameters.nbbin,
                                                channel = channel,
                                                verbose = verbose,
                                                ram = ram)
            file.remove(x)
            tmpfiles <- list.files(path_output, 
                                   pattern = utils::glob2rx(paste0("*", tempout, "*")),
                                   full.names = TRUE)
            file.remove(tmpfiles)
            return(ret_textures)
          })


# Function using RasterLayer ---------------------------------------------------
#' OTB wrapper for Haralick's simple, advanced and higher order texture features
#'@description  OTB wrapper for calculating Haralick's simple, advanced and higher order texture features on every pixel in each channel 
#' @rdname otbTexturesHaralick
#'
setMethod("otbTexturesHaralick", 
          signature(x = "RasterLayer"), 
          function(x,
                   texture="all",
                   path_output=NULL,
                   return_raster=TRUE,
                   parameters.xyrad=list(c(1,1)),
                   parameters.xyoff=list(c(1,1)),
                   parameters.minmax=c(0,255),
                   parameters.nbbin=8,
                   channel=NULL,
                   verbose=FALSE,
                   ram="8192"){
            ret_textures <- otbTexturesHaralick(x = raster::brick(x),
                                                texture = texture,
                                                path_output = path_output,
                                                return_raster = return_raster,
                                                parameters.xyrad = parameters.xyrad,
                                                parameters.xyoff = parameters.xyoff,
                                                parameters.minmax = parameters.minmax,
                                                parameters.nbbin = parameters.nbbin,
                                                channel = channel,
                                                verbose = verbose,
                                                ram = ram)
            return(ret_textures)
          })


# Function using RasterStack ---------------------------------------------------
#' OTB wrapper for Haralick's simple, advanced and higher order texture features
#'@description  OTB wrapper for calculating Haralick's simple, advanced and higher order texture features on every pixel in each channel 
#' @rdname otbTexturesHaralick
#'
setMethod("otbTexturesHaralick", 
          signature(x = "RasterStack"), 
          function(x,
                   texture="all",
                   path_output=NULL,
                   return_raster=TRUE,
                   parameters.xyrad=list(c(1,1)),
                   parameters.xyoff=list(c(1,1)),
                   parameters.minmax=c(0,255),
                   parameters.nbbin=8,
                   channel=NULL,
                   verbose=FALSE,
                   ram="8192"){
            ret_textures <- otbTexturesHaralick(x = raster::brick(x),
                                                texture = texture,
                                                path_output = path_output,
                                                return_raster = return_raster,
                                                parameters.xyrad = parameters.xyrad,
                                                parameters.xyoff = parameters.xyoff,
                                                parameters.minmax = parameters.minmax,
                                                parameters.nbbin = parameters.nbbin,
                                                channel = channel,
                                                verbose = verbose,
                                                ram = ram)
            return(ret_textures)
          })

# Function using GeoTiff -------------------------------------------------------
#' OTB wrapper for Haralick's simple, advanced and higher order texture features
#'@description  OTB wrapper for calculating Haralick's simple, advanced and higher order texture features on every pixel in each channel 
#' @rdname otbTexturesHaralick
#'
setMethod("otbTexturesHaralick", 
          signature(x = "character"), 
          function(x,
                   texture="all",
                   output_name="hara",
                   path_output=NULL,
                   return_raster=FALSE,
                   parameters.xyrad=list(c(1,1)),
                   parameters.xyoff=list(c(1,1)),
                   parameters.minmax=c(0,255),
                   parameters.nbbin=8,
                   channel=NULL,
                   verbose=FALSE,
                   ram="8192"){
            otb<- link2GI::linkOTB()
            if(texture == "all"){
              texture <- c("simple", "advanced", "higher")
            }
            
            if (is.null(channel)){
              channel <- seq(length(grep(gdalUtils::gdalinfo(x,nomd = TRUE),
                                         pattern = "Band ")))
            } 
            
            ret_textures <- lapply(channel, function(band){
              ret_textures <- lapply(parameters.xyrad, function(xyrad){
                ret_textures <- lapply(parameters.xyoff, function(xyoff){
                  ret_textures <- lapply(texture, function(txt){
                    path_outfile <- paste0(path_output,
                                           "band_", band, "_",
                                           output_name, "_",
                                           txt, "_",
                                           xyrad[1], xyrad[2], "_",
                                           xyoff[1], xyoff[2],
                                           ".tif")
                    # path_outfile <- paste0(path_output,
                    #                        output_name)
                    
                    command<-paste0(path_OTB,"otbcli_HaralickTextureExtraction",
                                    " -in ", x,
                                    " -channel ", band,
                                    " -out ", path_outfile,
                                    " -ram ",ram,
                                    " -parameters.xrad ",xyrad[1]
                                    , " -parameters.yrad ",xyrad[2]
                                    , " -parameters.xoff ",xyoff[1]
                                    , " -parameters.yoff ",xyoff[2]
                                    , " -parameters.min ",parameters.minmax[1]
                                    , " -parameters.max ",parameters.minmax[2]
                                    , " -parameters.nbbin ",parameters.nbbin,
                                    " -texture ",txt)
                    if (verbose) {
                      cat("\nrunning cmd:  ", command,"\n")
                      system(command)
                    } else{
                      system(command,intern = TRUE,ignore.stdout = TRUE)
                    }
                    if (return_raster){
                      if(txt == "simple"){
                        bnames <- c("Energy", "Entropy", "Correlation", 
                                    "Inverse_Difference_Moment", "Inertia", 
                                    "Cluster_Shade", "Cluster_Prominence",
                                    "Haralick_Correlation")
                      } else if(txt == "advanced"){
                        bnames <- c("Mean", "Variance", "Dissimilarity",
                                    "Sum_Average", 
                                    "Sum_Variance", "Sum_Entropy", 
                                    "Difference_of_Variances", 
                                    "Difference_of_Entropies", 
                                    "IC1", "IC2")
                      } else if(txt == "higher"){
                        bnames <- c("Short_Run_Emphasis", 
                                    "Long_Run_Emphasis", 
                                    "Grey-Level_Nonuniformity", 
                                    "Run_Length_Nonuniformity", 
                                    "Run_Percentage", 
                                    "Low_Grey-Level_Run_Emphasis", 
                                    "High_Grey-Level_Run_Emphasis", 
                                    "Short_Run_Low_Grey-Level_Emphasis", 
                                    "Short_Run_High_Grey-Level_Emphasis", 
                                    "Long_Run_Low_Grey-Level_Emphasis",
                                    "Long_Run_High_Grey-Level_Emphasis")
                      }
                      if (verbose) print("create: ", path_outfile)
                      ret_textures <- raster::readAll(raster::stack(path_outfile))
                      names(ret_textures) <- paste0(bnames, "-", 
                                                    "b", band,
                                                    "r", xyrad[1],
                                                    "o", xyoff[1],
                                                    "m", parameters.minmax[1],
                                                    parameters.minmax[2])
                    } else {
                      ret_textures <- NULL
                    }
                    return(ret_textures)
                  })
                  if(is.null(ret_textures[[1]])){
                    return(NULL)
                  } else {
                    return(raster::stack(ret_textures))  
                  }
                })
                if(is.null(ret_textures[[1]])){
                  return(NULL)
                } else {
                  return(raster::stack(ret_textures))  
                }
              })
              if(is.null(ret_textures[[1]])){
                return(NULL)
              } else {
                return(raster::stack(ret_textures))  
              }
            })
            if(is.null(ret_textures[[1]])){
              return(NULL)
            } else {
              return(raster::stack(ret_textures))  
            }
          })



#' Calculates local statistics for a given kernel size
#' 
#' @note the otb is used for the calculation of the statistics. Please provide a GeoTiff file  
#' @param input of GeoTiff containing 1 ore more gray value bands
#' @param out string pattern vor individual naming of the output file(s)
#' @param radius computational window in pixel
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param retRaster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @author Chris Reudenbach
#' @export otbLocalStat
#' @examples 
#' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' otblocalStat(input=paste0(getwd(),"4490600_5321400.tif"),radius=5)
#' }

otbLocalStat<- function(input=NULL,
                        out="localStat",
                        ram="8192",
                        radius=3,
                        channel=NULL,
                        retRaster=FALSE,
                        outDir=NULL,
                        verbose=FALSE){
  
  
  retStack<-list()
  if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (band in channel) {
    
    outName<-paste0("band_",
                    band,
                    "_",
                    out,
                    "_",
                    radius,
                    ".tif")
    
    command<-paste0(path_OTB,"otbcli_LocalStatisticExtraction")
    command<-paste(command, " -in ", input)
    command<-paste(command, " -channel ", channel)
    command<-paste(command, " -out ", outName)
    command<-paste(command, " -ram ",ram)
    command<-paste(command, " -radius ",radius)
    if (verbose) {
      cat("\nrunning cmd:  ", command[band],"\n")
      system(command[band])}
    else{
      system(command[band],intern = TRUE,ignore.stdout = TRUE)}  
    
    if (retRaster) retStack[[band]]<-assign(paste0(tools::file_path_sans_ext(basename(outName)),"band_",band),raster::stack(outName))
  }
  return(retStack)
}


#' Calculates edges for a given kernel size
#' @description list of geotiffs containing thelocal statistics for each channel
#' @note the otb is used for filtering. please provide a GeoTiff file
#' @param input of GeoTiff containing 1 ore more gray value band(s)
#' @param out the output mono band image containing the edge features
#' @param filter the choice of edge detection method (gradient/sobel/touzi)
#' @param filter.touzi.xradius x radius of the Touzi processing neighborhood (if filter==touzi) (default value is 1 pixel)
#' @param filter.touzi.yradius y radius of the Touzi processing neighborhood (if filter==touzi) (default value is 1 pixel)
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param retRaster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @author Chris Reudenbach
#' @export otbEdge
#' @examples 
#' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' otbEdge(input=paste0(getwd(),"4490600_5321400.tif"),filter = "sobel")
#' }


otbEdge<- function(input=NULL,
                   out="edge",
                   ram="8192",
                   filter="gradient",
                   filter.touzi.xradius=1,
                   filter.touzi.yradius=1,
                   channel=NULL,
                   retRaster=FALSE,
                   outDir=NULL,
                   verbose=FALSE){
  
  
  retStack<-list()
  if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (band in channel) {
    outName<-paste0("band_",
                    band,
                    "_",
                    out,
                    "_",
                    filter,
                    ".tif")
    
    command<-paste0(path_OTB,"otbcli_EdgeExtraction")
    command<-paste(command, " -in ", input)
    command<-paste(command, " -channel ", channel)
    command<-paste(command, " -filter ", filter)
    if (filter == "touzi") {
      command<-paste(command, " -filter.touzi.xradius ", filter.touzi.xradius)
      command<-paste(command, " -filter.touzi.yradius ", filter.touzi.yradius)
    }
    command<-paste(command, " -out ", outName)
    command<-paste(command, " -ram ",ram)
    if (verbose) {
      cat("\nrunning cmd:  ", command[band],"\n")
      system(command[band])}
    else{
      system(command[band],intern = TRUE,ignore.stdout = TRUE)}          
    
    if (retRaster) retStack[[band]]<-assign(paste0(tools::file_path_sans_ext(basename(outName)),"band_",band),raster::stack(outName))
  }
  return(retStack)
}

#' Calculates Gray scale morphological operations for a given kernel size
#' 
#' @note the otb is used for filtering. please provide a GeoTiff file
#' @param input of GeoTiff containing 1 ore more gray value bands
#' @param out the output mono band image containing the edge features
#' @param filter the choice of the morphological operation (dilate/erode/opening/closing) (default value is dilate)
#' @param structype the choice of the structuring element type (ball/cross)
#' @param structype.ball.xradius x the ball structuring element X Radius (only if structype==ball)
#' @param structype.ball.yradius y the ball structuring element Y Radius (only if structype==ball)
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param retRaster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @description list of geotiffs containing thelocal statistics for each channel
#' @author Chris Reudenbach
#' @export otbGrayMorpho
#' @examples 
#' \dontrun{
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,junkpaths = TRUE,overwrite = TRUE)
#' gm<-otbGrayMorpho(input=paste0(getwd(),"4490600_5321400.tif"),retRaster = TRUE)
#' raster::plot(gm[[1]])
#' }

otbGrayMorpho<- function(input=NULL,
                         out="morpho",
                         ram="8192",
                         filter="dilate",
                         structype="ball",
                         structype.ball.xradius=5,
                         structype.ball.yradius=5,
                         channel=NULL,
                         retRaster=FALSE,
                         outDir=NULL,
                         verbose=FALSE){
  
  
  retStack<-list()
  
  if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (band in channel) {
    outName<-paste0("band_",
                    band,
                    "_",
                    out,
                    "_",
                    filter,
                    "_",
                    structype,
                    ".tif")
    
    command<-paste0(path_OTB,"otbcli_GrayScaleMorphologicalOperation")
    command<-paste(command, " -in ", input)
    command<-paste(command, " -channel ", channel)
    command<-paste(command, " -filter ", filter)
    if (structype == "ball") {
      command<-paste(command, " -structype.ball.xradius ", structype.ball.xradius)
      command<-paste(command, " -structype.ball.yradius ", structype.ball.yradius)
    }
    command<-paste(command, " -out ", outName)
    command<-paste(command, " -ram ",ram)
    if (verbose) {
      cat("\nrunning cmd:  ", command[band],"\n")
      system(command[band])}
    else{
      system(command[band],intern = TRUE,ignore.stdout = TRUE)}         
    
    if (retRaster) retStack[[band]]<-assign(paste0(tools::file_path_sans_ext(basename(outName)),"band_",band),raster::stack(outName))
  }
  return(retStack)
}
# if necessary creates additional folders for the resulting files
getOutputDir<- function (outDir){
  if (!is.null(outDir)) {
    otbOutputDir<-outDir
    if (!file.exists(paste0(otbOutputDir, "/texture/"))) dir.create(file.path(paste0(otbOutputDir, "/texture/")), recursive = TRUE,showWarnings = FALSE)
  } else {
    otbOutputDir<-paste0(getwd(),"/texture/")
    if (!file.exists(paste0(otbOutputDir, "/texture/"))) dir.create(file.path(paste0(otbOutputDir, "/texture/")), recursive = TRUE,showWarnings = FALSE)
  }
  return(otbOutputDir)
}