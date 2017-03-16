if ( !isGeneric("otbTexturesHaralick") ) {
  setGeneric("otbTexturesHaralick", function(x, ...)
    standardGeneric("otbTexturesHaralick"))
}

#' OTB wrapper for Haralick's simple, advanced and higher order texture features
#'@description  OTB wrapper for calculating Haralick's simple, advanced and higher order texture features on every pixel in each channel of the input image.
#' @param x A \code{\link{Raster*}} object or a \code{\href{http://www.gdal.org/frmt_gtiff.html}{GeoTiff}} containing one or more gray  value bands
#' @param output_name string pattern vor individual naming of the output file(s)
#' @param parameters.xyrad list with the x and y radius in pixel indicating the kernel sizes for which the textures are calculated
#' @param parameters.xyoff  vector containg the directional offsets. Valid combinations are: list(c(1,1),c(1,0),c(0,1),c(1,-1))
#' @param n_grey Number of grey values. 
#' @param parallel A logical value indicating whether parameters are calculated parallely or not
#' @param parameters.minmax   minimum/maximum gray value which can occur. 
#' @param parameters.nbbin number of gray level bins (classes)
#' @param texture type of filter "all" for all, alternative one of "simple" "advanced" "higher"
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param return_raster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @return A list of RasterStacks containing the texture parameters for each 
#' combination of channel and filter  
#' @references Haralick, R.M., K. Shanmugam and I. Dinstein. 1973. Textural Features for Image Classification.
#' IEEE Transactions on Systems, Man and Cybernetics. SMC-3(6):610-620.\cr
#' \href{https://www.orfeo-toolbox.org/packages/OTBSoftwareGuide.pdf}{Orfeo Toolbox Sofware Guide, 2016}
#' @details 
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
#' Keep further in mind that this texture features are highly correlated:\cr
#' Homogeneity  with Contrast,  r = -0.80\cr
#' Homogeneity  with Dissimilarity, r = -0.95\cr
#' GLCM Variance  with Contrast,  r= 0.89\cr
#' GLCM Variance with Dissimilarity,  r= 0.91\cr
#' GLCM Variance  with Homogeneity,  r= -0.83\cr
#' Entropy  with ASM,  r= -0.87\cr
#' GLCM Mean and Correlation are more independent. For the same image:\cr
#' GLCM Mean shows  r< 0.1 with any of the other texture measures.\cr
#' GLCM Correlation shows  r<0.5 with any other measure.
#' 
#' @name otbTexturesHaralick
#' @export otbTexturesHaralick
#' @examples 
#' \dontrun{
#' http://www.ldbv.bayern.de/file/zip/10430/DGM_1_ascii.zip
#' # get some typical authority generated data
#' url<-"http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip"
#' res <- curl::curl_download(url, "testdata.zip")
#' unzip(res,files = grep(".tif", unzip(res,list = TRUE)$Name,value = TRUE),junkpaths = TRUE,overwrite = TRUE)
#' 
#' # first initialisation of the OTB environment
#' link2GI::linkOTB()
#' 
#' # calculate all Haralick-textures
#' otbTexturesHaralick(x=file.path(getwd(),basename(grep(".tif", unzip(res,list = TRUE)$Name,value = TRUE))))
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
                   verbose=FALSE,
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
                                   pattern = glob2rx(paste0("*", tempout, "*")),
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