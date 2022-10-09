#' OTB wrapper for Haralick's simple, advanced and higher order texture features.
#'
#' @description  OTB wrapper for calculating Haralick's simple, advanced and higher order texture features on every pixel in each channel of the input image
#'
#' @param x A \code{Raster*} object or a \href{http://www.gdal.org/frmt_gtiff.html}{GeoTiff} containing one or more gray  value bands
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
#' @param path_output path outut
#' @param otbLinks        list. of OTB tools cli pathes
#' @param gdalLinks     list. GDAL tools cli paths 
#' @return raster* object
#' @references Haralick, R.M., K. Shanmugam and I. Dinstein. 1973. Textural Features for Image Classification.
#' IEEE Transactions on Systems, Man and Cybernetics. SMC-3(6):610-620.\cr
#' \href{https://www.orfeo-toolbox.org/SoftwareGuide}{Orfeo Toolbox Sofware Guide, 2016}\cr
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

#' @author Chris Reudenbach

#' @details  More information at: \href{https://prism.ucalgary.ca/handle/1880/51900}{texture tutorial}
#' Keep in mind that:\cr
#' Homogeneity is correlated with Contrast,  r = -0.80\cr
#' Homogeneity is correlated with Dissimilarity, r = -0.95\cr
#' GLCM Variance is correlated with Contrast,  r= 0.89\cr
#' GLCM Variance is correlated with Dissimilarity,  r= 0.91\cr
#' GLCM Variance is correlated with Homogeneity,  r= -0.83\cr
#' Entropy is correlated with ASM,  r= -0.87\cr
#' GLCM Mean and Correlation are more independent. For the same image, GLCM Mean shows  r< 0.1 with any of the other texture measures demonstrated in this tutorial. GLCM Correlation shows  r<0.5 with any other measure.
#' for a review of a lot of feature extraction algorithms look at: \href{https://doi.org/10.1117/1.JEI.21.2.023016}{Williams et al, 2012, J. of Electronic Imaging, 21(2), 023016 (2012)}\cr
#' glcm <-> haralick "mean" <-> "advanced 1", "variance" <-> "advanced 2", "homogeneity" <-> "simple 4", "contrast"<-> "simple 5", "dissimilarity" <-> "advanced 2", "entropy" <-> "simple 2", "second_moment"<-> "simple 4", "correlation" <-> "simple 3"
#' Furthermore using stats will cover mean and variance while dissimilarity is highly correlated to homogeneity data.

#' @export otbtex_hara
#' @examples
#' \dontrun{

#' # load libraries
#' require(link2GI)
#' require(listviewer)
#' 
#' setwd(tempdir())
#' 
#' # check if OTB exists
#' otbLinks <- link2GI::linkOTB()
#' 
#' if (otbLinks$exist) {
#' data("rgb")
#' raster::plotRGB(rgb)
#' fn<-file.path(tempdir(),"rgb.tif")
#' raster::writeRaster(rgb, 
#'                     filename=fn,
#'                     format="GTiff", 
#'                     overwrite=TRUE)
#' # get help
#' cmd<-link2GI::parseOTBFunction(algo = "HaralickTextureExtraction",gili=otbLinks)
#' listviewer::jsonedit(cmd$help)
#'                        
#' # calculate simple Haralick-textures for 3 red, green and blue channel
#' r <- otbtex_hara(x=file.path(tempdir(),"rgb.tif"), 
#'                  texture = "simple", 
#'                  return_raster = TRUE, 
#'                  otbLinks =  otbLinks)
#'
#' # visualize all layers
#' raster::plot(r)
#' }
#' }


otbtex_hara<- function(x,
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
                       otbLinks = NULL,
                       gdalLinks = NULL,
                       ram="8192"){
  
  input=x
  
  if (nchar(dirname(input))>0){
    input<basename(input)
    path_run <- dirname(input)
  }
  else path_run <- tempdir()
  
  if (is.null(otbLinks)){
    otb<-link2GI::linkOTB()
  } else otb<- otbLinks$pathOTB
  path_OTB<- otbLinks$pathOTB
  
  if (is.null(gdalLinks)){
    gdal<-link2GI::linkGDAL()
  } else gdal<- gdalLinks
  path_gdal<- gdal$path
  
  if(texture == "all"){
    texture <- c("simple", "advanced", "higher")
  }
  
  if (is.null(channel))    channel <-seq(length(grep(system(paste0(gdal$path,'gdalinfo  ',input), intern = TRUE),pattern = "Band ",value = TRUE)))
  
  
  ret_textures <- lapply(channel, function(band){
    ret_textures <- lapply(parameters.xyrad, function(xyrad){
      ret_textures <- lapply(parameters.xyoff, function(xyoff){
        ret_textures <- lapply(texture, function(txt){
          path_outfile<-paste0(tools::file_path_sans_ext(output_name),
                               "__",
                               band,
                               "_",
                               txt,
                               "_",
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
            message("\nrunning cmd:  ", command,"\n")
            system(command)
          } else{
            system(command,intern = FALSE,ignore.stdout = FALSE)
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
}



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
#' @param outDir output Directory
#' @param otbLinks        list. of GI tools cli pathes
#' @param gdalLinks     list. GDAL tools cli paths 
#' @author Chris Reudenbach
#' @return raster* object
#' @export otb_stat
#' @examples
#' \dontrun{
#' # load libraries

#' require(link2GI)
#' require(listviewer)
#' 
#' setwd(tempdir())
#' 
#' # check if OTB exists
#' otbLinks <- link2GI::linkOTB()
#' 
#' if (otbLinks$exist) {
#' data("rgb")
#' raster::plotRGB(rgb)
#' fn<-file.path(tempdir(),"rgb.tif")
#' raster::writeRaster(rgb, 
#'                     filename=fn,
#'                     format="GTiff", 
#'                     overwrite=TRUE)
#' # get help
#' cmd<-link2GI::parseOTBFunction(algo = "LocalStatisticExtraction",gili=otbLinks)
#' listviewer::jsonedit(cmd$help)
#' 
#' # calculate statistics
#' result<- otb_stat(input=fn,
#'                   radius=5,
#'                   retRaster = TRUE,
#'                   channel = 1, 
#'                   otbLinks = otbLinks)
#' # plot the results :
#' raster::plot(result[[1]])
#' }
#' }




otb_stat<- function(input=NULL,
                    out="localStat",
                    ram="8192",
                    radius=3,
                    channel=NULL,
                    retRaster=FALSE,
                    outDir=NULL,
                    verbose=FALSE,
                    otbLinks = NULL,
                    gdalLinks = NULL){
  
  if (nchar(dirname(input))>0){
    input<basename(input)
    path_run <- dirname(input)
  }
  else path_run <- tempdir()
  
  if (is.null(otbLinks)){
    otb<-link2GI::linkOTB()
  } else otb<- otbLinks
  path_OTB<- otb$pathOTB
  
  if (is.null(gdalLinks)){
    gdal<-link2GI::linkGDAL()
  } else gdal<- gdalLinks
  path_gdal<- gdal$path
  
  retStack<-list()
  if (is.null(channel)) channel <-seq(length(grep(system(paste0(gdal$path,'gdalinfo  ',input), intern = TRUE),pattern = "Band ",value = TRUE)))
  for (band in channel) {
    
    outName<-file.path(paste0(tools::file_path_sans_ext(out),
                              "_ch",
                              band,
                              "_r",
                              radius,
                              ".tif"))
    
    command<-paste0(path_OTB,"otbcli_LocalStatisticExtraction")
    command<-paste(command, " -in ", input)
    command<-paste(command, " -channel ", channel)
    command<-paste(command, " -out ", outName)
    command<-paste(command, " -ram ",ram)
    command<-paste(command, " -radius ",radius)
    if (verbose) {
      message("\nrunning cmd:  ", command[band],"\n")
      system(command[band])}
    else{
      system(command[band],intern = TRUE,ignore.stdout = TRUE)}
    
    if (retRaster) retStack[[band]]<-assign(paste0(tools::file_path_sans_ext(basename(outName)),"band_",band),raster::stack(outName))
  }
  return(retStack)
}


#' Calculates edges for a given kernel size.
#' @description Calculates edges for a given kernel size. return list of geotiffs containing thelocal statistics for each channel

#' @note the otb is used for filtering. please provide a GeoTiff file
#' @param input of GeoTiff containing 1 ore more gray value band(s)
#' @param out the output mono band image containing the edge features
#' @param filter the choice of edge detection method (gradient/sobel/touzi)
#' @param touzi_xradius x radius of the Touzi processing neighborhood (if filter==touzi) (default value is 1 pixel)
#' @param touzi_yradius y radius of the Touzi processing neighborhood (if filter==touzi) (default value is 1 pixel)
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param retRaster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @param outDir output Directory
#' @param otbLinks  list of OTB tools cli pathes
#' @param gdalLinks list of GDAL cli pathes
#' @author Chris Reudenbach
#' @return raster* object
#' @export otbtex_edge
#' @examples
#'\dontrun{
#' # required packages
#' # load libraries

#' require(link2GI)
#' require(listviewer)
#' 
#' setwd(tempdir())
#' 
#' # check if OTB exists
#' otbLinks <- link2GI::linkOTB()
#' 
#' if (otbLinks$exist) {
#' data("rgb")
#' raster::plotRGB(rgb)
#' fn<-file.path(tempdir(),"rgb.tif")
#' raster::writeRaster(rgb, 
#'                     filename=fn,
#'                     format="GTiff", 
#'                     overwrite=TRUE)
#' # get help
#' cmd<-link2GI::parseOTBFunction(algo = "EdgeExtraction",gili=otbLinks)
#' listviewer::jsonedit(cmd$help)
#'
#' # calculate Sobel edge detection
#'   r<-otbtex_edge(input=fn,
#'                  filter="sobel",
#'                  retRaster = TRUE,
#'                  otbLinks = otbLinks)
#'
#' # visualize all layers
#'   raster::plot(r[[1]])
#' }
#'}


otbtex_edge<- function(input=NULL,
                       out="edge",
                       ram="8192",
                       filter="gradient",
                       touzi_xradius=1,
                       touzi_yradius=1,
                       channel=NULL,
                       retRaster=FALSE,
                       outDir=NULL,
                       verbose=FALSE,
                       otbLinks = NULL,
                       gdalLinks = NULL){
  
  if (nchar(dirname(input))>0){
    input<basename(input)
    path_run <- dirname(input)
  }
  else path_run <- tempdir()
  
  if (is.null(otbLinks)){
    otb<-link2GI::linkOTB()
  } else otb<- otbLinks
  path_OTB<- otb$pathOTB
  
  if (is.null(gdalLinks)){
    gdal<-link2GI::linkGDAL()
  } else gdal<- gdalLinks
  path_gdal<- gdal$path 
  retStack<-list()
  if (is.null(channel)) channel <-seq(length(grep(system(paste0(gdal$path,'gdalinfo  ',input), intern = TRUE),pattern = "Band ",value = TRUE)))
  for (band in channel) {
    outName<-file.path(paste0(tools::file_path_sans_ext(out),
                              "_ch",
                              band,
                              "_f",
                              filter,
                              ".tif"))
    
    command<-paste0(path_OTB,"otbcli_EdgeExtraction")
    command<-paste(command, " -in ", input)
    command<-paste(command, " -channel ", channel)
    command<-paste(command, " -filter ", filter)
    if (filter == "touzi") {
      command<-paste(command, " -filter.touzi.xradius ", touzi_xradius)
      command<-paste(command, " -filter.touzi.yradius ", touzi_yradius)
    }
    command<-paste(command, " -out ", outName)
    command<-paste(command, " -ram ",ram)
    if (verbose) {
      message("\nrunning cmd:  ", command[band],"\n")
      system(command[band])}
    else{
      system(command[band],intern = TRUE,ignore.stdout = TRUE)}
    
    if (retRaster) retStack[[band]]<-assign(paste0(tools::file_path_sans_ext(basename(outName)),"band_",band),raster::stack(outName))
  }
  return(retStack)
}

#' Calculates Gray scale morphological operations for a given kernel size.
#' @description Calculates Gray scale morphological operations for a given kernel size. return list of geotiffs containing thelocal statistics for each channel
#' @note the otb is used for filtering. please provide a GeoTiff file
#' @param input of GeoTiff containing 1 ore more gray value bands
#' @param out the output mono band image containing the edge features
#' @param filter the choice of the morphological operation (dilate/erode/opening/closing) (default value is dilate)
#' @param structype the choice of the structuring element type (ball/cross)
#' @param xradius x the ball structuring element X Radius (only if structype==ball)
#' @param yradius y the ball structuring element Y Radius (only if structype==ball)
#' @param channel sequence of bands to be processed
#' @param ram reserved memory in MB
#' @param retRaster boolean if TRUE a raster stack is returned
#' @param verbose switch for system messages default is FALSE
#' @param outDir output Directory
#' @param otbLinks        list. of GI tools cli pathes
#' @param gdalLinks     list. GDAL tools cli paths 
#' @author Chris Reudenbach
#' @export otbtex_gray
#' @return raster* object
#' 
#' @examples
#' \dontrun{
#' # load libraries

#' require(link2GI)
#' require(listviewer)
#' 
#' setwd(tempdir())
#' 
#' # check if OTB exists
#' otbLinks <- link2GI::linkOTB()
#' 
#' if (otbLinks$exist) {
#' data("rgb")
#' raster::plotRGB(rgb)
#' fn<-file.path(tempdir(),"rgb.tif")
#' raster::writeRaster(rgb, 
#'                     filename=fn,
#'                     format="GTiff", 
#'                     overwrite=TRUE)
#' # get help
#' cmd<-link2GI::parseOTBFunction(algo = "GrayScaleMorphologicalOperation",gili=otbLinks)
#' listviewer::jsonedit(cmd$help)
#' 
#' r<-otbtex_gray(input="pacman.tif",retRaster = TRUE,otbLinks=otbLinks)
#'
#' ##- visualize all layers
#' raster::plot(r[[1]])
#' }
#' }


otbtex_gray<- function(input=NULL,
                       out="morpho",
                       ram="8192",
                       filter="dilate",
                       structype="ball",
                       xradius=5,
                       yradius=5,
                       channel=NULL,
                       retRaster=FALSE,
                       outDir=NULL,
                       verbose=FALSE,
                       otbLinks = NULL,
                       gdalLinks = NULL){
  
  if (nchar(dirname(input))>0){
    input<basename(input)
    path_run <- dirname(input)
  }
  else path_run <- tempdir()
  
  retStack<-list()
  
  
  if (is.null(otbLinks)){
    otb<-link2GI::linkOTB()
  } else otb<- otbLinks
  path_OTB<- otb$pathOTB
  
  
  if (is.null(gdalLinks)){
    gdal<-link2GI::linkGDAL()
  } else gdal<- gdalLinks
  path_gdal<- gdal$path
  
  if (is.null(channel)) channel <-seq(length(grep(system(paste0(gdal$path,'gdalinfo  ',input), intern = TRUE),pattern = "Band ",value = TRUE)))
  for (band in channel) {
    outName<-file.path(paste0(
      tools::file_path_sans_ext(out),
      "_ch",
      band,
      "_f",
      filter,
      "_",
      structype,
      ".tif"))
    
    command<-paste0(path_OTB,"otbcli_GrayScaleMorphologicalOperation")
    command<-paste(command, " -in ", input)
    command<-paste(command, " -channel ", channel)
    command<-paste(command, " -filter ", filter)
    command<-paste(command, " -xradius ", xradius)
    command<-paste(command, " -yradius ", yradius)
    command<-paste(command, " -out ", outName)
    command<-paste(command, " -ram ",ram)
    if (verbose) {
      message("\nrunning cmd:  ", command[band],"\n")
    }
    else{
      system(command[band],intern = TRUE,ignore.stdout = TRUE)}
    
    if (retRaster) retStack[[band]]<-assign(paste0(tools::file_path_sans_ext(basename(outName)),"band_",band),raster::stack(outName))
  }
  return(retStack)
}