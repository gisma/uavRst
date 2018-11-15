
#' Calls the glcm package with useful settings
#' @note for the use of glcm_texture a glcm wrapper function
#'       a raster* object is required
#' @param x rasterLayer or a rasterStack containing different channels
#' @param nrasters vector of channels to use from x. Default =nlayers(x)
#' @param kernelSize vector of numbers indicating the environment sizes for which the textures are calculated
#' @param stats string vector of parameters to be calculated.
#' @param n_grey number of grey values.
#' @param parallel logical value indicating whether parameters are calculated parallel or not
#' @param min_x for each channel the minimum value which can occur. If NULL then the minimum value from the rasterLayer is used.
#' @param max_x for each channel the maximum value which can occur. If NULL then the maximum value from the rasterLayer is used.
#' This functions calls the glcm function from with standard settings
#' and returns list of RasterStacks containing the texture parameters for each combination of channel and kernelSize
#' @param  shift =list(c(0,1), c(1,1), c(1,0),c(1,-1))
#' @author Hanna Meyer
#'
#' @note More information at:
#' \href{https://prism.ucalgary.ca/handle/1880/51900}{texture tutorial}
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
#' @seealso \href{https://CRAN.R-project.org/package=glcm }{glcm package}
#' @export glcm_texture
#' @examples
#'
#' require(glcm)
#' ## example on how to calculate texture with glcm
#' owd <- getwd()
#' setwd(tempdir())
#' data("pacman")
#' # call glcm wrapper
#' result <- glcm_texture(pacman,
#'                        nrasters=1:3,
#'                        stats=c("mean", "variance", "homogeneity"),
#'                        parallel = FALSE)
#'
#' #plot the result:
#' raster::plot(result[[1]])
#' setwd(owd)




glcm_texture <- function(x,
                             nrasters=1:raster::nlayers(x),
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
        x[[i]]=raster::reclassify(x[[i]], c(max_x[i],Inf,max_x[i]))
        x[[i]]=raster::reclassify(x[[i]], c(-Inf,min_x[i],min_x[i]))
      }
    } else { # only one raster
      x=raster::reclassify(x, c(max_x,Inf,max_x))
      x=raster::reclassify(x, c(-Inf,min_x,min_x))
    }
  }


  glcm_filter<-list()
  for (j in 1:length(kernelSize)){
    if ((class (x)=="RasterStack")||(class (x)=="RasterBrick")){
      if (parallel){
        glcm_filter[[j]]<-foreach::foreach(i=nrasters,
                                           .packages= c("glcm","raster"))%dopar%{
                                             glcm(x[[i]],
                                                  window = c(kernelSize[j], kernelSize[j]),
                                                  shift=shift,
                                                  statistics=stats,n_grey=n_grey,
                                                  min_x=min_x[i],max_x=max_x[i],
                                                  na_opt="center")
                                           }
      } else {
        glcm_filter[[j]]<-foreach::foreach(i=nrasters,
                                           .packages= c("glcm","raster"))%do%{
                                             raster::mask(glcm(x[[i]],
                                                               window = c(kernelSize[j], kernelSize[j]),
                                                               shift=shift,
                                                               statistics=stats,n_grey=n_grey,
                                                               min_x=min_x[i],max_x=max_x[i],
                                                               na_opt="center"), x[[i]])
                                           }
      }
      names(glcm_filter[[j]])<-names(x)[nrasters]
    } else {
      glcm_filter[[j]]<-raster::mask(glcm(x, window = c(kernelSize[j], kernelSize[j]),
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
#' @param giLinks        list. of GI tools cli pathes
#'
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

#' require(uavRst)
#' require(link2GI)
#' ## -check if OTB is installed correctly
#' giLinks <- uavRst::linkAll()
#' if (giLinks$otb$exist) {
#' setwd(tempdir())
#' ##- get some typical data as provided by the authority
#' tmp<-Sys.setlocale('LC_ALL','C')
#' utils::download.file(url="http://www.ldbv.bayern.de/file/zip/5619/DOP%2040_CIR.zip",
#'                      destfile="testdata.zip")
#' unzip("testdata.zip",junkpaths = TRUE,overwrite = TRUE)
#'
#' # calculate simple Haralick-textures
#' r<- otbtex_hara(x="4490600_5321400.tif",texture = "simple",return_raster = TRUE)
#'
#' #plot the results :
#' ##- visualize all layers
#' raster::plot(r)
#' tmp<-Sys.setlocale(category = "LC_ALL", locale = "de_DE.-8")
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
                   giLinks = NULL,
                   ram="8192"){



  if (is.null(giLinks)){
    giLinks <- linkAll()
  }
  path_OTB <- giLinks$otb$pathOTB
  otb<- giLinks$otb
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
                      cat("\nrunning cmd:  ", command,"\n")
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
#' @param giLinks        list. of GI tools cli pathes
#' @author Chris Reudenbach

#' @export otb_stat
#' @examples
#' \dontrun{
#' require(uavRst)
#' # check if OTB is installed correctly
#' giLinks <- uavRst::linkAll()
#' if (giLinks$otb$exist) {
#' setwd(tempdir())
#' data("pacman")
#' raster::writeRaster(pacman,"pacman.tif",overwrite=TRUE)
#' 
#' # calculate statistics
#' result<- otb_stat(input="pacman.tif",radius=5,retRaster = TRUE)
#' #plot the results :
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
                        giLinks = NULL){

  if (is.null(giLinks)){
    giLinks <- linkAll()
  }
  path_OTB <- giLinks$otb$pathOTB

  retStack<-list()
  if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (band in channel) {

    outName<-paste0(tools::file_path_sans_ext(out),
                    "__",
                    band,
                    "_r",
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


#' Calculates edges for a given kernel size.
#' @description Calculates edges for a given kernel size. return list of geotiffs containing thelocal statistics for each channel

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
#' @param outDir output Directory
#' @param giLinks        list. of GI tools cli pathes
#' @author Chris Reudenbach
#' @export otbtex_edge
#' @examples
#'\dontrun{
#' ##- required packages
#' require(uavRst)
#' require(link2GI)
#' setwd(tempdir())
#' 
#' ## check if OTB exists
#' giLinks <- list()
#' giLinks$otb <- link2GI::linkOTB()
#' 
#' if (giLinks$otb$exist) {
#' data("pacman")
#' pacman<-raster::disaggregate(pacman,10)
#' raster::writeRaster(pacman,"pacman.tif",overwrite=TRUE)
#'
#' ##- calculate Sobel edge detection
#' r<-otbtex_gray(input="pacman.tif",
#'                filter = "erode",
#'                structype = "ball", 
#'                structype.ball.xradius = 3,
#'                structype.ball.yradius = 3 ,
#'                retRaster = TRUE)
#'
#' ##- visualize all layers
#' raster::plot(r[[1]])
#' }
#' }


otbtex_edge<- function(input=NULL,
                   out="edge",
                   ram="8192",
                   filter="gradient",
                   filter.touzi.xradius=1,
                   filter.touzi.yradius=1,
                   channel=NULL,
                   retRaster=FALSE,
                   outDir=NULL,
                   verbose=FALSE,
                   giLinks = NULL){

    if (is.null(giLinks)){
      giLinks <- linkAll()
    }
  path_OTB <- giLinks$otb$pathOTB
  retStack<-list()
  if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (band in channel) {
    outName<-paste0(tools::file_path_sans_ext(out),
                    "__",
                    band,
                    "_f",
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

#' Calculates Gray scale morphological operations for a given kernel size.
#' @description Calculates Gray scale morphological operations for a given kernel size. return list of geotiffs containing thelocal statistics for each channel
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
#' @param outDir output Directory
#' @param giLinks        list. of GI tools cli pathes
#' @author Chris Reudenbach
#' @export otbtex_gray

#' @examples
#' \dontrun{
#' require(uavRst)
#' require(link2GI)
#' setwd(tempdir())
#' ## check if OTB exists
#' giLinks<-list()
#' giLinks$otb <- link2GI::linkOTB()
#' 
#' if (giLinks$otb$exist) {
#' data("pacman")
#' raster::writeRaster(pacman,"pacman.tif",overwrite=TRUE)
#' r<-otbtex_gray(input="pacman.tif",retRaster = TRUE)
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
                         structype.ball.xradius=5,
                         structype.ball.yradius=5,
                         channel=NULL,
                         retRaster=FALSE,
                         outDir=NULL,
                         verbose=FALSE,
                         giLinks = NULL){

  if (is.null(giLinks)){
    giLinks <- linkAll()
  }
  path_OTB <- giLinks$otb$pathOTB

  retStack<-list()

  if (is.null(channel)) channel<-seq(length(grep(gdalUtils::gdalinfo(input,nomd = TRUE),pattern = "Band ")))
  for (band in channel) {
    outName<-paste0(tools::file_path_sans_ext(out),
                    "__",
                    band,
                    "_f",
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
#' calculates most important DEM parameters
#'
#' @note please provide a GeoTiff file
#' @param dem  character filname to GeoTiff containing one channel DEM
#' @param item character list containing the keywords of the DEM parameter to be calculated. Default parameter are c("hillshade", "slope", "aspect", "TRI", "TPI", "Roughness", "SLOPE", "ASPECT", "C_GENE", "C_PROF", "C_PLAN", " C_TANG"," C_LONG", "C_CROS", "C_MINI", "C_MAXI", "C_TOTA", "C_ROTO", "MTPI")
#' @param verbose logical. be quiet
#' @param morphoMethod  numeric. saga morphometric method  see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_0.html}{SAGA GIS Help}. GDAL parameters see also: \href{https://www.gdal.org/gdaldem.html}{gdaldem}
#' @param minScale  numeric. in scale for multi scale TPI see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_28.html}{SAGA GIS Help}
#' @param maxScale  numeric. max scale for multi scale TPI see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_28.html}{SAGA GIS Help}
#' @param numScale  numeric. number of scale for multi scale TPI see also: \href{http://www.saga-gis.org/saga_tool_doc/6.2.0/ta_morphometry_28.html}{SAGA GIS Help}
#' @param retRaster boolean if TRUE a raster stack is returned
#' @param giLinks    list. of GI tools cli pathes

#' @export morpho_dem
#' @examples
#'\dontrun{
#' ##- required packages
#' require(uavRst)
#' require(link2GI)
#' setwd(tempdir())
#' giLinks<-list()
#' ## check if OTB exists
#' giLinks$otb <- link2GI::linkOTB()
#' giLinks$saga <- link2GI::linkSAGA()
#' giLinks$grass <- link2GI::linkGRASS7(returnPaths = TRUE)
#' if (giLinks$otb$exist & giLinks$saga$exist & giLinks$grass$exist) {
#' data("mrbiko")
#' proj = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
#' mrbiko <- raster::projectRaster(mrbiko, crs = proj,method = "ngb",res = 20)
#' raster::writeRaster(mrbiko,"dem.tif",overwrite=TRUE)
#' r<-morpho_dem(dem="dem.tif",c("hillshade", "slope", "aspect", "TRI", "TPI",
#'                               "Roughness", "SLOPE", "ASPECT",  "C_GENE", "C_PROF",
#'                               "C_PLAN", " C_TANG"," C_LONG", "C_CROS"),
#'                               giLinks= giLinks)
#' r_st=raster::stack(r)
#' names(r_st)=names(r)
#' raster::plot(r_st)
#' }
#' }


morpho_dem<- function(dem,
                    item=c("hillshade","slope", "aspect","TRI","TPI","Roughness","SLOPE","ASPECT", "C_GENE","C_PROF","C_PLAN"," C_TANG"," C_LONG","C_CROS","C_MINI","C_MAXI","C_TOTA","C_ROTO","MTPI"),
                    verbose=FALSE,
                    morphoMethod = 6,
                    minScale = 1,
                    maxScale = 8,
                    numScale = 2,
                    retRaster = TRUE,
                    giLinks = NULL) {
  if (!exists("path_run")) path_run = paste0(getwd(),"/")
  retStack<-list()
  if (is.null(giLinks)){
    giLinks <- linkAll()
  }
  issaga <- Vectorize(issagaitem)
  isgdal <- Vectorize(isgdaldemitem)
  saga_items<-item[issaga(item)]
  gdal_items<-item[isgdal(item)]

  gdal <- giLinks$gdal
  saga <- giLinks$saga
  sagaCmd<-saga$sagaCmd
  invisible(env<-RSAGA::rsaga.env(path = saga$sagaPath))

  s<-raster::raster(dem)
  y<-raster::yres(s)
  x<-raster::xres(s)
  res<-gdalUtils::gdalwarp(dem,paste0(path_run,'dem2.tif'),
                           te=paste(extent(s)[1],' ',
                                    extent(s)[3],' ',
                                    extent(s)[2],' ',
                                    extent(s)[4]),
                           tr=paste(x,' ', y),
                           overwrite = TRUE,
                           multi = TRUE)

  for (item in gdal_items){
    cat(getCrayon()[[1]](":::: processing ",item,"\n"))
    res<-   gdalUtils::gdaldem(mode = item,
            input_dem=paste0(path_run,"dem2.tif"),
            output = paste0(path_run,item,".tif"))
    if (retRaster) retStack[[item]]<-assign(item,raster::stack(paste0(path_run,item,".tif")))
  }

  if (length(saga_items>0))  {
    rdem<-raster::raster(paste0(path_run,'dem2.tif'))
    raster::writeRaster(rdem,paste0(path_run,"SAGA_dem.sdat"),overwrite = TRUE,NAflag = 0)
    # claculate the basics SAGA morphometric params
    cat(getCrayon()[[1]](":::: processing ",saga_items,"\n"))
    if (length(saga_items>0) )  { #&& !("MTPI" %in% saga_items)
    RSAGA::rsaga.geoprocessor(lib = "ta_morphometry", module = 0,
                       param = list(ELEVATION = paste(path_run,"SAGA_dem.sgrd", sep = ""),
                                    UNIT_SLOPE = 1,
                                    UNIT_ASPECT = 1,
                                    SLOPE = paste(path_run,"SLOPE.sgrd", sep = ""),
                                    ASPECT = paste(path_run,"ASPECT.sgrd", sep = ""),
                                    C_GENE = paste(path_run,"C_GENE.sgrd", sep = ""),
                                    C_PROF = paste(path_run,"C_PROF.sgrd", sep = ""),
                                    C_PLAN = paste(path_run,"C_PLAN.sgrd", sep = ""),
                                    C_TANG = paste(path_run,"C_TANG.sgrd", sep = ""),
                                    C_LONG = paste(path_run,"C_LONG.sgrd", sep = ""),
                                    C_CROS = paste(path_run,"C_CROS.sgrd", sep = ""),
                                    C_MINI = paste(path_run,"C_MINI.sgrd", sep = ""),
                                    C_MAXI = paste(path_run,"C_MAXI.sgrd", sep = ""),
                                    C_TOTA = paste(path_run,"C_TOTA.sgrd", sep = ""),
                                    C_ROTO = paste(path_run,"C_ROTO.sgrd", sep = ""),
                                    METHOD = morphoMethod),
                       show.output.on.console = FALSE, invisible = TRUE,
                       env = env)}
    if ("MTPI" %in% saga_items){
      if (RSAGA::rsaga.get.version(env = env) >= "3.0.0") {

        # calculate multiscale p
        # Topographic Position Index (TPI) calculation as proposed by Guisan et al. (1999).SAGA
        # This implementation calculates the TPI for different scales and integrates these into
        # one single grid. The hierarchical integration is achieved by starting with the
        # standardized TPI values of the largest scale, then adding standardized values from smaller
        # scales where the (absolute) values from the smaller scale exceed those from the larger scale.
        RSAGA::rsaga.geoprocessor(lib = "ta_morphometry", module = 28,
                           param = list(DEM = paste(path_run,"SAGA_dem.sgrd", sep = ""),
                                        SCALE_MIN = minScale,
                                        SCALE_MAX = maxScale,
                                        SCALE_NUM = numScale,
                                        TPI = paste(path_run,"MTPI.sgrd", sep = "")),
                           show.output.on.console = FALSE,invisible = TRUE,
                           env = env)
        
      } else {cat(getCrayon()[[2]]("\nPlease install SAGA >= 3.0.0\n Run without MTPI...\n"))
        saga_items<-saga_items[  !(saga_items %in% "MTPI")]

      }
    }
    for (item in saga_items){
      cat(getCrayon()[[1]](":::: converting ",item,"\n"))
      ritem<-raster::raster(paste(path_run,item,".sdat", sep = ""))
      raster::writeRaster(ritem,paste(path_run,item,".tif", sep = ""), overwrite = TRUE, NAflag = 0)
      if (retRaster) retStack[[item]]<-assign(item,raster::stack(paste(path_run,item,".tif", sep = "")))
    }
    
    
  }
  if (retRaster) return(retStack)
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

#' RGB indices
#'
#' @description
#' This function calculates various spectral indices from a RGB. It returns at least red green and blue as splitted channels in a stack. Additionally you can choose RGB indices.
#' \code{Raster*} object.
#'
#' @param red   a single \code{Raster} or \code{RasterBrick} band.
#' @param green a single \code{Raster} or \code{RasterBrick} band.
#' @param blue  a single \code{Raster} or \code{RasterBrick} band.
#' @param rgbi the implemented RGB indices currently
#' \itemize{
#'\item \code{BI}    sqrt((R**2+G**2+B*2)/3 Brightness Index\cr
#'\item \code{SCI}   (R-G)/(R+G) Soil Colour Index\cr
#'\item \code{GLI}   (2*g - r - b)/(2*g + r + b) Green leaf index Vis Louhaichi et al. (2001)\cr
#'\item \code{HI}    (2*R-G-B)/(G-B) Primary colours Hue Index\cr
#'\item \code{NDTI}  (R-G)/(R+G) Normalized difference turbidity index Water\cr
#'\item \code{NGRDI} (G-R)/(G+R) Normalized green red difference index (sometimes GRVI) Tucker (1979)
#'\item \code{RI}    R**2/(B*G**3) Redness Index\cr
#'\item \code{SI}    (R-B)/(R+B) Spectral Slope Saturation Index\cr
#'\item \code{TGI}   -0.5[190(R670-R550)-120(R670 - R480)] The triangular greenness index (TGI) estimates chlorophyll concentration in leaves and canopies\cr
#'\item \code{VARI}  (green-red)/(green+red-blue). A Visible Atmospherically Resistant Index (VARI)\cr
#'\item \code{VVI}   (1-(r-30)/(r+30))*(1-(g-50)/(g+50))*(1-(b-1)/(b+1))\cr
#'\item \code{GLAI}  (25 * (green - red) / (green +  red -  blue ) + 1.25 )\cr
#'\item \code{GRVI}  (green-red)/(green+red)  Green-Red Vegetation Index
#'\item \code{CI}    (red - blue) / red Coloration Index
#'\item \code{HUE}   atan(2 * (red - green - blue) / 30.5 * (green - blue)) Overall Hue Index
#'\item \code{SAT}   (max(red,green,blue) - min(red,green,blue)) / max(red,green,blue) Overall Saturation Index
#'\item \code{SHP} 	 2 * (red - green - blue) / (green - blue) Shape index
#' }
#'
#' @export rgb_indices
#'
#' @references
#'
#' Planetary Habitability Laboratory (2015): Visible Vegetation Index (VVI). Available online via \href{http://phl.upr.edu/projects/visible-vegetation-index-vvi}{VVI}.\cr
#' Lacaux, J. P., Tourre, Y. M., Vignolles, C., Ndione, J. A., and Lafaye, M.: Classification of ponds from high-spatial resolution remote sensing: Application to Rift Valley Fever epidemics in Senegal, Remote Sens. Environ., 106, 66-74, 2007.(NDTI) )\cr
#' Gitelson, A., et al.: Vegetation and Soil Lines in Visible Spectral Space: A Concept and Technique for Remote Estimation of Vegetation Fraction.  International Journal of Remote Sensing 23 (2002): 2537-2562. (VARI)\cr
#' MADEIRA, J., BEDIDI, A., CERVELLE, B., POUGET, M. and FLAY, N., 1997, Visible spectrometric indices of hematite (Hm) and goethite (Gt) content in lateritic soils: 5490 N. Levin et al. the application of a Thematic Mapper (TM) image for soil-mapping in Brasilia, Brazil. International Journal of Remote Sensing, 18, pp. 2835-2852.\cr
#' MATHIEU, R., POUGET, M., CERVELLE, B. and ESCADAFAL, R., 1998, Relationships between satellite-based radiometric indices simulated using laboratory reflectance data and typic soil colour of an arid environment. Remote Sensing of Environment, 66, pp. 17-28. \cr
#' Louhaichi, M., Borman, M.M., Johnson, D.E., 2001. Spatially located platform and aerial photography for documentation of grazing impacts on wheat. Geocarto International 16, 65-70.\cr
#' Tucker, C.J., 1979. Red and photographic infrared linear combinations for monitoring vegetation. Remote Sensing of Environment 8, 127-150.\cr
#' GRVI  Green-Red Vegetation Index  Remote Sensing 2010, 2, 2369-2387; doi:10.3390/rs2102369\cr
#' CI  \href{https://www.indexdatabase.de/search/?s=color}{IDB Coloration}\cr
#' HUE Index \href{https://www.indexdatabase.de/search/?s=HUE}{IDB Hue}\cr
#' Saturation Index \href{https://www.indexdatabase.de/db/i-single.php?id=77}{IDB Saturation}\cr
#' Shape Index \href{https://www.indexdatabase.de/search/?s=shape}{IDB Shape}\cr
#'
#' @seealso
#' For a comprehensive overview of remote sensing indices have a look at: \href{http://www.indexdatabase.de/db/i.php}{A database for remote sensing indices}\cr
#' Approximatly wavelength ranges for overlapping digital camera bands are:
#' \itemize{
#' \item \code{red} 580-670 nm,
#' \item \code{green} 480-610 nm,
#' \item \code{blue} 400-520 nm
#'}
#' \href{http://digitalcommons.unl.edu/cgi/viewcontent.cgi?article=2161&context=usdaarsfacpub}{Hunt et al., 2005}. However check the manual of your camera.
#'
#' @examples
#' ## ## ##

#'##- setup environment
#'require(uavRst)

#'data(rgb)
#'
#'##- visualize the image
#'raster::plotRGB(rgb)
#'
#'##- calculate the indices
#'rgbI<-rgb_indices(red   = rgb[[1]],
#'                  green = rgb[[2]],
#'                  blue  = rgb[[3]],
#'                  rgbi = c("NDTI","VARI","TGI"))
#'
#'##- visualize the indices
#'raster::plot(rgbI)
#'##+

rgb_indices <- function(red,green,blue,rgbi=c("VVI","VARI","NDTI","RI","SCI","BI",
                                              "SI","HI",
                                              "TGI","GLI",
                                              "NGRDI","GRVI",
                                              "GLAI","HUE",
                                              "CI","SAT","SHP")) {

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

    } else if (item == "SCI") {
      # SCI Soil Colour Index
      cat("\n      calculate Soil Colour Index (SCI)")
      SCI <- (red - green) / (red + green)
      names(SCI) <- "SCI"
      return(SCI)

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

    }  else if (item=="GLAI"){
      # NGRDI Normalized green red difference index
      cat("\n      calculate greenish Leaf Area Index  (GLAI) (highly experimental)")
      # vevi<-(green - red) / (green +  red -  blue )
      GLAI = (25 * ((green - red) / (green +  red -  blue )) + 1.25 )
      names(GLAI) <- "GLAI"
      return(GLAI)

    }  else if (item=="GRVI"){
      # GRVI  Green-Red Vegetation Index  Remote Sensing 2010, 2, 2369-2387; doi:10.3390/rs2102369
      cat("\n      calculate  Green-Red Vegetation Index   (GRVI)")
      GRVI<-(green-red)/(green+red)
      names(GRVI) <- "GRVI"
      return(GRVI)

    } else if (item == "CI") {
      # CI  https://www.indexdatabase.de/search/?s=color
      cat("\n      calculate Coloration Index (CI)")
      CI <- (red - blue) / red
      names(CI) <- "CI"
      return(CI)

    } else if (item == "HUE") {
      # HUE Index https://www.indexdatabase.de/search/?s=HUE
      cat("\n      calculate Hue Index (HUE)")
      HUE <- 	 atan(2 * (red - green - blue) / 30.5 * (green - blue))
      names(HUE) <- "HUE"
      return(HUE)

    }  else if (item == "SAT") {
      # Saturation Index https://www.indexdatabase.de/db/i-single.php?id=77
      cat("\n      calculate Saturation Index (SAT)")
      SAT <- 	 (max(red,green,blue) - min(red,green,blue)) / max(red,green,blue)
      names(SAT) <- "SAT"
      return(SAT)

    } else if (item == "SHP") {
      # Shape Index https://www.indexdatabase.de/search/?s=shape
      cat("\n      calculate Shape Index (SHP)")
      SHP <- 	 2 * (red - green - blue) / (green - blue)
      names(SHP) <- "SHP"
      return(SHP)

    }
  })
  return(raster::stack(indices))
}



