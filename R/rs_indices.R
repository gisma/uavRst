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
#'\item \code{HI}    (2*R-G-B)/(G-B) Primary colours Hue Index MATHIEU et al. (1998)\cr
#'\item \code{NDTI}  (R-G)/(R+G) Normalized difference turbidity index Water Lacaux et al. (2007)\cr
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
#' @return raster* object
#' 
#' @references
#'
#' Planetary Habitability Laboratory (2015): Visible Vegetation Index (VVI). Available online via \href{http://phl.upr.edu/projects/visible-vegetation-index-vvi}{VVI}.\cr
#' Lacaux, J. P., Tourre, Y. M., Vignolles, C., Ndione, J. A., and Lafaye, M.: Classification of ponds from high-spatial resolution remote sensing: Application to Rift Valley Fever epidemics in Senegal, Remote Sens. Environ., 106, 66-74, 2007.(NDTI) )\cr
#' Gitelson, A., et al.: Vegetation and Soil Lines in Visible Spectral Space: A Concept and Technique for Remote Estimation of Vegetation Fraction.  International Journal of Remote Sensing 23 (2002): 2537-2562. (VARI)\cr
#' MADEIRA, J., BEDIDI, A., CERVELLE, B., POUGET, M. and FLAY, N., 1997, Visible spectrometric indices of hematite (Hm) and goethite (Gt) content in lateritic soils: 5490 N. Levin et al. the application of a Thematic Mapper (TM) image for soil-mapping in Brasilia, Brazil. International Journal of Remote Sensing, 18, pp. 2835-2852.(RI)\cr
#' MATHIEU, R., POUGET, M., CERVELLE, B. and ESCADAFAL, R., 1998, Relationships between satellite-based radiometric indices simulated using laboratory reflectance data and typic soil colour of an arid environment. Remote Sensing of Environment, 66, pp. 17-28.(BI, SI, HI) \cr
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
      message(getCrayon()[[3]](":::: Visible Vegetation Index  ",item,"\n"))
      #message("\n      calculate Visible Vegetation Index (VVI)")
      VVI <- (1 - abs((red - 30) / (red + 30))) *
        (1 - abs((green - 50) / (green + 50))) *
        (1 - abs((blue - 1) / (blue + 1)))
      names(VVI) <- "VVI"
      return(VVI)
      
    } else if (item == "VARI") {
      # calculate Visible Atmospherically Resistant Index (VARI)
      message(getCrayon()[[3]](":::: Visible Atmospherically Resistant Index ",item,"\n"))
      #message("\n      calculate Visible Atmospherically Resistant Index (VARI)")
      VARI <- (green - red) / (green + red - blue)
      names(VARI) <- "VARI"
      return(VARI)
      
    } else if (item == "NDTI") {
      ## Normalized difference turbidity index
      message(getCrayon()[[3]](":::: Normalized Difference Turbidity Index ",item,"\n"))
      #message("\n      calculate Normalized difference turbidity index (NDTI)")
      NDTI <- (red - green) / (red + green)
      names(NDTI) <- "NDTI"
      return(NDTI)
      GLAI
    } else if (item == "RI") {
      # redness index
      message(getCrayon()[[3]](":::: Redness Index ",item,"\n"))
      #message("\n      calculate redness index (RI)")
      RI <- red**2 / (blue*green**3)
      names(RI) <- "RI"
      return(RI)
      
    } else if (item == "SCI") {
      # SCI Soil Colour Index
      message(getCrayon()[[3]](":::: Soil Colour Index ",item,"\n"))
      #message("\n      calculate Soil Colour Index (SCI)")
      SCI <- (red - green) / (red + green)
      names(SCI) <- "SCI"
      return(SCI)
      
    } else if (item == "BI") {
      #  Brightness Index
      message(getCrayon()[[3]](":::: Brightness Index ",item,"\n"))
      #message("\n      calculate Brightness Index (BI)")
      BI <- sqrt((red**2 + green**2 + blue*2) / 3)
      names(BI) <- "BI"
      return(BI)
      
    } else if (item == "SI") {
      # SI Spectra Slope Saturation Index
      message(getCrayon()[[3]](":::: Spectra Slope Saturation Index ",item,"\n"))
      #message("\n      calculate Spectra Slope Saturation Index (SI)")
      SI <- (red - blue) / (red + blue)
      names(SI) <- "SI"
      return(SI)
      
    } else if (item=="HI"){
      # HI Primary colours Hue Index
      message(getCrayon()[[3]](":::: Primary Colours Hue Index ",item,"\n"))
      #message("\n      calculate Primary colours Hue Index (HI)")
      HI<-(2*red-green-blue)/(green-blue)
      names(HI) <- "HI"
      return(HI)
      
    } else if (item=="TGI"){
      # Triangular greenness index
      message(getCrayon()[[3]](":::: Triangular Greenness Index ",item,"\n"))
      #message("\n      calculate Triangular greenness index (TGI)")
      TGI <- -0.5*(190*(red - green)- 120*(red - blue))
      names(TGI) <- "TGI"
      return(TGI)
      
    } else if (item=="GLI"){
      #message("\n      calculate Green leaf index (GLI)")
      message(getCrayon()[[3]](":::: Green Leaf Index ",item,"\n"))
      # Green leaf index
      GLI<-(2*green-red-blue)/(2*green+red+blue)
      names(GLI) <- "GLI"
      return(GLI)
      
    } else if (item=="NGRDI"){
      # NGRDI Normalized green red difference index
      message(getCrayon()[[3]](":::: Normalized Green-Red Difference Index ",item,"\n"))
      #message("\n      calculate Normalized green red difference index  (NGRDI)")
      NGRDI<-(green-red)/(green+red)
      names(NGRDI) <- "NGRDI"
      return(NGRDI)
      
    }  else if (item=="GLAI"){
      # NGRDI Normalized green red difference index
      message(getCrayon()[[3]](":::: Greenish Leaf Area Index ",item,"\n"))
      #message("\n      calculate greenish Leaf Area Index  (GLAI) (highly experimental)")
      # vevi<-(green - red) / (green +  red -  blue )
      GLAI = (25 * ((green - red) / (green +  red -  blue )) + 1.25 )
      names(GLAI) <- "GLAI"
      return(GLAI)
      
    }  else if (item=="GRVI"){
      # GRVI  Green-Red Vegetation Index  Remote Sensing 2010, 2, 2369-2387; doi:10.3390/rs2102369
      message(getCrayon()[[3]](":::: Green-Red Vegetation Index ",item,"\n"))
      #message("\n      calculate  Green-Red Vegetation Index   (GRVI)")
      GRVI<-(green-red)/(green+red)
      names(GRVI) <- "GRVI"
      return(GRVI)
      
    } else if (item == "CI") {
      # CI  https://www.indexdatabase.de/search/?s=color
      message(getCrayon()[[3]](":::: Coloration Index ",item,"\n"))
      # message("\n      calculate Coloration Index (CI)")
      CI <- (red - blue) / red
      names(CI) <- "CI"
      return(CI)
      
    } else if (item == "HUE") {
      # HUE Index https://www.indexdatabase.de/search/?s=HUE
      message(getCrayon()[[3]](":::: Hue Index ",item,"\n"))
      #message("\n      calculate Hue Index (HUE)")
      HUE <- 	 atan(2 * (red - green - blue) / 30.5 * (green - blue))
      names(HUE) <- "HUE"
      return(HUE)
      
    }  else if (item == "SAT") {
      # Saturation Index https://www.indexdatabase.de/db/i-single.php?id=77
      message(getCrayon()[[3]](":::: Saturation Index ",item,"\n"))
      #message("\n      calculate Saturation Index (SAT)")
      SAT <- 	 (max(red,green,blue) - min(red,green,blue)) / max(red,green,blue)
      names(SAT) <- "SAT"
      return(SAT)
      
    } else if (item == "SHP") {
      # Shape Index https://www.indexdatabase.de/search/?s=shape
      message(getCrayon()[[3]](":::: Shape Index ",item,"\n"))
      #message("\n      calculate Shape Index (SHP)")
      SHP <- 	 2 * (red - green - blue) / (green - blue)
      names(SHP) <- "SHP"
      return(SHP)
      
    }
  })
  return(raster::stack(indices))
}

