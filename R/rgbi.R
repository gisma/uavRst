if (!isGeneric('rgbi')) {
  setGeneric('rgbi', function(x, ...)
    standardGeneric('rgbi'))
}
#' RGB indices 
#' 
#' @description
#' This function calculates various spectral indices from a RGB 
#' \code{Raster*} object.  
#' 
#' @param rgb A \code{RasterStack} or \code{RasterBrick} object. 3
#' bands are mandatory (usually red, green and blue).

#' @return 
#' A \code{RasterLayer} with the index calculates as:\cr
#' VARI (green-red)/(green+red-blue). A Visible Atmospherically Resistant Index (VARI)\cr
#' BI  sqrt((R**2+G**2+B*2)/3 Brightness Index\cr
#' SI (R-B)/(R+B) Spectra Slope Saturation Index\cr
#' HI (2*R-G-B)/(G-B) Primary colours Hue Index\cr
#' CI (R-G)/(R+G) Soil Colour Index\cr
#' RI R**2/(B*G**3) Redness Index\cr
#' NDTI (R-G)/(R+G) Normalized difference turbidity index Water\cr
#' NGRDI (G-R)/(G+R) Normalized green red difference index (sometimes GRVI) Tucker (1979)
#' VVI  (1-(r-30)/(r+30))*(1-(g-50)/(g+50))*(1-(b-1)/(b+1))\cr
#' TGI  -0.5[190(R670-R550)-120(R670 - R480)] The triangular greenness index (TGI) estimates chlorophyll concentration in leaves and canopies\cr
#' GLI Green leaf index Vis GLI (2*g - r - b)/(2*g + r + b) Louhaichi et al. (2001)



#' 
#' @author
#' Chris Reudenbach, (VVI) Florian Detsch, (VVI) Tim Appelhans
#' 
#' @references
#' 
#' Planetary Habitability Laboratory (2015): Visible Vegetation Index (VVI). Available online via \url{http://phl.upr.edu/projects/visible-vegetation-index-vvi}.
#' 
#' Lacaux, J. P., Tourre, Y. M., Vignolles, C., Ndione, J. A., and Lafaye, M.: Classification of ponds from high-spatial resolution remote sensing: Application to Rift Valley Fever epidemics in Senegal, Remote Sens. Environ., 106, 66-74, 2007. 
#' 
#' Gitelson, A., et al.: Vegetation and Soil Lines in Visible Spectral Space: A Concept and Technique for Remote Estimation of Vegetation Fraction.  International Journal of Remote Sensing 23 (2002): 2537-2562. (VARI)
#' 
#' MADEIRA, J., BEDIDI, A., CERVELLE, B., POUGET, M. and FLAY, N., 1997, Visible spectrometric indices of hematite (Hm) and goethite (Gt) content in lateritic soils: 5490 N. Levin et al. the application of a Thematic Mapper (TM) image for soil-mapping in Brasilia, Brazil. International Journal of Remote Sensing, 18, pp. 2835-2852.
#'  
#' MATHIEU, R., POUGET, M., CERVELLE, B. and ESCADAFAL, R., 1998, Relationships between satellite-based radiometric indices simulated using laboratory reflectance data and typic soil colour of an arid environment. Remote Sensing of Environment, 66, pp. 17-28. 
#' 
#' Louhaichi, M., Borman, M.M., Johnson, D.E., 2001. Spatially located platform and aerial photography for documentation of grazing impacts on wheat. Geocarto International 16, 65-70.
#' 
#' Tucker, C.J., 1979. Red and photographic infrared linear combinations for monitoring vegetation. Remote Sensing of Environment 8, 127-150.
#' 
#' @seealso 
#' Wavelength ranges for overlapping digital camera bands are: red 580-670 nm, green 480-610 nm, and blue 400-520 nm (Hunt et al., 2005)
#' http://digitalcommons.unl.edu/cgi/viewcontent.cgi?article=2161&context=usdaarsfacpub
#' 
#' @examples
#' library(raster)
#' url <- "https://upload.wikimedia.org/wikipedia/commons/2/28/RGB_illumination.jpg"
#' dFile <- download.file(url, "Image.jpg")
#' img <- stack("Image.jpg") 
#' plotRGB(img)
#' rgbI <- rgbi(img)
#' plot(rgbI, col = gray(255:0/255))
#' 
#' @export rgbi
#' 

rgbi<- function(rgb,rgbi=c("red","green","blue","VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI")) {
  
  ### prerequisites
  
  ## compatibility check
  if (nlayers(rgb) < 3)
    stop("Argument 'rgb' needs to be a Raster* object with at least 3 layers (usually red, green and blue).")
  
  ### processing
  
  
  ## separate visible bands
  red <- rgb[[1]]
  green <- rgb[[2]]
  blue <- rgb[[3]]
  
  for (item in rgbi) {
    ## calculate vvi
    if (item=="VVI"){
      VVI <- (1 - abs((red - 30) / (red + 30))) * 
        (1 - abs((green - 50) / (green + 50))) * 
        (1 - abs((blue - 1) / (blue + 1)))
    } else if (item=="VARI"){
      # calculate VARI
      VARI<-(green-red)/(green+red-blue)
    } else if (item=="NDTI"){
      ## Normalized difference turbidity index
      NDTI<-(red-green)/(red+green)
    } else if (item=="RI"){
      # redness index
      RI<-red**2/(blue*green**3)
    } else if (item=="ci"){
      # CI Soil Colour Index
      CI<-(red-green)/(red+green)
    } else if (item=="BI"){
      #  Brightness Index
      BI<-sqrt((red**2+green**2+blue*2)/3)
    } else if (item=="SI"){
      # SI Spectra Slope Saturation Index
      SI<-(red-blue)/(red+blue) 
    } else if (item=="HI"){    
      # HI Primary colours Hue Index
      HI<-(2*red-green-blue)/(green-blue)
    } else if (item=="TGI"){
      # Triangular greenness index
      TGI <- -0.5*(190*(red - green)- 120(red - blue))
    } else if (item=="GLI"){
      # Green leaf index
      GLI<-(2*green-red-blue)/(2*green+red+blue)
    } else if (item=="NGRDI"){
      # NGRDINormalized green red difference index 
      NGRDI<-(green-red)/(green+red) 
    }  
    
  }
  ## return rgbi
  
  result <- stack(red,green,blue,VVI,VARI,NDTI,RI,CI,BI,SI,HI,TGI,GLI,NGRDI)
  result<-  stack(eval(rgbi))
  names(result) <- c("red","green","blue","VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI")
  return(result)
}