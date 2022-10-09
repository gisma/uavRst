#' create name vector corresponding to the training image stack
#' create vector containing the names of the image stack claculated using \code{\link{calc_ext}}
#' @param rgbi character. codes of the RGB indices
#' @param bandNames character.  band names
#' @param stat character.  stat codes
#' @param morpho character.  morpho codes
#' @param edge character.  edge codes
#' @param rgbTrans character.  rgbTrans codes
#' @param dem charater. dem codes
#' @param l_raster number of raster layer that exist
#' @keywords internal
#'
#' @export make_bandnames

make_bandnames <- function(rgbi    = NA,
                           bandNames = NA,
                           stat    = FALSE,
                           morpho  = NA,
                           edge    = NA ,
                           rgbTrans = NA,
                           dem =    NA,
                           l_raster = NA,
                           pca=NA){
  if (!is.na(rgbi[1])) bandNames <- append(c("red","green","blue"),rgbi)
  if (!is.na(bandNames[1])) {
    if(bandNames[1] == "simple"){
      bandNames <- c("Energy", "Entropy", "Correlation",
                     "Inverse_Difference_Moment", "Inertia",
                     "Cluster_Shade", "Cluster_Prominence",
                     "Haralick_Correlation")
    } else if(bandNames[1] == "advanced"){
      bandNames <- c("Hara_Mean", "Hara_Variance", "Dissimilarity",
                     "Sum_Average",
                     "Sum_Variance", "Sum_Entropy",
                     "Difference_of_Variances",
                     "Difference_of_Entropies",
                     "IC1", "IC2")
    } else if(bandNames[1] == "higher"){
      bandNames <- c("Short_Run_Emphasis",
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
    } else if(bandNames[1] == "all"){
      bandNames <- c("Energy", "Entropy", "Correlation",
                     "Inverse_Difference_Moment", "Inertia",
                     "Cluster_Shade", "Cluster_Prominence",
                     "Haralick_Correlation",
                     "Hara_Mean", "Hara_Variance", "Dissimilarity",
                     "Sum_Average",
                     "Sum_Variance", "Sum_Entropy",
                     "Difference_of_Variances",
                     "Difference_of_Entropies",
                     "IC1", "IC2",
                     "Short_Run_Emphasis",
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
    if (!is.na(l_raster)) bandNames<-bandNames[1:l_raster]
  }
  if (stat == TRUE)  {
    bandNames    = c("Stat_Mean","Stat_Variance", "Stat_Skewness", "Stat_Kurtosis")
  }
  if (!is.na(dem))  {
    bandNames    =  dem
  }
  
  if (!is.na(morpho))  {
    bandNames    =  morpho
  }
  
  if (!is.na(edge))  {
    bandNames    =  edge
  }
  if (!is.na(pca)[1])  {
    bandNames    =  "PCA"
  }
  if (!is.na(rgbTrans))  {
    if (rgbTrans %in% c("Gray"))
      bandNames    =  bandNames <- c(paste0(rgbTrans,"_b1"))
    # c("cielab","CMY","Gray","HCL","HSB","HSI","Log","XYZ","YUV")
    else if (rgbTrans %in% c("cielab"))
      bandNames    =  bandNames <- c(paste0(rgbTrans,"_b1"),paste0(rgbTrans,"_b2"),paste0(rgbTrans,"_b3"),paste0(rgbTrans,"_b4"))
    else 
      bandNames    =  bandNames <- c(paste0(rgbTrans,"_b1"),paste0(rgbTrans,"_b2"),paste0(rgbTrans,"_b3"))    
  }
  return(bandNames)
  
}