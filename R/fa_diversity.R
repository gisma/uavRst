# foliage height density
# Originally MacArthur & MacArthur (1961)
# Implemented after:
# Hashimoto, H., Imanishi, J., Hagiwara, A., Morimoto, Y., & Kitada, K. (2004). Estimating forest structure indices for evaluation of forest bird habitats by an airborne laser scanner. In M. Thies, B. Koch, H. Spiecker, & H. Weinacker (Eds.), Laser scanners for forest and landscape assessment: Proceedings of the ISPRS Working Group VIII/2, Freiburg, 3-6 October 2004, 254-258.
# http://www.isprs.org/proceedings/XXXVI/8-W2/HASHIMOTO.pdf

#TOFIX #@name
#@titel
#@description
#@author

fun_fhd <- function(a) {
  l <- raster::nlayers(a)
  a[a<=0]=1
  p_i <- a/a[[l]]
  r <- p_i * log(a / p_i)
  sum(r[[1:(l-1)]])
}

#slightly changed for GridMetric output (gives already pi)
#TOFIX #@name
#@titel
#@description
#@author

fun_fhd_fu <- function(b) {
  a <- subset(b, 1:6)
  l<-raster::nlayers(a)
  r<- -1 * ((a[[l]]) * log(a[[l]]))
  sum(r[[1:(l-1)]])
}

# Vertical distribution ratio (VDR)
# VDR is a ratio of the distance between the canopy height return and the median height
# return. VDR = [CH-HOME]/CH. 
# Forested regions are characterized by a dense canopy and sparse understory will exhibit lower VDR values
# Areas characterized by a more even distribution of biomass throughout the vertical profile will exhibit larger VDRs (closer to 1)
# Goetz, S. J., D. Steinberg, R. Dubayah, and B. Blair. 2007. Laser remote sensing of canopy habitat heterogeneity as a predictor of bird species richness in an eastern temperate forest, USA. Remote Sensing of Environment 108:254-263. 
# http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.111.2979&rep=rep1&type=pdf

fun_vdr <- function(max,med) {
  med[med < 0]=0
  max[max < 0]=0
  vdr <- (max - med) / max
  vdr[is.na(vdr)]=0
  vdr[is.infinite(vdr)]=0
  vdr[vdr<0]=0
  return(vdr)
}
