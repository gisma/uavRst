% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fa_crownmetrics.R
\name{poly_metrics}
\alias{poly_metrics}
\title{calculate morphometric features of polygons.}
\usage{
poly_metrics(
  crownarea,
  funNames = c("length", "elongation", "eccentricityboundingbox", "solidity",
    "eccentricityeigen", "calliper", "rectangularity", "circularityharalick",
    "convexity")
)
}
\arguments{
\item{crownarea}{sp*  spatialpolygon object}

\item{funNames}{character. names of morphometrics to be calculated
available are ("length","elongation","eccentricityboundingbox","solidity","eccentricityeigen",
"calliper","rectangularity","circularityharalick","convexity")}
}
\value{
sp* object
}
\description{
calculate morphometric features of polygons. Calculate some crown related metrics, returns the metrics as a spatialpointdataframe/spatialpolygondataframe
}
\examples{

\dontrun{
# required packages
require(uavRst)
require(mapview)

# get the rgb image, chm and training data
utils::download.file("https://github.com/gisma/gismaData/raw/master/uavRst/data/tutorial_data.zip",
                      paste0(tempdir(),"tutorial_data.zip"))
unzip(zipfile = paste0(tempdir(),"tutorial_data.zip"), exdir = tempdir())
crown<-raster::shapefile(paste0(tempdir(),"rgb_3-3_train2.shp"))
## calculate polygon morpho metrics
polymetric <- poly_metrics(crown)

# visualize it
mapview::mapview(polymetric)
##+}
}
\seealso{
\href{https://CRAN.R-project.org/package=Momocs}{Momocs}
\href{https://www.researchgate.net/profile/Paul_Rosin/publication/228382248_Computing_global_shape_measures/links/0fcfd510802e598c31000000.pdf?origin=publication_detail}{Paul Rosin}
}
