[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) 
[![Travis-CI Build Status](https://travis-ci.org/gisma/uavRst.svg?branch=master)](https://travis-ci.org/gisma/uavRst)
![](https://cranlogs.r-pkg.org/badges/grand-total/uavRst?color=green)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)

# Unmanned Aerial Vehicle R  Tools
The `uavR` tools consist of two packages:

  * mission planning ```uavRmp``` ([CRAN](https://CRAN.R-project.org/package=uavRmp))
  * remote sensing toolbox ```uavRst```   ([CRAN](https://CRAN.R-project.org/package=uavRst))


## Unmanned Aerial Vehicle Remote Sensing Toolbox - uavRst

The `uavRst`analysis toolbox package is far from being *mature*. You will need for most of the `uavRst`  functions a bunch of third party software. The most comfortable way to fulfill these requirements is to install `QGIS`, `GRASS`- and `SAGA-GIS`. Following the excellent provided by the  [`RQGIS`](https://CRAN.R-project.org/package=RQGIS) team will give you a good first try to ensure a smooth working environment.

 Most of the LiDAR related operations can be done by the great R package [`lidR`](https://CRAN.R-project.org/package=lidR). However for some of the basic point cloud related operations you will need to install the `LAStool` software. The [`LAStools`](http://lastools.org/download/LAStools.zip)  toolset is available at the [rapidlasso](https://rapidlasso.com/lastools/) homepage. Please download it and unzip it as usual. For Windows systems it is by default expected that you put it  at `C:/LASTools`, running  Linux at `~/apps/LASTools`. 
 
Note: For running LAStools tools under Linux you first need to install wine.

In addition you need to install the  `link2GI` package. 
In case of any problems drop an issue or try to use the actual github hosted version of the package. 

Nevertheless all mentioned software packages have to be installed correctly on your the OS. Most of it tested under Windows and Linux and should run... The most easiest way to obtain a fairly good runtime enviroment is to setup Linux as a dual boot system or in a VB. 

A full list of necessary libaries and binaries beyond ```R``` will hopefully (soon) be provided.

## Wiki
You will find some tutorials and examples at the uavRst [Wiki](https://github.com/gisma/uavRst/wiki). Please feel free to participate.


## Installation

**Note:** Currently (06/2020)  the `velox` package as well as the `spatial.tools` package are removed from CRAN and only available as an archived version. Up to now the functionality is not substituted. So you have to install both packages from source:

```
devtools::install_url('http://cran.r-project.org/src/contrib/Archive/spatial.tools/spatial.tools_1.6.2.tar.gz')

devtools::install_url('http://cran.r-project.org/src/contrib/Archive/velox/velox_0.2.0.tar.gz')

```



To install the actual development version from ```github```  you need to have installed the ```devtools``` package.

```R
devtools::install_github("gisma/uavRst", ref = "master")
```
