[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) 
[![Travis-CI Build Status](https://travis-ci.org/gisma/uavRst.svg?branch=master)](https://travis-ci.org/gisma/uavRst)
[![GitHub last commit](https://img.shields.io/github/last-commit/google/skia.svg)](/master/)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)

# Unmanned Aerial Vehicle Remote Sensing Tools
The `uavR` tools consist of two packages:

  * mission planning ```uavRmp``` (already on [CRAN](https://CRAN.R-project.org/package=uavRmp))
  * remote sensing toolbox ```uavRst```

## Mission Planning - uavRmp

The `uavRmp` package provides functions for rtf-UAV based autonomous mission planning. In the first place it is a simple and open source planning tool to plan autonomous terrainfollowing monitoring flights of low budget drones based on ```R```. It provides an easy workflow for survey planning including battery-dependent task splitting, obstacle avoiding departures, and approaches of each monitoring chunks or spatial position. 


## Unmanned Aerial Vehicle Remote Sensing Toolbox - uavRst

The `uavRst`analysis toolbox package is far from being *mature*. You will need for most of the `uavRst`  functions a bunch of third party software. The most comfortable way to fulfill these requirements is to install `QGIS`, `GRASS`- and `SAGA-GIS`. Following the excellent provided by the  [`RQGIS`](https://CRAN.R-project.org/package=RQGIS) team will give you a good first try to ensure a smooth working environment.

 Most of the LiDAR related operations can be done by the great R package [`lidR`](https://CRAN.R-project.org/package=lidR). However for some of the basic point cloud related operations you will need to install the `LAStool` software. The [`LAStools`](http://lastools.org/download/LAStools.zip)  toolset is available at the [rapidlasso](https://rapidlasso.com/lastools/) homepage. Please download it and unzip it as usual. For Windows systems it is by default expected that you put it  at `C:/LASTools`, running  Linux at `~/apps/LASTools`. 
 
Note: For running LAStools tools under Linux you first need to install wine.

In addition you need to install the  `link2GI` package. 
In case of any problems drop an issue or try to use the actual github hosted version of the package. 

Nevertheless all mentioned software packages have to be installed correctly on your the OS. Most of it tested under Windows and Linux and should run... The most easiest way to obtain a fairly good runtime enviroment is to setup Linux as a dual boot system or in a VB. 

A full list of necessary libaries and binaries beyond ```R``` will hopefully (soon) be provided.

## Wiki
You will find some tutorials and examples at the uavRst Wiki. Please feel free to participate.

## CRAN

The CRAN checks are passing - so it is only a question of documentation and time until it will be submitted... however it runs fine for now ...

To install from ```github```  you need to have installed the ```devtools``` package.

```R
devtools::install_github("gisma/uavRst", ref = "master")
```
