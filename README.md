[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active) 
[![Travis-CI Build Status](https://travis-ci.org/gisma/uavRst.svg?branch=master)](https://travis-ci.org/gisma/uavRst)
[![GitHub last commit](https://img.shields.io/github/last-commit/google/skia.svg)](/master/)
[![License](https://img.shields.io/badge/license-GPL%20%28%3E=%203%29-lightgrey.svg?style=flat)](http://www.gnu.org/licenses/gpl-3.0.html)

# Unmanned Aerial Vehicle Remote Sensing Tools
The [uavR](https://github.com/gisma/uavR) "package family" is divided in in two packages:

  * mission planning ```uavRmp``` (already on [CRAN](https://cran.r-project.org/web/packages/uavRmp/index.html))
  * remote sensing toolbox ```uavRst```

## Mission Planning - uavRmp

It is strongly encouraged to use the new package for flight planning [uavRmp](https://github.com/gisma/uavRmp) for uav autonomous mission planning. In the first place it is a simple and open source planning tool for monitoring flights of low budget drones based on ```R```. It provide an easy workflow for planning autonomous 
surveys including battery-dependent task splianantting, save departures, and approaches of each monitoring chunks. 


## Unmanned Aerial Vehicle Remote Sensing Toolbox - uavRst

The ```uavRst``` analysis toolbox package is far from being *mature*. In addition you will need for running the most of the ```uavRst```  functions a bunch of third party software. The most comfortable way to fulfill most of the requirements is to install QGIS, GRASS- and SAGA-GIS and depending on the choosen tool in addition the Fusion tools for the fusion processing chain of LiDAR data. Following the excellent [installation instructions](https://github.com/jannes-m/RQGIS/blob/master/vignettes/install_guide.Rmd)  of the [RQGIS](https://cran.r-project.org/web/packages/RQGIS/index.html) package will have a good first try to ensure a smooth working environment.

The [Fusion toolset](http://forsys.sefs.uw.edu/fusion/fusionlatest.html) is available at the developer homepage. Please download it and install it as usual. Note you have to adapt the Installation folder in the ``controlFusion.txt`` file. If you are running a default installation on Windows you do not need to change the path. 

The [LAStools toolset](http://lastools.org/download/LAStools.zip) is available at the [rapidlasso](https://rapidlasso.com/lastools/) homepage. Please download it and unzip it as usual. For Windows systems it is expected that you put it  at `C:/LASTools` under Linux at `~/apps/LASTools`. 

Note: For running Fusion and LAStools tools under Linux you first need to install wine.

In addition you need to install the  ```link2GI``` package. 
In case of any problems drop an issue or try to use the actual github hosted version of the [link2GI](https://github.com/gisma/link2GI/blob/master/README.md) package. 

Nevertheless all mentioned software packages have to be installed correctly on your the OS. Most of it tested under Windows and Linux and should run....The most easiest way to obtain a fairly good runtime enviroment is to setup Linux as a dual boot system or in a VB. If interested in setting up a clean Xubuntu or Mint Linux and then  use the  [postinstall script](http://giswerk.org/doku.php?do=export_code&id=tutorials:softgis:xubuntu:xubuntugis&codeblock=0setup) for installing most of the stuff. For using some of the the Solo related functions you need to install the [dronekit](http://python.dronekit.io/develop/installation.html) python libs in addition.

A full list of necessary libaries and binaries beyond ```R``` will hopefully (soon) be provided.

Even if honestly working on it it will be still a long run passing the CRAN check, nevertheless it runs fine for now ...

To install from ```github```  you need to have installed the ```devtools``` package.

```S
devtools::install_github("gisma/uavRst", ref = "master")
```

If you want to install all dependencies (may take a while) use:

```S
devtools::install_github("gisma/uavRst", ref = "master", dependencies = TRUE)
```
