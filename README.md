# uavRst
Unmanned Aerial Vehicle Remote Sensing Tools


The [uavRst](https://github.com/gisma/uavRst) package for is designed for uav autonomous flight planning and analysis to open the opportunity for low budget, open source based remote sensing of surfaces using ```R```. It is aiming to provide an easy to use workflow for controlling rtf UAVs from planning and flying autonomous surveys, over pre- and post-processing of the derived data.

Up to now it is dedicated to low budget rtf-UAVs as the DJI Phantom series and all Pixhawk based uavs like the 3DR Solo.

## Supported UAV platforms

The reason using DJI is their absolute straightforward usage. Everybody can fly with a DJI but the price to pay off is a hermetically closed system. Only the litchi app provides additionally to a cloud based mission planer an offline/standalone interface that is up to date and facilitate the upload of a CSV formatted waypoint file to control autonomous flights with the the Phantom.

The open uav community is focused on the PixHawk autopilot unit and the Mission Planner software. It is well documented and several APIs are provided. Nevertheless an affordable terrain following autonomous flight planning tool is not available yet. It creates basic ```MAVLINK``` format compliant mission files that can be uploaded directly or via a Ground Control Station to the Pixhawk controller/Solo.

## Mission planning 

The core planning tool ```makeFP``` (make flight plan) creates either intermediate flight control files for the dji phantom x UAVs or ready to upload control files for the 3DR Solo. The dji control files are designed for using with the proprietary litchi flight control app exchange format, while the 3DR Solo files are using the ```MAVLINK``` common message format, that is used by the PixHawk flight controller family.

## Analysis

The package is far from being well organized. Nevertheless including the flight planning it can roughly divided in 5 categories as marked by more or less meaningful prefixes:

  * flight planning (fp)
  * forest analysis (fa)
  * remote sensing (rs)
  * archaeology (ao)
  * useful tools (tool)

Please note that uavRst is making strong use of  GRASS7, SAGA GIS, JS, Python OTB and some othe CLI tools. The setup  of the correct linkage to these APIs can be cumbersome. For using the ```uavRST``` package you need to install the  ```link2GI``` package. Because the CRAN version is a bit outdated you should get the actual github hosted version of the [link2GI](https://github.com/gisma/link2GI/blob/master/README.md) package. 

Nevertheless all mentioned software packages have to be installed correctly on your the OS. It is just in parts tested under Windows but should run....The most easiest way to obtain a fairly good runtime enviroment is to setup Linux as a dual boot system or in a VB. If interested in setting up a clean Xubuntu or Mint Linux and then  use the  [postinstall script](http://giswerk.org/doku.php?do=export_code&id=tutorials:softgis:xubuntu:xubuntugis&codeblock=0setup) for installing most of the stuff. For using some of the the Solo related functions you need to install the [dronekit](http://python.dronekit.io/develop/installation.html) python libs in addition.

A full list of necessary libaries and binaries beyond ```R``` will soon be provided.

Even if honestly working on it it will be still a long run passing the CRAN check, nevertheless it runs fine for now ...

To install from ```github```  you need to have installed the ```devtools``` package.

```S
devtools::install_github("gisma/uavRst", ref = "master")
```

If you want to install all dependencies use:

```S
devtools::install_github("gisma/uavRst", ref = "master", dependencies = TRUE)
```
