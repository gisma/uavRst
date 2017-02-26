# uavRst
Unmanned Aerial Vehicle Remote Sensing Tools

====

uavRst is a collection of tools for uav autonomous flight planning and analysis. Up to now it is dedicated to rtf low budget UAVs as the DJI Phantom series and the 3DR Solo.

The packgage is far from beeing well organized. Nevertheless it can roughly diveded in 4  categories:

  * flight planning 
  * forest analysis
  * remote sensing 
  * archeology


Please note that uavRst is making strong use of  GRASS7, SAGA GIS, JS, Python OTB and some othe CLI tools. Because the CRAN version is a bit outdated you should install the github hosted version of the [link2GI](https://github.com/gisma/link2GI/blob/master/README.md) package. All of them needs to be installed correctly on the OS. It is just in parts tested under Windows. The most easiest way to obtain a fairly good runtime enviroment is to setup Linux as a dual boot system or in a VB. If interested you find a [posinstall script](http://giswerk.org/doku.php?do=export_code&id=tutorials:softgis:xubuntu:xubuntugis&codeblock=0setup) doing most of the stuff.


It will be a long run passing the cran check, nevertheless it runs fine for now ...

For installation use devtools::install_github().

```S
devtools::install_github("gisma/uavRst", ref = "master")
```

If you want to install all dependencies use:

```S
devtools::install_github("gisma/uavRst", ref = "master", dependencies = TRUE, force = TRUE)
```
