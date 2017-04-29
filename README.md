# uavRst
Unmanned Aerial Vehicle Remote Sensing Tools

====

```uavRst``` is a collection of tools for uav autonomous flight planning and analysis. Up to now it is dedicated to low budget rtf-UAVs as the DJI Phantom series and the 3DR Solo.

The packgage is far from beeing well organized. Nevertheless it can roughly diveded in 5 categories as marked by more or less meaningful prefixes:

  * flight planning (fp)
  * forest analysis (fa)
  * remote sensing  (rs)
  * archeology      (ao)
  * useful tools    (tool)


Please note that uavRst is making strong use of  GRASS7, SAGA GIS, JS, Python OTB and some othe CLI tools. The setup  of the correct linkage to these APIs can be cumbersome. For using the ```uavRST``` package you need to install the  ```link2GI``` package. Because the CRAN version is a bit outdated you should get the actual github hosted version of the [link2GI](https://github.com/gisma/link2GI/blob/master/README.md) package. 

Nevertheless all mentioned software packages have to be installed correctly on your the OS. It is just in parts tested under Windows but should run....The most easiest way to obtain a fairly good runtime enviroment is to setup Linux as a dual boot system or in a VB. If interested in setting up a clean Xubuntu or Mint Linux and then  use the  [postinstall script](http://giswerk.org/doku.php?do=export_code&id=tutorials:softgis:xubuntu:xubuntugis&codeblock=0setup) for installing most of the stuff. For using some of the the Solo related functions you need to install the [dronekit](http://python.dronekit.io/develop/installation.html) python libs in addition.

A full list of necessary libaries and binaries beyond ```R``` will soon be provided.


Even if honestly working on it it will be still a long run passing the CRAN check, nevertheless it runs fine for now ...



```S
devtools::install_github("gisma/uavRst", ref = "master")
```

If you want to install all dependencies use:

```S
devtools::install_github("gisma/uavRst", ref = "master", dependencies = TRUE)
```
