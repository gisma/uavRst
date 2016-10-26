# uavRst
Unmanned Aerial Vehicle Remote Sensing Tools

====

uavRst is a collection of tools for uav autonomous flight planning and analysis for the DJI Phantom and 3DR Solo platforms.

In addition to R packages it is using GRASS, SAGA, JS and Python tools that have to be installed correctly on the OS. 


It will be a long run passing the cran check, nevertheless it runs fine for now ...

For installation use devtools::install_github().

```S
devtools::install_github("gisma/uavRst", ref = "master")
```

If you want to install all dependencies use:

```S
devtools::install_github("gisma/uavRst", ref = "master", dependencies = TRUE, force = TRUE)
```
