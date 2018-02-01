# --- gi-ws-10-1 example control script 
# --- MOC - Advanced GIS 
# --- setup working environment 
# --- setup basic GI API links
# --- calculate crown segmentation
#
#  NOTE  It may occure that generic windows tools like fusion or lastools 
#        will NOT run under Windows most reasonable because I just tested 
#        the wine emulation on Linux platforms...
#
#        Whenever you run in trouble open an issue
#        https://github.com/logmoc/msc-phygeo-class-of-2017-creuden/issues
#        describe the problem with source code and add your sessionInfo() output
#
#  Codebase: https://github.com/logmoc/msc-phygeo-class-of-2017-creuden
# 
#
### -------------------------- setup the environment --------------------------

# --- Basic idea is to set up a a arbitrary folder ("rootDir") as a root point for 
# --- fixed subfolder structure. The project subfolder of the rootDir path is assigned
# --- using the variable "projDir".
# --- Within the courses you have to provide the "courseCode" ("gi","rs","da") and the 
# --- "activeSessionFolder" (1..15) wich should be the number of the class session you 
# --- like to work in.
# --- All functions like the setup procedure, formulas, wrappers etc. will be stored
# --- in the folder file.path(rootDir,projdir,"fun") and will be automatically sourced.

# --- The current script acts as main control script 

# --- That's it

#--> library requirements
rm(list =ls())
devtools::install_github("gisma/link2GI", ref = "master", dependencies = TRUE)
library(uavRst)
library(link2GI)
require(gdalUtils)
require(rgrass7)
require(raster)
require(mapview)

#--> NOTE point to whereever you want but avoid strange letters as dots etc
#--> the ~ is a substitute for the system variable HOME

#--> rootDir is general project folder  basic folder eg. C:/Dokumente/1_semester_MSCGEO/GIS/
if (Sys.info()["sysname"] == "Windows"){
  rootDir<-"e:/R_proj/teaching/"
} else {
  rootDir<-"~/lehre/msc/active/msc-2017/"
}

#-->  rootFolder of the github repository 
projDir<-"msc-phygeo-class-of-2017-creuden"
#--> current class
courseCode<-"gi"
#--> current class session folder
activeSessionFolder<-10

#--> projection string 
proj4="+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#--> create complete projDir string
projfolder<-file.path(rootDir,projDir,.Platform$file.sep,fsep = .Platform$file.sep)
#--> make a list of all functions in the corresponding function folder and source these functions
res<- sapply(list.files(pattern="[.]R$",path=paste0(projfolder,"/fun"),full.names=TRUE),FUN=source)

# workaround to use uavRst
makGlobalVar("path_run",gi_run)
makGlobalVar("path_tmp",gi_run)
makGlobalVar("path_output",gi_output)


unlink(paste0(path_run,"*"), force = TRUE)
# orthoimage filename
rgb<-raster::stack("/home/creu/lehre/msc/active/msc-2017/data/gis/input/ref/geonode-_476000_5630000_1.tif")
load("/home/creu/lehre/msc/active/msc-2017/data/gis/output/0_5_10_15_20_50stat.RData")

plot2<-raster::shapefile("/home/creu/lehre/msc/active/msc-2017/data/gis/input/ref/plot_UTM.shp")
tree3<-raster::shapefile("/home/creu/lehre/msc/active/msc-2017/data/gis/input/ref/crowns.shp")

chm<-statLayer[[2]][[3]]


ext<- raster::extent(477393.,477460. ,5631938. , 5632003.)
# link GDAL and SAGA
# gdal <- link2GI::linkgdalUtils()
saga <- link2GI::linkSAGA()

# ---------- start pr-processing ---------------------------------------------------  ---------------------------------------------------

cat("\n::: run pre processing...\n")
chmR<-raster::crop(chm,ext)
raster::writeRaster(chmR,paste0(gi_run,"chm.tif"),
                    overwrite = TRUE)
#chmR<- chmR * -1 + raster::maxValue(chmR)
indices<-c("HI", "GLI",  "GRVI")
indices <- c("chm")

 for (item in indices){
   gdaldem(item,paste0(gi_run,"chm.tif"),paste0(gi_run,item,".tif"))
   
   gdalUtils::gdalwarp(paste0(path_run,item,".tif"), 
                       paste0(path_run,item,".sdat"), 
                       overwrite = TRUE,  
                       of = 'SAGA',
                       verbose = FALSE)
 }


# ----- start ADDON RGB image processing ---------------------------------------------------
#  rgb <- raster::crop(rgb,ext)
#  rgb <- raster::resample(rgb, chmR, method = 'bilinear')
#  raster::writeRaster(rgb,paste0(path_output,"ortho.tif"),
#                      overwrite = TRUE)
# # #cat("::: calculate RGBI... \n")
# rgbI <- uavRst::rgbIndices(rgb[[1]],rgb[[2]],rgb[[3]],indices)
# 
#  cat("\n convert indices to SAGA... \n")
#  i <- 1
#  for (index in indices) {
#    cat("convert ",index,"\n")
# #   gdalUtils::gdalwarp(paste0(path_run,index,".tif"),paste0(path_run,index,".sdat"), overwrite=TRUE,  of='SAGA') 
#    uavRst::r2saga(rgbI[[i]],index)
#    i <- i + 1
#  }
#
# ----  start crown analysis --------------------------------------------------------

# call tree crown segmentation 
seeds <- uavRst::fa_treeSeeding(chmR,
                                 minTreeAlt = 12.5,
                                 crownMinArea = 3,
                                 crownMaxArea = 225,
                                 is0_join = 1, 
                                 is0_thresh = 0.20, 
                                 seeding = TRUE
)

# call tree crown segmentation 
crowns <- uavRst::fa_crown_segmentation(seeds = seeds,
                                        majority_radius = 9.0,
                                        is3_sig1 = 0.05,
                                        is3_sig2 = 0.05,
                                        is3_threshold = 0.00005,
                                        is3_seed_params = indices,
                                        seeding = TRUE
)

# calculate potential insolation
#uavRst::potInsolation("chm",pi_day = "01/06/2017",pi_day_stop = "30/06/2017",pi_hour_step = 1.0)

cat("::: run post-classification...\n")
# extract stats
polyStat <- uavRst::xpolystat(c("chm"),
                              spdf = crowns)

# calculate metrics of the crown geometries
crowns <- uavRst::fa_caMetrics(polyStat)

# export geojson
sf::st_write(sf::st_as_sf(crowns), "crowns.geojson",delete_dsn=TRUE,driver="GeoJSON")
rgdal::writeOGR(obj = crowns,
                layer = "crowns", 
                driver = "ESRI Shapefile", 
                dsn = path_run, 
                overwrite_layer = TRUE)
# simple filtering of crownareas based on tree height min max area and artifacts at the analysis/image borderline
trees_crowns <- uavRst::fa_basicTreeCrownFilter(crownFn = paste0(path_run,"crowns.geojson"),
                                                minTreeAlt = 5,
                                                crownMinArea = 5,
                                                crownMaxArea = 150,
                                                mintreeAltParam = "chmQ20"
                                                )


# extract all pixvalues according to a vector geometry
# pixvalues <- basicExtraction(x = chmR,fN = trees_crowns_2[[2]],responseCat = "ID")

# cut result is with reference
pl<-mapview(plot2)
tc<-mapview(trees_crowns[[2]])
tc+p
finalTrees<-rgeos::gIntersection(plot2,trees_crowns[[2]],,byid = TRUE,)
plot(finalTrees)
cat(":: ...finsihed \n")
