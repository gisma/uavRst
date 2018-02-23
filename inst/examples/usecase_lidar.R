
#--> library requirements
rm(list =ls())
library(rgrass7)
correctLas = FALSE
# define project settings, data folders etc
# define project folder
projRootDir <- "~/proj/uav/thesis/finn"
# lidar data folder
las_data_dir <- "~/proj/uav/thesis/finn/data/lidar/"
# proj subfolders
projFolders = c("data/","input/","output/","run/","las/")
# export folders as global
global = TRUE
# with folder name plus following prefix
path_prefix = "path_"
# proj4 string of ALL data
proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "

# referenz shape filename
plot2<-raster::shapefile("/home/creu/lehre/msc/active/msc-2017/data/gis/input/ref/plot_UTM.shp")

# create project structure and export global pathes
paths<-link2GI::initProj(projRootDir = projRootDir,
                         projFolders = projFolders,
                         global = TRUE,
                         path_prefix = path_prefix)

# link all CLI stuff
giLinks<-uavRst:::linkBuilder()

las_data_dir<-path.expand(las_data_dir)
### ---------------------------- Thematic  Settings ----------------------------

#    0) basic correction of las files TODO put it in a preprocessing step
#    1) reducing the oversampling of counts using lasoverage
#    2) rescaling of las files to a 1 cm resolution
#    3) TODO set projection with https://www.liblas.org/utilities/las2las.html 
#       (needs some adaption due to naming conflicts)
#       las2las -i in.las -o out.laz -utm 32N -vertical_wgs84
# NOTE no correction of broken extents is performed this will be done during runtime
#

if (correctLas){
  cat("\n: correcting las files...\n")
  lasfiles<-list.files(las_data_dir,pattern="*.las$", full.names=FALSE) 
  origlasfiles<-lasfiles
  for (j in 1:(length(lasfiles))) {
    cat(":: check extent patterns...\n")
    ext<-lasTool(lasDir = paste0(path_input, lasfiles[j]))
    link2GI::linkGRASS7(spatial_params = c(ext[2],ext[1],ext[4],ext[3],"+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
    m<-try(execGRASS("r.in.lidar",
                     input = paste0(las_data_dir, lasfiles[j]),
                     output = "tst",
                     flags = c("e","n","overwrite","o","v"),
                     resolution = 10,
                     method = "min",
                     class_filter = 2,
                     echoCmd=FALSE,
                     intern = TRUE,
                     ignore.stderr = FALSE))
    if (!class(m)=="try-error") {
      cat(":: reducing overlap patterns...\n")
      lasTool("lasoverage",paste0(las_data_dir, lasfiles[j]))
      cat(":: rescaling las files...\n")
      lasTool("rescale",paste0(las_data_dir,"o_", lasfiles[j]))
    } else {
      file.rename(paste0(las_data_dir, lasfiles[j]), paste0(las_data_dir, "s_o_",lasfiles[j]))
    }
    # getting the new las file list
    
  }
  correctLas = TRUE
  lasfiles<-list.files(paste0(las_data_dir),pattern="s_o_", full.names=FALSE) 
} else {
  
  lasfiles<-list.files(paste0(las_data_dir),pattern="s_o_", full.names=FALSE) 
}

# for corrected las files classFilter has to be 13 
# if running uncorrected lasfiles set it to 2
classFilter<-13

#--> list of height pairs as used for slicing the las data
zrList <- list(c(0,5,10,15,20,50))
zrange<- uavRst:::makenames(zrList)[[2]]
zrnames<- uavRst:::makenames(zrList)[[1]]


#--> list of statistic calculation for more info see r.in.lidar help
statList <- list("max", "median","range")

#--> target grid sizec(0,5,10,15,20,50)
gridsize <- 0.5
# horizontal aggregation level
focalSize <- 3
#--> projection string 
proj4="+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

### ---------------------------- here we go ----------------------------

# NOTE the used GRASS modules are wrapped using the rgrass7. they can be found as single functions 
#      in the file grassLiDAR.R file. The main control structure is written below as a mixture of function 
#      calls and generic R code
#      
cat(": starting calculations...\n")
# define some list variables
m_vdr <- m_fhd <-vdr <- fhd <- zrLayer <- statLayer <- hdl1<- hdl2<- gapl<- list() 

# for each existing las file do
for (j in 1:(length(lasfiles))) {
  
  # create *temporyry* GRASS location
  ext<-uavRst:::lasTool(lasFile= paste0(las_data_dir, lasfiles[j]))
  link2GI::linkGRASS7(gisdbase = path_data, location = "las",spatial_params = c(ext[2],ext[1],ext[4],ext[3],proj4),resolution = gridsize,returnPaths = T,gisdbase_exist = T)
  
  # create straightforward dem 
  uavRst:::r_in_lidar(input = paste0(las_data_dir,lasfiles[j]), 
             output = paste0("dem_",j),
             method = "min",
             resolution = gridsize,
             class_filter = 2,
             flags = c("e","n","overwrite","o","v"))
  
  # fill data gaps 
  raster::writeRaster(raster::raster(rgrass7::readRAST(paste0("dem_",j))),paste0(path_data,"dem_",j), overwrite=TRUE,format="GTiff")
  uavRst:::fillGaps(path_data,paste0("dem_",j))
  
  # for each height break do
  for ( i in 1:(length(zrnames))) {
    
    # slice las file according to height breaks and reduce the altitude by subtracting the base raster
    # NOTE the v flag allows only for valid points
    uavRst:::r_in_lidar(input = paste0(las_data_dir,lasfiles[j]), 
               output = zrnames[i],
               method = "n",
               base_raster = paste0("dem_",j),
               zrange = c(zrange[[i]][1],zrange[[i]][2]),
               class_filter = classFilter,
               resolution=gridsize,
               flags = c("d","overwrite","o","v")
    )
    # fill data gaps 
    raster::writeRaster(raster::raster(rgrass7::readRAST(zrnames[i])),paste0(path_data,zrnames[i]), overwrite=TRUE,format="GTiff")
    uavRst:::fillGaps(path_data,zrnames[i])
  }
  
  # calculate the statistics as given  by the statList
  for (meth in statList ){
    uavRst:::r_in_lidar(input = paste0(las_data_dir,lasfiles[j]), 
               output = paste0(meth,"_veg"),
               method = meth,
               base_raster = paste0("dem_",j),
               resolution = gridsize,
               flags = c("d","overwrite","o","v")
    )
    raster::writeRaster(raster::raster(rgrass7::readRAST(paste0(meth,"_veg"))),paste0(path_data,paste0(meth,"_veg")), overwrite=TRUE,format="GTiff")
    uavRst:::fillGaps(path_data,paste0(meth,"_veg"))
  }
  
  
  # import and stack the data from GRASS back to R
  # all height layers
  zrLayer[[j]] <- raster::stack(lapply(zrnames ,
                               function(x){raster::raster(rgrass7::readRAST(x))})
  )
  
  test <- sum(raster::stack(lapply(zrnames[2:5] ,
                           function(x){raster::raster(rgrass7::readRAST(x))})
  ))
  
  # all stat layers
  statLayer[[j]] <- raster::stack(lapply(statList ,
                                 function(x){raster::raster(rgrass7::readRAST(paste0(x,"_veg"),NODATA=-9999))})
  )
  cat(": starting calculation of indices...\n")
  # calculate indices
  # FHD foliage height density  -> diversityindeces.R
  fhd[[j]]<- uavRst:::fun_fhd(zrLayer[[j]])
  
  # VDR vertical density ratio -> diversityindeces.R
  vdr[[j]]<- uavRst:::fun_vdr(statLayer[[j]][[3]],statLayer[[j]][[2]])
}

