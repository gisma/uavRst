# ---  example control script for lidar data 
#
# -------------------------- setup the environment ----------------------------

# clean environment
rm(list =ls())
# library requirements
library(rgrass7)
# switch if you want to correct las files for extension, overlap and scaling issues
correctLas = FALSE
# define project settings, data folders etc
# define root project folder
projRootDir <- "~/proj/uav/thesis/finn"
# define lidar data folder
las_data_dir <- "~/proj/uav/thesis/finn/data/lidar/"
# define all subfolders you want need in your project note they can be cascaded
projFolders = c("data/","input/","output/","run/","las/")
# switch for exporting the folders as global variables
global = TRUE
#  prefix of the global variables the wil look like path_prefixPathName
path_prefix = "path_"
# proj4 string of ALL data
proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "

# reference shape filename
plot2<-raster::shapefile("/home/creu/lehre/msc/active/msc-2017/data/gis/input/ref/plot_UTM.shp")

# create project structure and export global pathes
paths<-link2GI::initProj(projRootDir = projRootDir,
                         projFolders = projFolders,
                         global = TRUE,
                         path_prefix = path_prefix)

# link all CLI stuff
giLinks<-linkBuilder()
# expand las folder
las_data_dir<-path.expand(las_data_dir)

# ------------------------ Optional LAS File correction------------------------

if (correctLas){
  cat("\n: correcting las files...\n")
  # get list of las files
  lasfiles<-list.files(las_data_dir,pattern="*.las$", full.names=FALSE) 
  # store original names
  origlasfiles<-lasfiles
  # for all las files 
  for (j in 1:(length(lasfiles))) {
    cat(":: check extent patterns...\n")
    # get extension
    ext<-lasTool(lasDir = paste0(path_input, lasfiles[j]))
    # GRASS linkage
    link2GI::linkGRASS7(spatial_params = c(ext[2],ext[1],ext[4],ext[3],"+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
    # try if GRASS is able to allocate enough memory
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
    # if so go ahead
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

# list of height pairs as used for vertical slicing the las data
zrList <- list(c(0,5,10,15,20,50))
zrange<- makenames(zrList)[[2]]
zrnames<- makenames(zrList)[[1]]

# list of statistic calculation for more info see r.in.lidar help
statList <- list("max", "median","range")

# target grid size
gridsize <- 0.5

# horizontal aggregation kernel size (if needed)
focalSize <- 3

# projection string 
proj4="+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

### ------------------------ Main Script ----------------------------

# NOTE the used GRASS modules are wrapped using the rgrass7. they can be found as single functions 
#      in the file grassLiDAR.R file. The main control structure is written below as a mixture of function 
#      calls and generic R code

cat(": starting calculations...\n")

# define some list variables
m_vdr <- m_fhd <-vdr <- fhd <- zrLayer <- statLayer <- hdl1<- hdl2<- gapl<- list() 

# for each existing las file do
for (j in 1:(length(lasfiles))) {
  
  # get extent of the lasfile
  ext<-lasTool(lasFile= paste0(las_data_dir, lasfiles[j]))
  # create a corresponding GRASS location
  link2GI::linkGRASS7(gisdbase = path_data, location = "las",spatial_params = c(ext[2],ext[1],ext[4],ext[3],proj4),resolution = gridsize,returnPaths = T,gisdbase_exist = T)
  
  # create a straightforward dem 
  uavRst:::r_in_lidar(input = paste0(las_data_dir,lasfiles[j]), 
                      output = paste0("dem_",j),
                      method = "min",
                      resolution = gridsize,
                      class_filter = 2,
                      flags = c("e","n","overwrite","o","v"))
  
  # write GRASS to TIF
  raster::writeRaster(raster::raster(rgrass7::readRAST(paste0("dem_",j))),paste0(path_data,"dem_",j), overwrite=TRUE,format="GTiff")
  # fill data gaps 
  fillGaps(path_data,paste0("dem_",j))
  
  # for each height slice do then
  for ( i in 1:(length(zrnames))) {
    
    # slice las file according to height breaks and reduce the altitude by subtracting the base raster
    # NOTE the v flag allows only for valid points
    r_in_lidar(input = paste0(las_data_dir,lasfiles[j]), 
                        output = zrnames[i],
                        method = "n",
                        base_raster = paste0("dem_",j),
                        zrange = c(zrange[[i]][1],zrange[[i]][2]),
                        class_filter = classFilter,
                        resolution=gridsize,
                        flags = c("d","overwrite","o","v")
    )
    # write GRASS to TIF
    raster::writeRaster(raster::raster(rgrass7::readRAST(zrnames[i])),paste0(path_data,zrnames[i]), overwrite=TRUE,format="GTiff")
    # fill data gaps 
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
    # write GRASS to TIF
    raster::writeRaster(raster::raster(rgrass7::readRAST(paste0(meth,"_veg"))),paste0(path_data,paste0(meth,"_veg")), overwrite=TRUE,format="GTiff")
    # fill data gaps 
    uavRst:::fillGaps(path_data,paste0(meth,"_veg"))
  }
  
  
  # get all height layers back from GRASS as a stack 
  zrLayer[[j]] <- raster::stack(lapply(zrnames ,
                                       function(x){raster::raster(rgrass7::readRAST(x))})
  )
  
  
  # get all statistic layers back from GRASS as a stack 
  statLayer[[j]] <- raster::stack(lapply(statList ,
                                         function(x){raster::raster(rgrass7::readRAST(paste0(x,"_veg"),NODATA=-9999))})
  )
  
  cat(": starting calculation of indices...\n")
  # calculate FHD foliage height density  -> diversityindeces.R
  fhd[[j]]<- fun_fhd(zrLayer[[j]])
  
  # calculate VDR vertical density ratio -> diversityindeces.R
  vdr[[j]]<- fun_vdr(statLayer[[j]][[3]],statLayer[[j]][[2]])
}

