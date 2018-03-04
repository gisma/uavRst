# ---  example control script for lidar data correction
#
# -------------------------- setup the environment ----------------------------

# clean environment
rm(list =ls())

# library requirements
require(rgrass7)

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
giLinks<-get_gi()

# expand las folder
las_data_dir<-path.expand(las_data_dir)


# ------------------------ LAS File correction------------------------

cat(":: reducing overlap patterns...\n")
lastool("lasoverage",paste0(las_data_dir, lasfiles[j]))
cat(":: rescaling las files...\n")
lastool("rescale",paste0(las_data_dir,"o_", lasfiles[j]))
lasfiles<-list.files(paste0(las_data_dir),pattern="s_o_", full.names=FALSE)
# for corrected las files classFilter has to be 13
# if running uncorrected lasfiles set it to 2
classFilter<-13

# check if extent is ok if not try to correct
for (i  in 1:length(lasfiles)) lastool(lasFile= paste0(las_data_dir, lasfiles[i]))

# decompress and merge  point cloud files
lastool(tool="laz2las" , lasFile =paste0(las_data_dir,"*"),outpath=las_data_dir)
lastool(tool="lasmerge" , lasFile =las_data_dir, outpath=las_data_dir)
# get new list of las files
lasfiles<-list.files(paste0(las_data_dir),pattern="merge", full.names=FALSE)


