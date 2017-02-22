
if (!isGeneric('pc2DTM')) {
  setGeneric('pc2DTM', function(x, ...)
    standardGeneric('pc2DTM'))
}

#'@name pc2DTM
#'@title calcualte and post-classifies the morphological structure of raw tree crowns
#'
#'@description
#' calcualte and post-classifies the morphological structure of raw tree crowns
#'
#'@usage pc2DTM(runDir,currentP,allP)
#'
#'@author Chris Reudenbach
#'
#'
#'@param gridsize  resolution of the DTM raster
#'@param path_lastools directory for the windows lastools
#'@param cores number of cores that will be used
#'@param level_max number ob spline iterations



#'@return pc2DTM basically returns a DEM and DSM
#'
#'
#'@export pc2DTM
#'

pc2DTM <- function(level_max = 3 , gridsize=0.5, path_lastools = "~/apps/LAStools/bin",cores = 3) {

#level_max <- 7
#####
# set lastool folder
path_lastools <- path.expand(path_lastools)

if (Sys.info()["sysname"] == "Windows") {
  cmd <- path_lastools
} else {
  cmd <- paste("wine ",path_lastools)
}


# specify temporary directory for tiles
path_run <- paste0(path_lastools,"/tmp")
path_lasdata <- paste0(path_lastools,"/input")
dir.create(path_run)
dir.create(path_lasdata)
setwd(path_run)


### reduce data amount
  system(paste0("wine ",path_lastools,"/las2las-cli.exe",
                " -i ",path_lasdata,"/export*.laz ",
                " -odix _red2 -olaz",
                " -keep_class 2",
                " -thin_with_grid .5"
  ))
 

# merge all files
system(paste0("wine ",path_lastools,"/lasmerge-cli.exe",
              " -i ",path_lasdata,"/export*_red2.laz",
              " -o ",path_lasdata,"/out.las"),
       intern=TRUE
)


#### starting lastools classification 
# run lasground 
system(paste0("wine ",path_lastools,"/lasground_new-cli.exe",
              " -i ",path_lasdata,"/out.las",
              " -all_returns ",
              " -bulge 1.5",
              " -skip_files",
              " -city ",
              " -ultra_fine",
              " -odix g -olas",
              " -cores ",cores
              ),
        intern=TRUE
      )
# create lastools  DTM
ret <- system(paste0("wine ",path_lastools,"/las2dem-cli.exe",
                     " -i ",path_lasdata,"/*g.las",
                     " -keep_class 2",
                     " -extra_pass",
                     " -step ",gridsize,
                     " -ocut 3 ",
                     " -odix _dtm -otif",
                     " -cores ",cores),
              intern =TRUE
           )


#### starting SAGA classification
# create output mask file for interpolation
r <- raster::raster(paste0(path_lasdata,"/o_dtm.tif"))
r[r > 0] <- 1
raster::writeRaster(r,filename = paste0(path_lasdata,"/o_dtm.tif"),overwrite = TRUE)
gdalUtils::gdalwarp(paste0(path_lasdata,"/o_dtm.tif"), 
                    paste0(path_lasdata,"/lasdtm.sdat"), 
                    overwrite = TRUE,  
                    of = 'SAGA',
                    verbose = FALSE) 


# export las file to text file
system(paste0("wine ",path_lastools,"/las2txt.exe",
              " -i ",path_lasdata,"/*g.las",
              " -parse xyzrRGB",
              " -sep komma"),
       intern =TRUE
)


# import to saga as point cloud
ret <- system(paste0(sagaCmd,' io_shapes 16 ',
                     ' -POINTS ', path_lasdata,'/pointcloud',
                     ' -FILE  ', path_lasdata,'/outg.txt',
                     ' -XFIELD 1',
                     ' -YFIELD 2',
                     ' -FIELDS "4;5;6;7"',
                     ' -FIELDNAMES "r;R;G;B"',
                     ' -FIELDTYPES "1;1;1;1"',
                     ' -SKIP_HEADER 0',
                     ' -FIELDSEP 2'))


# saga spline interpolation of the alt values
ret <- system(paste0(sagaCmd,' grid_spline 4 ',
                             ' -SHAPES ', path_lasdata,'/pointcloud.spc',
                             ' -FIELD 2',
                             ' -TARGET_DEFINITION 0',
                             ' -TARGET_OUT_GRID ',paste0(path_lasdata,"/lasdtm.sdat"),
                             ' -TARGET_USER_SIZE ',gridsize,
                             ' -METHOD 1',
                             ' -EPSILON 0.000100',
                             ' -LEVEL_MAX ',level_max))

}