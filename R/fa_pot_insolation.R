if (!isGeneric('potInsolation')) {
  setGeneric('potInsolation', function(x, ...)
    standardGeneric('potInsolation'))
}

#'@name potInsolation
#'@title calculate statitiscs of polygon based raster extraction
#'
#'@description
#' calculate statitiscs of polygon based raster extraction
#'
#'@author Chris Reudenbach
#'
#'@param x  spatial raster object
#'@param svf_radius search radius in map units default is 10000.0, set to zero for ignoring,
#'@param sfv_method default is 0 (multiscale) NOTE much slower than 1 (sectors), 
#'@param svf_dlevel  multi scale factor default is 3.0 minimum is 1.25
#'@param sfv_ndirs number of sectors default is 8.0 minimum is 3.0
#'@param pi_solarconst (W/m**2) default is 1367.0, 
#'@param pi_units  units for output radiation values. default is 0 (kWh/m**2) available are: 1 (kJ/m**2) 2 J/cm**2, 
#'@param pi_shadow default is 1 (fat), choose 0 (slim) to trace grid node's shadow, 1 (fat) to trace the whole cell's shadow, or 2 (none) to ignore shadowing effects. The first is slightly faster but might show some artifacts
#'@param pi_day = "21/06/17",
#'@param pi_day_stop = "21/06/17", 
#'@param pi_day_step = 1
#'@param pi_hour_range_min = 0.0, 
#'@param pi_hour_range_max = 24.0,
#'@param pi_hour_step = 1.0, 
#'@param pi_lumped = 70.0

#'@return returns all calculations as SAGA Grid for statistical analysis
#'
#'
#'@export potInsolation
#'@examples
#'\dontrun{
#' # potential insolation based on a CHM
#' pot_insolation <- potInsolation("chm")
#'}
#'
potInsolation <- function(x = NULL,
                           svf_radius=10000.000000, 
                           sfv_method=0, 
                           svf_dlevel=3.000000, 
                           sfv_ndirs=8,
                           pi_solarconst = 1367.000000, 
                           pi_units = 0, 
                           pi_shadow = 1 ,
                           pi_day = "21/06/17", 
                           pi_day_stop = "21/06/17", 
                           pi_day_step = 1, 
                           pi_hour_range_min = 0.000000, 
                           pi_hour_range_max = 24.000000,
                           pi_hour_step = 0.500000, 
                           pi_lumped = 70.000000 )   {
  
  cat(":: run pot solar analysis...\n")
  
  
  
  ret <-  system(paste0(sagaCmd, " saga_cmd ta_lighting 3 ",
                        " -DEM ",path_run,x,".sgrd",
                        " -SFV ",path_run,"sfv.sgrd",
                        " -SIMPLE NULL",
                        " -TERRAIN NULL",
                        " -DISTANCE NULL",
                        " -RADIUS ",sfv_radius, 
                        " -METHOD ",sfv_method, 
                        " -DLEVEL ",sfv_dlevel,
                        " -NDIRS ",sfv_ndirs))
  

  ret <-  system(paste0(sagaCmd, " saga_cmd ta_lighting 2  ",
                        " -GRD_DEM ",path_run,x,".sgrd",
                        " -GRD_SVF ",path_run,"sfv.sgrd", 
                        " -GRD_DIRECT ",path_run,"dir.sgrd", 
                        " -GRD_DIFFUS ",path_run,"dis.sgrd", 
                        " -GRD_TOTAL ",path_run,"tot.sgrd", 
                        " -GRD_RATIO ",path_run,"rat.sgrd",                           
                        " -GRD_DURATION ",path_run,"dur.sgrd", 
                        " -GRD_SUNRISE ",path_run,"ris.sgrd", 
                        " -GRD_SUNSET ", path_run,"set.sgrd",
                        " -SOLARCONST ",as.character(pi_solarconst),
                        " -LOCALSVF 0",
                        " -UNITS ",as.character(pi_units),
                        " -SHADOW ",as.character(pi_shadow),
                        " -LOCATION 1", 
                        " -PERIOD 2",
                        " -DAY ",pi_day, 
                        " -DAY_STOP ",pi_day_stop, 
                        " -DAYS_STEP ",as.character(pi_day_step),
                        " -HOUR_RANGE_MIN ",as.character(pi_hour_range_min),
                        " -HOUR_RANGE_MAX ",as.character(pi_hour_range_max),
                        " -HOUR_STEP ",as.character(pi_hour_step), 
                        " -METHOD 2", 
                        " -LUMPED ",as.character(pi_lumped) ))


}