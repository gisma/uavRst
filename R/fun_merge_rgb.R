#' merge single images
#' @description  merge single geocoded images to on big image
#' @param files list of filenames to be merged
#' @param proj4 valid proj4 string 
#' @param output valid path and name for the output image
#' @param ext valid extent of the the area that is requested
#' @details https://stackoverflow.com/questions/15876591/merging-multiple-rasters-in-r
#'     with regards to Flo Detsch who asked it the right way
#' @note if proj4 is NULL no reprojection is performed otherwise the merged raster is projected by using the proj4 string.      
#' @return The merged image as a raster object 
#' # example
#' # tif_files is a list of valid filenames
#' merged_r = merge_rgb(files = tif_files,
#'                      out = file.path(tempdir(),"out.tif"))
#' @export merge_rgb                     
#' 

merge_rgb = function(files=NULL,proj4=NULL,output=NULL,cropoutput=NULL,ext=NULL){
  # create a list for the files to be merged
  mofimg=list()
  # get new filelist
  
  if (is.null(files) || is.null(output)) return("Input/Output arguments are not valid\n")
  
  # stacking all files and put them in the list object
  cat("stack files...\n")
  for (f in 1:length(files)){
    mofimg[[f]] = stack(files[f])
  }
  
  # setting the merging parameters

  if(!is.null(ext)){
    mofimg$tolerance = 1
    mofimg$overwrite = TRUE
    mofimg$filename  = cropoutput
    mofimg$ext = ext
    
    cat(getCrayon()[[3]]("merge and crop files - this will take quite a while \n"))
  } else {
    mofimg$tolerance = 1
    mofimg$filename  = output
    mofimg$overwrite = TRUE
    mofimg$ext = ext
    cat(getCrayon()[[3]]("merge files - this will take quite a while \n"))
  }
  r = do.call(raster::merge, mofimg)
  
  # reproject it 
  if (!is.null(proj)){
    cat(getCrayon()[[3]]("Projecting image... this will take even longer\n"))
    r = raster::projectRaster(r,crs = proj4)}
  
  return(r)
}