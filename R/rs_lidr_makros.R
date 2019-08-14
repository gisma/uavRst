#' checks extension mismatch of a lasfile 
#' @param lasfiles filename of an las file
#' @param proj4 correct proj4 string
#' @export
correctLas<-function(lasfiles,
                     proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"){
  changed=FALSE
  
  cat("\n: checking extent of las file...",lasfiles,"\n")
  l1<- lidR::readLAS(lasfiles)
  sp::proj4string(l1) <-sp::CRS(proj4,doCheckCRSArgs=TRUE)
  if (l1@bbox[1]< l1@bbox[3]-1000.1) {
    cat(getCrayon()[[2]]("\n: corrected minx")," ",l1@bbox[1], "=>" ,l1@bbox[3]-1000)
    l1@bbox[1]<-l1@bbox[3]-1000
  }
  if (l1@bbox[2]<l1@bbox[4]-1000.1){
    cat(getCrayon()[[2]]("\n: corrected miny")," ",l1@bbox[2], "=>" ,l1@bbox[4]-1000)
    l1@bbox[2]<-l1@bbox[4]-1000
  }
  cat(": new extend... ")
  subset = lidR::lasclipRectangle(l1, l1@bbox[1],l1@bbox[3],l1@bbox[2],l1@bbox[4])
  cat(subset@bbox,"\n")
  sp::proj4string(subset) <-sp::CRS(proj4,doCheckCRSArgs=TRUE)
  return(subset)
} 


#' corrects the extent of a wrong 0,0 coordinate and adds a lidR lax file 
#' @param las_files filename of an las file
#' @param proj4 correct proj4 string
#' @param level0path path for saving the corrected level0 data
#' @export
llas2llv0<-function(las_files,
                    level0path,
                    proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
){
  for(fn in las_files){
    las<-uavRst::correctLas(fn,proj4 = proj4)
    sp::proj4string(las) <- sp::CRS(proj4,doCheckCRSArgs=TRUE)
    las = lidR::writeLAS(las,paste0(level0path,basename(fn)))
    rlas::writelax(paste0(level0path,basename(fn)))
  }
}

#' create lidR catalog

#' @param proj4 correct proj4 string
#' @param path path for saving the corrected level0 data
#' @param cores number of cores for parallel procession
#' @param chunksize chunksiz in map units
#' @param chunkbuffer buffersize of chunk in mapunits
#' @param alignment  <- value,
#' @param progress  <- value,
#' @param stop_early  <- value,
#' @param w2w  <- value,
#' @param output_files  <- value,

#' @param select  <- value,
#' @param filter  <- value                  
#' @export

make_lidr_catalog <- function(path = NULL,
                              cores = 3,
                              chunksize = 00,
                              chunkbuffer= 30,
                              alignment = c(0,0),
                              progress = TRUE,
                              stop_early =TRUE,
                              w2w =TRUE,
                              output_files="",
                              select="",
                              filter="",
                              proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
) {
  ## setting up the lidR catalog
  ctg <- lidR::catalog(path)
  sp::proj4string(ctg) <- sp::CRS(proj4) # ETRS89 / UTM zone 32N
  lidR::opt_cores(ctg) <- cores
  lidR::opt_chunk_size(ctg) = chunksize
  lidR::opt_chunk_buffer(ctg) <- chunkbuffer
  lidR::opt_chunk_alignment(ctg) <- alignment
  lidR::opt_progress(ctg) <- progress
  lidR::opt_stop_early(ctg) <- stop_early
  lidR::opt_wall_to_wall(ctg) <- w2w
  lidR::opt_output_files(ctg) <- output_files
  
  lidR::opt_select(ctg) <- select
  lidR::opt_filter(ctg) <- filter
  return(ctg)
}


#' clips a lidR las* or lascatalog object to a given shapefile
#' @description clips a lidR las* or lascatalog object to a given shapefile. if an outpath is provided it additionally writes the las and lax files into this folder
#' @param cgs filename of an las file
#' @param shapefile filename of an shapefile-mask
#' @param lasfilename filname of lasfile if written
#' @param outpath path to write to if null nothing is written
#' @param proj4 correct proj4 string
#' @export
cut_aoi<-function(cgs,
                  shapefile,
                  outpath=NULL,
                  lasfilename="aoi.las",
                  proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"){
  # Clip catalog to the area of interest retile and reconfigure the catalog
  aoi = raster::shapefile(shapefile)
  aio_bb = sp::bbox(aoi)
  aoicgs<- lidR::lasclipRectangle(cgs,
                                  xleft = aio_bb[1],
                                  ybottom = aio_bb[2],
                                  xright = aio_bb[3],
                                  ytop = aio_bb[4])
  if (!is.null(outpath)){
    cat("\n: saving: ",file.path(outpath,lasfilename))
    lidR::writeLAS(aoicgs,file.path(outpath,lasfilename))
    rlas::writelax(paste0(outpath,lasfilename))
    return(paste0(outpath,lasfilename))
  }
  else 
    
    return(aoicgs)
}

#' divide a lidR las* or lascatalog object by given factors
#' @description divide a lidR las* object by given factors. The resulting tiles are  written to the outpath folder
#' @param cgs filename of an las file
#' @param factorx factor to divide in x direction
#' @param factory factor to divide in y direction
#' @param lasfn filname of lasfile if written
#' @param outpath path to write to if null nothing is written
#' @param proj4 correct proj4 string
#' @export
#' @examples 
#' \dontrun{
#' cut_eq(cgs = lidR::readLAS(las_file),factorx = 4,factory = 4 )
#' }
tile_eq<-function(cgs,
                 factorx = 4,
                 factory = 4,
                 outpath=NULL,
                 lasfn="cut",
                 proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"){
  # Clip catalog to the area of interest retile and reconfigure the catalog
  aio_bb = sp::bbox(cgs)
  xext<-round(aio_bb[3]-aio_bb[1],0)/factorx
  yext<-round(aio_bb[4]-aio_bb[2],0)/factory
  cutlas<-list()
  ybottom<-aio_bb[2]+0
  xleft<-aio_bb[1]+0
  xright<-aio_bb[1]+xext
  ytop<-aio_bb[2]+yext
  oldxleft<-oldybottom<-oldxright<-oldytop<-0
  i=1
  for (k in seq(1,factory)){
    for (j in seq(1,factorx)){
      cutlas[[i]]<- lidR::lasclipRectangle(cgs,
                                           xleft = xleft,
                                           ybottom = ybottom,
                                           xright =xright,
                                           ytop = ytop)
      
      xleft<-xleft+xext
      xright<- xright +xext
      
      cat("\n: saving: ",paste0(outpath,lasfn,"_",i,".las"))
      lidR::writeLAS(cutlas[[i]],paste0(outpath,lasfn,"_",i,".las"))
      rlas::writelax(paste0(outpath,lasfn,"_",i,".las"))
      i=i+1
    }
    ytop<-ytop+yext
    ybottom<-ybottom+yext
    xleft<-aio_bb[1]+0
    xright<-aio_bb[1]+xext
  }
}