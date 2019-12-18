

#' divide a lidR las* or lascatalog object by given factors
#' @description divide a lidR las* object by given factors. The resulting tiles are  written to the outpath folder
#' @param cgs filename of an las file
#' @param factor_x factor to divide in x direction
#' @param factor_y factor to divide in y direction
#' @param las_fn filname of lasfile if written
#' @param outpath path to write to if null nothing is written
#' @param proj4 correct proj4 string
#' @export
#' @examples 
#' \dontrun{
#' cut_eq(cgs = lidR::readLAS(las_file),factor_x = 4,factor_y = 4 )
#' }
tile_eq<-function(cgs,
                 factor_x = 4,
                 factor_y = 4,
                 outpath=NULL,
                 las_fn="cut",
                 proj4 = "+init=epsg:32632"){
  # Clip catalog to the area of interest retile and reconfigure the catalog
  aio_bb = sp::bbox(cgs)
  xext<-round(aio_bb[3]-aio_bb[1],0)/factor_x
  yext<-round(aio_bb[4]-aio_bb[2],0)/factor_y
  cutlas<-list()
  ybottom<-aio_bb[2]+0
  xleft<-aio_bb[1]+0
  xright<-aio_bb[1]+xext
  ytop<-aio_bb[2]+yext
  oldxleft<-oldybottom<-oldxright<-oldytop<-0
  i=1
  for (k in seq(1,factor_y)){
    for (j in seq(1,factor_x)){
      cutlas[[i]]<- lidR::lasclipRectangle(cgs,
                                           xleft = xleft,
                                           ybottom = ybottom,
                                           xright =xright,
                                           ytop = ytop)
      
      xleft<-xleft+xext
      xright<- xright +xext
      
      message("\n: saving: ",paste0(outpath,las_fn,"_",i,".las"))
      lidR::writeLAS(cutlas[[i]],paste0(outpath,las_fn,"_",i,".las"))
      rlas::writelax(paste0(outpath,las_fn,"_",i,".las"))
      i=i+1
    }
    ytop<-ytop+yext
    ybottom<-ybottom+yext
    xleft<-aio_bb[1]+0
    xright<-aio_bb[1]+xext
  }
}