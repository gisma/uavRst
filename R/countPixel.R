#' trains model according to trainingdata and image stacks
#' 
#' @param ids ids
#' @param position position
#' @param  imageFiles image files
#' @param out_prefix out prefix string
#' @param ext extension
#' @param path   output path
#' @param dropChars chars to drop
#' 
#' @export getCounts
#' @examples  
#' df1<-getCounts(position = position
#' imageFiles = imageFiles
#' dropChars = 8
#' pre=pre
#' ext=".tif")

getCounts<- function(ids=c(1,2),
                     position=NULL,
                     imageFiles = NULL,
                     buffersize=1.5,
                     out_prefix="classified_index_",
                     ext=".tif",
                     path = path_output,
                     dropChars=0) {
  
  buffers<-rgeos::gBuffer(position,width=buffersize) 
  ex<-data.frame()
  df<- lapply(seq(1:length(position)), function(i) {
    fn<-paste0(path,out_prefix,substr(position[i,]$tree,1,nchar(position[i,]$tree)-dropChars),ext)
    
    if (file.exists(fn)){
      ex <- as.data.frame(unlist(raster::extract(raster(fn) , position[i,]    , buffer=buffersize, df=TRUE)))
    
    idVal<-as.numeric(vector(length = length(ids)))
    for (j in 1:length(ids)){
      idVal[j] <- sum(ex[ex[2] == ids[j],  2])/j
      #nogreen <- table(ex[ex[2] == id2,  ])
    }}
    #  all=nogreen+green  
    #  gr=green/all
    #  no=nogreen/all
   
   return(c(idVal,basename(fn)))
  #return(c("green"=gr,"nogreen"=no,"all"=all, "plot"=basename(imageFiles[i])))
  }
  )
  result <- do.call("rbind", df)
  return(result)
}