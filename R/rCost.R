#'@name rcost
#'@title Calculate the least cost path from point to point using a given friction matrix
#'@description
#' http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description
#'
#'@usage rcost(pointList,costraster)
#'@author Chris Reudenbach
#'
#'
#'@param pointList containing coords
#'@param a cost raster object
#'
#'@return rcost returns matrix cost/transformed real distances
#'
#'@export rcost
#'@examples
#'#### calculate least cost pathes
#'
#'       costMatrix<-rcostance(pointList,dem)


rcost<- function(pointList, costraster, plots=TRUE){
  library(gdistance)
  # costpath is pretty time consuming so we construct a max search distance from the dsistance matrix + the dom threshold
  dist<-as.data.frame(pointDistance(pointList,pointList,lonlat=TRUE,allpairs=TRUE))
  min.dist<-  max(apply(dist,1,min),na.rm=TRUE) #+domthres
  # caluclate transition raster
  tr=transition(costraster, transitionFunction=function(x){1}, directions=16)
  # correct it by geometry
  tr=geoCorrection(tr)

  if (plots==TRUE){plot(tr)}

  # generate a df dor the resultsinternalCost
  cost<-data.frame()
  # do peak by peak
  print("Starting costPath have a look at the Plots panel and be happy...")
  for (i in 1: nrow(pointList)){
    # set startpoint of first list (external list)
    start=pointList[i,]
    for (j in 1: nrow(pointList)){
      # with this point start costpath loop if the target peak distance is less then mindist
      if (dist[i,j] <= min.dist){
        # set target peak as end point (from DEMpeaklist)
        end=pointList[j,]

        # calculate the costpath
        costpath=shortestPath(tr, start@coords, end@coords, output="SpatialLines")
        # because you are bored plot it
        if (plots==TRUE){

          lines(costpath)}
        # get the values of each path
        tmp<-extract(costraster, costpath)
        tmp.sum <- lapply(tmp, function(i) {
          # get sum inverted altitude
          val.sum <- sum(i)
          return(val.sum)
        })
        # put the result in the cost matrix
        cost[i,j] = tmp.sum
      }else{cost[i,j]<-NA}
    }}
  # filter for min Value
  cost.min<-apply(cost,1,min)
  # take the original df
  ##ep<-as.data.frame(ext.peaklist)
  ##dp<-as.data.frame(dem.peaklist)
  #colnames(ep)<-c('xcoord', 'ycoord','name', 'altitude')
  # and merge them with using the min cost results
  ##newdp<-dp[apply(cost,1,which.min),]
  ##newdp$name<-ep$Name
  return(cost.min)
  #return(newdp)
}


