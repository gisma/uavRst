#'@name costMergePeaks
#'@title Trys to merge indenpendent peak positions to provide a reliable join using a cost analysis
#'@description  
#' http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description
#'
#'@usage costMergePeaks(dem.peaklist,ext.peaklist)
#'@author Chris Reudenbach 
#'
#'
#'@references \url{http://moc.environmentalinformatics-marburg.de/doku.php?id=courses:msc:advanced-gis:description}
#' 
#'@param dem.peaklist DEM derived peaklist containing coords and altitude
#'@param ext.peaklist external peaklist, containing coords name and altitude
#'@param dem DEM data as a raster object
#'
#'@return costMergePeaks returns one merged peaklist with the names of the external peaks and the coords and altitude from the DEM derived list
#'
#'@export costMergePeaks
#'@examples   
#'#### Example merges the two peak lists
#' note the ext.peaklist must contain at least valid coordinates and names
#' 
#'       new.peaklist<-costmergePeaks(dem.peaklist,ext.peaklist)


costMergePeaks<- function(dem.peaklist, ext.peaklist, costraster, domthres,plots=TRUE){
  # costpath is pretty time consuming so we construct a max search distance from the dsistance matrix + the dom threshold
  dist<-as.data.frame(pointDistance(ext.peaklist,dem.peaklist,lonlat=FALSE,allpairs=TRUE))
  min.dist<-  max(apply(dist,1,min),na.rm=TRUE)+domthres
  # incert the DEM to get a costraster by inverted altitudes
  costraster=(costraster*-1)+maxValue(setMinMax(costraster))
  if (plots==TRUE){plot(costraster)}
  
  # generate a df dor the results
  cost<-data.frame()
  # do peak by peak
  print("Starting costPath have a look at the Plots panel and be happy...")
  for (i in 1: nrow(ext.peaklist)){
    # set startpoint of first list (external list)
    start=ext.peaklist[i,]
    for (j in 1: nrow(dem.peaklist)){
      # with this point start costpath loop if the target peak distance is less then mindist
      if (dist[i,j] <= min.dist){
        # set target peak as end point (from DEMpeaklist)
        end=dem.peaklist[j,]
        # caluclate transition raster
        tr=transition(costraster, mean, directions=8)
        # correct it by geometry
        tr=geoCorrection(tr)
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
  ep<-as.data.frame(ext.peaklist)
  dp<-as.data.frame(dem.peaklist)
  #colnames(ep)<-c('xcoord', 'ycoord','name', 'altitude')
  # and merge them with using the min cost results
  newdp<-dp[apply(cost,1,which.min),]
  newdp$name<-ep$Name
  
  return(newdp)
} 


