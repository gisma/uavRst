#' digitizing vector features using leaflet draw
#'
#' @description  leafDraw is based on the leaflet draw plugin. It provides a bunch of leaflet maps as base layers for digitizing vector features. 
#'
#' @note Yu can either save the digitized object to a json file or you ma grab the json string via the clipboard
#' @param mapcenter c(lat,lon) central point of the leaflet map
#' @param zoom initial zoom level
#' @param line enable the draw tool line tool
#' @param poly enable the draw polygon tool 
#' @param circle enable the draw circle tool
#' @param point enable the draw point tool
#' @param remove enable/disable the remove feature of the draw tool
#' @param position place to put the toolbar (topright, topleft, bottomright, bottomleft)
#' @param intersection enable/disable th possibility to overlay lines or polygons
#' @param maplayer string as provided by leaflet-provider 
#' @param preset textstring "NULL" full draw version, "uav" for flightarea digitizing, "ext" for rectangles
#' @param overlay optional sp object 
#' 
#' @author
#' Chris Reudenbach
#'
#' @examples
#' 
#' # all features
#' leafDraw()
#' 
#' # preset for digitizing uav flight areas in Meuse
#' 
#' uavRst::leafDraw(overlay = over, preset = "uav")
#' 
#' data(meuse) 
#' coordinates(meuse) <- ~x+y 
#' proj4string(meuse) <-CRS("+init=epsg:28992") 
#' me<-sp::spTransform(meuse,CRSobj = sp::CRS("+init=epsg:4326"))
#'  
#' # preset for digitizing extents
#' leafDraw(preset="ext",overlay = me)
#' @export leafDraw
#'               

leafDraw <- function(mapCenter=c(50.80801,8.72993),
                     zoom=15, 
                     line = TRUE, 
                     rectangle = TRUE, 
                     poly = TRUE, 
                     circle = TRUE, 
                     point = TRUE,
                     remove = TRUE, 
                     position= "topright", 
                     maplayer=c("CartoDB.Positron","OpenStreetMap","Esri.WorldImagery","Thunderforest.Landscape","OpenTopoMap"),
                     overlay=NULL,
                     features=NULL,
                     preset = "all",
                     cex = 10,
                     lwd = 2,
                     alpha = 0.6,
                     opacity = 0.7) {
  
  # create tmp path
  tmpPath<- createTempDataTransfer()

    if (!is.null(overlay)){
      
      if (class(overlay)  %in% c("SpatialPointsDataFrame","SpatialLinesDataFrame","SpatialLines","SpatialPoints")) {
        e <- as(raster::extent(overlay), "SpatialPolygons")
        e <- sp::SpatialPolygonsDataFrame(e, data.frame(ID="overlay"))
        proj4string(e) <- sp::proj4string(overlay)
        overlay<-sp::spTransform(e,CRSobj = sp::CRS("+init=epsg:4326"))
      } else if  (class(overlay)=="SpatialPolygonsDataFrame") {
        overlay<-sp::spTransform(overlay,CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
        overlay <- sp::SpatialPolygonsDataFrame(overlay, data.frame(ID="overlay"))
      }
      
      rgdal::writeOGR(overlay, paste(tmpPath, "jsondata", sep=.Platform$file.sep), "OGRGeoJSON", driver="GeoJSON")
      
      # for fastet json read in a html document we wrap it with var data = {};
      # and we fix the crs item of ogr2json
      # TODO loop a list of data
      
      # main data object
      lns <- data.table::fread(paste(tmpPath, "jsondata", sep=.Platform$file.sep), header = FALSE, sep = "\n", data.table = FALSE)
      
      # do it for main
      lns[1,] <-paste0('var jsondata = {')
      lns[3,]<-paste0('"crs": { "type": "name", "properties": { "name": "EPSG:4326" } },')
      lns[length(lns[,1]),]<- '};'
      write.table(lns, paste(tmpPath, "jsondata", sep=.Platform$file.sep), sep="\n", row.names=FALSE, col.names=FALSE, quote = FALSE)
      features<-names(overlay)
      # correct if only Lines or Polygons (obsolete here?)
      if (class(overlay)[1] == 'SpatialPolygonsDataFrame' | class(overlay)[1] == 'SpatialPolygons'){
        noFeature <- length(overlay@polygons)
      } else if (class(overlay)[1] == 'SpatialLinesDataFrame' | class(overlay)[1] == 'SpatialLines'){
        noFeature <- length(overlay@lines)
      } 
      jsondata<-1
      
    
    
    mapCenter<-c(raster::extent(overlay)[3]+raster::extent(overlay)[4]-raster::extent(overlay)[3],raster::extent(overlay)[1]+raster::extent(overlay)[2]-raster::extent(overlay)[1])
    #features<-overlay
    
    }  else {jsondata<-0}
  
  if ( preset == "uav") {
    if (is.null(mapCenter)){
      mapCenter<-c(50.80801,8.72993)}
    else {
      mapCenter<-mapCenter
    }
    zoom<-15 
    line<-TRUE
    rectangle<-FALSE
    poly<-FALSE
    circle<-FALSE
    point<-FALSE
    remove<-TRUE
    maplayer=c("Esri.WorldImagery","OpenStreetMap","Thunderforest.Landscape","OpenTopoMap")
    overlay=overlay
  } 
  else if (preset == "ext") {
    if (is.null(mapCenter)){
      mapCenter<-c(50.80801,8.72993)}
    else {
      mapCenter<-mapCenter
    }
    zoom<-10
    line<-FALSE
    rectangle<-TRUE
    poly<-FALSE
    circle<-FALSE
    point<-FALSE
    remove<-FALSE   
    position<-"topright"
    maplayer=c("OpenStreetMap","CartoDB.Positron","Esri.WorldImagery","Thunderforest.Landscape","OpenTopoMap")
    overlay=NULL
  } else {
    mapCenter<-mapCenter
    zoom<-zoom
    line<-line
    maplayer=c("OpenStreetMap","CartoDB.Positron","Esri.WorldImagery","Thunderforest.Landscape","OpenTopoMap")
    overlay=overlay
    rectangle<-rectangle
    poly<-poly
    circle<-circle
    point<-point
    remove<-remove
    position<-position
  }

  
  ### create the rest of the JS strings
  CRSvarMapCenter<-paste0('var mapCenter = [',mapCenter[1],',',mapCenter[2],'];')
  CRSinitialZoom<-paste('var initialZoom = ',zoom,';')
  
  ### write it to CRS.js
  # assign tmpfilename for CRS definition
  tmpCRS <- paste0(tmpPath,"/crs.js")
  # write the proj4leaflet CRS
  write(CRSinitialZoom,tmpCRS,append = TRUE)
  write(CRSvarMapCenter,tmpCRS,append = TRUE)

  
  # create parameter list for the widget
  x <- list(data  = 'undefined',
            features=features,
            layer=maplayer,
            zoom = zoom,
            html = mapview:::getPopupStyle(),
            #refpoint=refpoint,
            line=line,
            rectangle=rectangle,
            poly=poly,
            circle=circle,
            point=point,
            remove=remove,
            position=position,
            scaleBar=TRUE,
            color=mapviewGetOption("raster.palette")(256),
            na.color=mapviewGetOption("na.color"),
            cex = 10,
            lwd = 2,
            alpha = 0.6,
            legend = FALSE,
            opacity = 0.7,
            overlay=jsondata
            
  )
  leafDrawInternal(tmpPath, x = x)  
}


# create dependencies
digiDependencies <- function(tmpPath) {
  
  data_dir <- paste0(tmpPath,sep=.Platform$file.sep)
  
  
  list(
    htmltools::htmlDependency(name = "crs",
                              version = "1",
                              src = c(file = tmpPath),
                              script = list("crs.js")),
    
    htmltools::htmlDependency(name = "jsondata",
                              version = "1",
                              src = c(file = tmpPath),
                              script = list("jsondata")),
    
    htmltools::htmlDependency(
      name = "leaflet-draw",
      version= "0.7.3",
      src = c(file = tmpPath),
      script = list("leaflet.draw.js"),
      stylesheet=list("leaflet.draw.css")
    )
    
  )
}

###  creates temporary file structure for data transfer =================================================
createTempDataTransfer <- function (){
  tmpPath <- tempfile(pattern="007")
  dir.create(tmpPath)
  return(tmpPath)
}

leafDrawInternal <- function(tmpPath, x = NULL) {
  deps<-digiDependencies(tmpPath) 
  sizing = htmlwidgets::sizingPolicy(
    browser.fill = TRUE,
    viewer.fill = TRUE,
    viewer.padding = 5
  )
  # create widget
  htmlwidgets::createWidget(
    name = 'leafDraw',
    x,
    dependencies = deps,
    sizingPolicy = sizing,
    package = 'uavRst'
  )
}

### Widget output function for use in Shiny =================================================
#
leafDrawOutput <- function(outputId, width = '100%', height = '800px') {
  htmlwidgets::shinyWidgetOutput(outputId, 'leafDraw', width, height, package = 'uavRst')
}

### Widget render function for use in Shiny =================================================
#
renderleafDraw<- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) {
    expr <- substitute(expr)
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, projViewOutput, env, quoted = TRUE)
}