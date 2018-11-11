#' Easy digitizing of vector features within your rstudio session (or any browser) 
#' @author Chris Reudenbach
#' @description  digitize is based on the leaflet draw plugin. It provides a bunch of leaflet maps as base layers for digitizing vector features. 
#'
#' @note You can either save the digitized object to a \code{JSON} or \code{KML} file to your hard disk. As an alternative you may grab the \code{JSON} string via the clipboard.
#' @param mapCenter c(lat,lon) central point of the leaflet map
#' @param zoom initial zoom level
#' @param line enable the draw tool line tool
#' @param poly enable the draw polygon tool 
#' @param rectangle enable the draw polygon tool 
#' @param circle enable the draw circle tool
#' @param point enable the draw point tool
#' @param remove enable/disable the remove feature of the draw tool
#' @param position place to put the toolbar (topright, topleft, bottomright, bottomleft)

#' @param maplayer string. as provided by leaflet-provider 
#' @param preset character. defaut is "NULL" full draw version, "uav" for flightarea digitizing, "ext" for rectangles
#' @param locPreset character. default is "muf" for Marburg University Forest, others are "tra" Traddelstein, "hag" Hagenstein, "baw" Bayerwald.
#' @param overlay optional sp object 
#' @param features features 
#' @param cex cex 
#' @param lwd lwd
#' @param alpha alpha 
#' @param opacity opacity
#' @export 
#' @examples

#'\dontrun{
#' ##- libs
#' require(sp)
#' require(uavRst)
#' 
#' ##- preset for digitizing uav flight areas in Meuse
#' require(sp)
#' data(meuse) 
#' sp::coordinates(meuse) <- ~x+y 
#' sp::proj4string(meuse) <-sp::CRS("+init=epsg:28992") 
#' me<-sp::spTransform(meuse,CRSobj = sp::CRS("+init=epsg:4326"))
#' uavRst::digitize(overlay = me)
#'   
#' ##- preset for digitizing extents
#' uavRst::digitize(preset="ext",overlay = me)}
#'

digitize <- function(mapCenter=NULL,
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
                     locPreset = "muf",
                     cex = 10,
                     lwd = 2,
                     alpha = 0.6,
                     opacity = 0.7) {
  
  if (is.null(mapCenter)) {
    if ( locPreset == "muf") {
      mapCenter<-c(50.84,8.68)
    } else if (locPreset == "tra") {
      mapCenter<-c(51.13,8.97)  
    } else if (locPreset == "hag") {
      mapCenter<-c(51.16,8.90)  
    }else if (locPreset == "baw") {
      mapCenter<-c(48.92,13.40)  
    }
  }
  
  else {
    mapCenter<-mapCenter
  }
  
  # create tmp path
  tmpPath<- createTempDataTransfer()
  
  if (!is.null(overlay)){
    
    if (class(overlay) %in% c("SpatialPointsDataFrame","SpatialLinesDataFrame","SpatialLines","SpatialPoints")) {
      #e <- as(raster::extent(overlay), "SpatialPolygons")
      #e <- sp::SpatialPolygonsDataFrame(e, data.frame(ID="overlay"))
      sp::proj4string(overlay) <- sp::proj4string(overlay)
      overlay<-sp::spTransform(overlay,CRSobj = sp::CRS("+init=epsg:4326"))
    } else if  (class(overlay)  %in% c("SpatialPolygonsDataFrame")) {
      overlay<-sp::spTransform(overlay,CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +no_defs"))
      #overlay <- sp::SpatialPolygonsDataFrame(overlay, data.frame(ID="overlay"))
    }
    
    rgdal::writeOGR(overlay, paste(tmpPath, "jsondata", sep=.Platform$file.sep), "OGRGeoJSON", driver="GeoJSON")
    
    # for the fastest json read in a html document, we wrap it with var data = {};
    # and we fix the crs item of ogr2json
    # TODO loop a list of data
    
    # main data object
    lns <- data.table::fread(paste(tmpPath, "jsondata", sep=.Platform$file.sep), header = FALSE, sep = "\n", data.table = FALSE)
    
    # do it for main
    lns[1,] <-paste0('var jsondata = {')
    lns[3,]<-paste0('"crs": { "type": "name", "properties": { "name": "EPSG:4326" } },')
    lns[length(lns[,1]),]<- '};'
    utils::write.table(lns, paste(tmpPath, "jsondata", sep=.Platform$file.sep), sep="\n", row.names=FALSE, col.names=FALSE, quote = FALSE)
    features<-names(overlay)
    # correct if only Lines or Polygons (obsolete here?)
    if ((class(overlay)  %in% c("SpatialPolygonsDataFrame")) | (class(overlay)  %in% c("SpatialPolygons"))){
      noFeature <- length(overlay@polygons)
    } else if ((class(overlay)  %in% c('SpatialLinesDataFrame')) | (class(overlay)  %in% c('SpatialLines'))){
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
            html = getPopupStyle(),
            #refpoint=refpoint,
            line=line,
            rectangle=rectangle,
            poly=poly,
            circle=circle,
            point=point,
            remove=remove,
            position=position,
            scaleBar=TRUE,
            color=mapview::mapviewGetOption("raster.palette")(256),
            na.color=mapview::mapviewGetOption("na.color"),
            cex = cex,
            lwd = lwd,
            alpha = alpha,
            legend = FALSE,
            opacity = opacity,
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