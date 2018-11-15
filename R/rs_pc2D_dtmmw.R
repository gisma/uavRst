
#' Create a Digital Terrain Model from UAV generated point clouds by minimum altitude sampling (moving window)
#'
#'@description
#' Create a Digital Terrain Model from a high density point cloud as typically derived by an optical UAV retrieval. Due to the poor estimation of ground points
#' a minimum samopling approach is applied. It retrieves on a coarse sampling gridsize the minimum value and interpolates on these samples a surface grid with a higher target
#' resolution. this is a kind of an try and error process and provides fairly good results if the point cloud shows at least some real surface points on a not to coarse grid.
#'
#'@author Chris Reudenbach, Finn Möller
#'
#'@param laspcFile  character. default is \code{NULL} path  to the laz/las file(s)
#'@param gisdbasePath character. default is \code{NULL} root directory of the project. NOTE the function creates two subfolder named \code{run} and \code{output}
#'@param sampleGridSize  numeric, resolution extraction raster
#'@param sampleMethod  character. sampling method of r.in.lidar Statistic to use for raster values Options: n, min, max, range, sum, mean, stddev, variance, coeff_var, median, percentile, skewness, trimmean Default: mean
#'@param winRes  list of moving window resolution for optimize Ground model
#'@param targetGridSize numeric. the resolution of the target DTM raster
#'@param splineThresGridSize numeric. threshold of minimum gridsize tha is used for splininterpolation if the desired resolution is finer a two step approximation is choosen
#'first step spline interpolation using the treshold gridsize second step bilinear resampling to the desired targetGridSize.
#'@param tension  numeric. tension of spline interpolation.
#'@param proj4  character. valid proj4 string that should be assumingly the correct one
#'@param giLinks list of link2GI cli pathes, default is NULL
#'@param projFolder subfolders that will be created/linked for R related GRASS processing
#'@param verbose to be quiet (1)
#'@param cutExtent clip area
#'@param grassVersion numeric. version of GRASS as derived by findGRASS() default is 1 (=oldest/only version) please note GRASS version later than 7.4 is not working with r.inlidar
#'@param searchPath path to look for grass
#'@export
#'@examples
#'\dontrun{
#'
#' require(uavRst)
#' require(link2GI)
#'
#' # create and check the links to the GI software
#' giLinks<-list()
#' giLinks$grass<-link2GI::linkGRASS7(returnPaths = TRUE)
#' if (giLinks$grass$exist) {
#'
#' # proj subfolders
#' projRootDir<-tempdir()

#' paths<-link2GI::initProj(projRootDir = tempdir(),
#'                          projFolders = c("data/","data/ref/","output/","run/","las/"),
#'                          global = TRUE,
#'                          path_prefix = "path_")

#' # get the data
#' utils::download.file(url="https://github.com/gisma/gismaData/raw/master/uavRst/data/lidar.las",
#' destfile="lasdata.las")
#'
#' # create 2D point cloud DTM
#' dtm <- pc2D_dtm(laspcFile = "lasdata.las",
#'                 gisdbasePath = tempdir(),
#'                 tension = 20 ,
#'                 sampleGridSize = 25,
#'                 targetGridSize = 0.5,
#'                 giLinks = giLinks)
#'  raster::plot(dtm)
#'}
#'}
pc2D_dtm <- function(laspcFile = NULL,
                       gisdbasePath = projRootDir,
                       grassVersion=1,
                       searchPath =NULL,
                       tension = 30 ,
                       sampleMethod="min",
                       cutExtent = NULL,
                       sampleGridSize=50,
                       winRes = c(100,50,25),
                       # beliebige Anzahl an "Suchfenstergrößen" und beliebige
                       # Werte. Nur mit Kommastellen kann diese Version noch
                       # nicht um. lässt sich aber einbauen, hab ich nur
                       # entfernt weil ich es nicht gebraucht habe
                       targetGridSize = 0.05,
                       splineThresGridSize = 0.5,
                       projFolder = NULL,
                       proj4 = "+proj=utm +zone=32 +datum=WGS84 +units=m +no_defs +ellps=WGS84",
                       giLinks =giLinks,
                       verbose = FALSE) {


  ## Bis Zeile 69 ist das eins zu eins von dir

  gdal <- link2GI::linkGDAL()
  if (is.null(searchPath)){
    if(Sys.info()["sysname"]=="Windows") searchPath="C:"
    else searchPath <- "/usr"}
  # if (is.null(giLinks)){
  #   giLinks <- linkAll()
  # }

  #saga <- giLinks$saga
  #otb <- giLinks$otb
  #sagaCmd<-saga$sagaCmd
  #path_OTB <- otb$pathOTB

  if (!verbose){
    GV <- Sys.getenv("GRASS_VERBOSE")
    Sys.setenv("GRASS_VERBOSE"=0)
    ois <- rgrass7::get.ignore.stderrOption()
    rgrass7::set.ignore.stderrOption(TRUE)}

  if (is.null(projFolder)) projFolder <-  c("data/","output/","run/","las/")

  # get/map the las binary folder and create the base command line
  if (is.null(laspcFile)) stop("no directory containing las/laz files provided...\n")
  else laspcFile <- path.expand(laspcFile)
  name<-basename(laspcFile)

  # create project structure and export global paths
  if (!nchar(Sys.getenv("GISDBASE")) > 0 ){
    link2GI::initProj(projRootDir = tempdir() ,
                      projFolders =  projFolder)
  }



  if (!file.exists(paste0(path_run,name)))
    cat(":: create copy of the las file at the working directory... \n")
  file.copy(from = laspcFile,
            to = paste0(path_run,name),
            overwrite = TRUE)



  las<-rlas::read.lasheader(laspcFile)
  sp_param <- c(as.character(las$`Min X`),as.character(las$`Min Y`),as.character(las$`Max X`),as.character(las$`Max Y`))
  # rename output file according to the extent
  fn<- paste(sp_param ,collapse=" ")
  tmp <- gsub(paste(sp_param ,collapse=" "),pattern = " ",replacement = "_")
  name<-paste0(gsub(tmp,pattern = "[.]",replacement = "_"),".las")
  file.rename(from =paste0(path_run,basename(laspcFile)),
              to = paste0(path_run,name))

  # copy it to the output folder
  sp_param[5] <- proj4


  ## der erste Schritt erstellt für jede gewählte Auflösung erstmal eine
  ## vectormap und speichert die in die "vdtms" Liste. Das Erstellen von der
  ## vectormap läuft über deinen Weg, sprich r.in.lidar zum einlesen (erstellen
  ## eines minimum rasters) und dann r.to.vect zum umwandeln in eine
  ## Punkte/vectormap.


  vdtms <- list()

  for (i in  winRes) {
    link2GI::linkGRASS7(gisdbase = projRootDir,
                        location = "pc2D_dtm",
                        spatial_params = sp_param,
                        resolution = i,
                        returnPaths = FALSE,
                        quiet = TRUE,
                        ver_select = grassVersion,
                        search_path = searchPath)

    # cat(":: sampling minimum altitudes using : ", sampleGridSize ,"meter grid size\n")
#    if (!grepl(system("g.extension -l",ignore.stdout = TRUE),pattern = "r.in.lidar"))
      # ret <- rgrass7::execGRASS("r.in.pdal",
      #                           flags  = c("overwrite","quiet"),
      #                           input  = paste0(path_run,name),
      #                           output = paste0("dem",i),
      #                           method = "min",
      #                           proj_in = sp_param[5],
      #                           resolution = as.numeric(i),
      #                           intern = TRUE,
      #                           ignore.stderr = TRUE
      # )
    # else
    ret <- rgrass7::execGRASS("r.in.lidar",
                              flags  = c("overwrite","quiet","o","e","n"),
                              input  = paste0(path_run,name),
                              output = paste0("dem",i),
                              method = "min",
                              resolution = i,
                              # trim = 49,
                              intern = TRUE,
                              ignore.stderr = TRUE
    )

    ret <- rgrass7::execGRASS("r.to.vect",
                              flags  = c("overwrite","quiet","v", "z"),
                              input  = paste0("dem",i),
                              output = paste0("vdtm",i),
                              type = "point",
                              intern = TRUE,
                              ignore.stderr = TRUE
    )

    ## Hier werden die erstellen vectormaps in die vdtms liste via
    ## rgrass7::readVECT gespeichert um sie in R verfügbar zumachen und die
    ## Grass logic zu verlassen.
    vdtms[paste0("vdtm",i)] <- rgrass7::readVECT(paste0("vdtm",i))
  }

  ## Der nächste Schritt geht Punkt für Punkt die jeweilige Vectormap durch und
  #sucht innerhalb eines Buffers nach Punkten, # die alle Kriterien erfüllen und
  #dann als "keeps" ausgewählt werden. Die jeweilige Vectormap ist in Runde eins
  #die vectormap # des größten Suchfensters, und ab Runde zwei die Vectormap der
  #"keeps" Punkte. erstellt leeren keeps vector der von runde zu runde neu
  #gefüllt wird vdtms_edit <- vdtms
  for (k in c(1:(length(winRes)-1))) {
    # hier wird festgelegt das er alle Suchfenstergrößen bis auf die letzte
    # durch geht.

    ## der if Schalter sorgt nur dafür das er in der ersten Runde wenn keeps
    ## noch leer ist die orgiginal Vectormap des größte Suchfensterns als
    ## "vdtm_run" nutzt. Sobald keeps gefüllt ist wird werden diese Punkte als
    ## "vdtm_run" genutzt.
    ##
    if (length(keeps) > 1) {
      vdtm_run <- keeps
    } else if (length(keeps) < 1) {
      vdtm_run <-vdtms[paste0("vdtm",winRes[k] )]
      vdtm_run <- vdtm_run[[1]]
    }


    keeps <- vdtm_run[1,]
    ## hier wird keeps mit dem ersten Punkt aus der ersten Vectormap gefüllt
    ## damit keeps beim späteren rbing nicht NULL ist und rausfliegt. Da werde
    ## ich mir bei Zeit nochmal was klügeres einfallen lasssen aber da
    ## vermutlich kein Untersuchungsgebiet so am Rand liegt hab ich das vorerst
    ## ignoriert.

    for (o in c(1:length(vdtm_run))) {
      ## jeder Punkt des jweiligen vdtm_run soll nun einmal durch gespielt werden

      buffer <- rgeos::gBuffer(vdtm_run[o,], capStyle= "SQUARE", width = winRes[k]/2)
      ## erstellt rechteckigen Buffer pro Punkt jeweils
      ## halb so groß wie die Suchfenstergröße im entsprechenden durchlauf
      ## Da werde ich auch noch mal bei Zeit prüfen ob man das variable gestaltet
      ## und als Funktionsparameter einfügt. Mal sehen wie sich das auswirkt.

      vdtm_run_match <- vdtms[paste0("vdtm",winRes[k+1] )]
      ## vtdm_run_match wählt die vectormap des nächst kleineren Suchfensters aus.

      hits <- which(rgeos::gContains(buffer,vdtm_run_match[[1]] , byid=TRUE))
      ## Und nun werden alle Punkte der nächst kleineren Punktewolke die im Buffer liegen
      ## ausgewählt und als "hits" abgespeichert.

      #Einmal die kommende Zeile in einzelne Schritte aufgedröselt:
      # In "vdtm_run_match[[1]]" sollen alle Punkte die folgendem
      # Kriterium entsprechen ausgewählt und als keeps_new gespeichert
      # werden:
      # Kriterium teil eins:
      # hits[which( abs(vdtm_run[o,]@coords[,3] -
      # vdtm_run_match[[1]][c(hits),]@coords[,3])
      ## Innnerhalb der "hits" werden die Z Koordinaten von
      # vdtm_run_match[[1]][c(hits),]@coords[,3] (Höhe)
      # von denen der vdtm_run[o,]@coords[,3] abgezogen,
      ## sprich die Höhenunterschiede zwischen den Punkten
      # der unterschiedlichen Auflösungen werden ermittelt.
      # Kriterium teil zwei: < ((winRes[k]- winRes[k+1])/5))
      ## Wenn die Höhenunterschiede kleiner als ein fünftel
      # der differenz zwischen den beiden Suchfenstergrößen liegt
      # werden sie als gültig erachtet
      ## Bsp. winRes[100] - winRes[50] = 50
      ##      50 / 5 = 10
      ## sprich innerhalb des 100x100 Suchfensters lasse ich zu das Punkte
      # die 10 meter höher als der referenzpunkt liegen übernommen werden.
      ## auch hier würde ich noch einbauen das man das variable gestaltet
      # und als Funktionsparameter einbaut. Dann kann man das ans
      # Gelände anpassen,
      ## in meinem Fall hat das am Hang so ganz gut gepasst.


      keeps_new <- vdtm_run_match[[1]][hits[which( abs(vdtm_run[o,]@coords[,3] - vdtm_run_match[[1]][c(hits),]@coords[,3]) < ((winRes[k]- winRes[k+1])/5))],]


      keeps <-  rbind(keeps, keeps_new)
      ## Dann werden alte mit neuen keeps verbunden und ab in den
      # nächsten durchlauf.
      ## Ggf würde ich hier auch noch testen ob es sinniger ist die
      # alten keeps raus zu werfen und nur die neuen zu übernehmen.
    }
  }

  # rm(keeps)
  # par(mfcol=c(1,5))
  # plot(vdtm_run[[1]],  col="black")
  # plot(vdtm_run[[1]], col="black")
  # plot(vdtm_run_match[[1]], col= "lightgrey", add=TRUE)
  # plot(vdtm_run[[1]], col="black")
  # plot(vdtm_run_match[[1]], col= "lightgrey", add=TRUE)
  # plot(keeps, add=TRUE, col="red")
  # plot(keeps, col="red")
  # raster::plot(dtm, axes=FALSE, box=FALSE, legend= FALSE)
  # plot(keeps_new, add=TRUE)


  # wenn das für jede Fenstergröße fertig wird die letzte Vectormap
  # abgespeichert und via "v.in.ascii" wieder in die GRASS umgebung geladen.
  utils::write.csv(data.frame(as.numeric(keeps@coords[,1]),as.numeric(keeps@coords[,2]),as.numeric(keeps@coords[,3])), paste0(path_run, "vec"), sep = " ", col.names = FALSE, row.names = FALSE)
  ret <- rgrass7::execGRASS("v.in.ascii",
                            flags  = c("quiet", "overwrite"),
                            input= paste0(path_run, "vec"),
                            output= "ascii",
                            separator = "comma",
                            skip = 1,
                            z=3,
                            intern = TRUE,
                            ignore.stderr = TRUE
  )






  ## Ab hier geht es denn so weiter wie du es erstellt hast
  ## nur halt mit der veränderten Vectormap.


  # set regio with new gridsize
  if (targetGridSize < splineThresGridSize) {
    oldtgs<-targetGridSize
    targetGridSize <- splineThresGridSize
    cat(":: target grid size is", targetGridSize ," => setting grid size to: ",splineThresGridSize,"\n")
  } else {
    splineThresGridSize <- targetGridSize
    oldtgs<-targetGridSize
  }

  ret <- rgrass7::execGRASS("g.region",
                            flags  = c("quiet"),
                            res= as.character(1),
                            intern = TRUE,
                            ignore.stderr = TRUE
  )

  cat(":: create DTM by interpolation to a raster size of: ", targetGridSize ,"\n")
  ret <- rgrass7::execGRASS("v.surf.rst",
                            flags  = c("overwrite","quiet"),
                            input  = "ascii",
                            elevation = "tdtm",
                            tension = 10,
                            intern = TRUE,
                            ignore.stderr = TRUE
  )




  # apply mask for input data area
  ret <- rgrass7::execGRASS("r.mapcalc",
                            flags  = c("overwrite","quiet"),
                            expression= paste0('"dtm = if(',paste0("dem",sampleGridSize),' > 0  ,tdtm,0)"'),
                            intern = TRUE,
                            ignore.stderr = TRUE
  )



  dtm0<- raster::writeRaster(raster::raster(rgrass7::readRAST("dtm")),paste0(path_run,"dtm0"), overwrite=TRUE,format="GTiff")
  if (oldtgs < splineThresGridSize) {
    cat(":: Resample to a grid size of: ", targetGridSize ,"\n")
    res<-gdalUtils::gdalwarp(srcfile = paste0(path_run,"dtm0.tif"),
                             dstfile = paste0(path_run,"dtm.tif"),
                             tr=c(oldtgs,oldtgs),
                             r="bilinear",
                             overwrite = TRUE,
                             multi = TRUE)
    dtm <- raster::raster(paste0(path_run,"dtm.tif"))
  } else {
    dtm <- raster::raster(paste0(path_run,"dtm0.tif"))
  }

  if (!verbose)  {
    Sys.setenv("GRASS_VERBOSE"=GV)
    rgrass7::set.ignore.stderrOption(ois)
  }
  #return(list(dtm,dtmA,paste0(fn,".",extFN)))
  return(dtm)
}

