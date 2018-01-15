#' calculate prediction performance statistics for classication models 
#' 
#' @description this function calculates prediction performance statistics
#' between vectors of predicted and observed values. Users may also create 
#' a dotplot visualising the results.
#' 
#' @param prd factor vector of predicted values with two levels
#' @param obs factor vector of observed values with two levels
#' @param prob optional. Predicted probabilities for the first class
#' @param plot logical, whether to produce a visualisation of the results.
#' Defaults to FALSE
#' 
#' @return
#'  \code{plot = FALSE} (the default), a data frame. 
#'  \code{plot = TRUE}, a list with components \code{stats} - data frame
#' and \code{plot} - a trellis plot object.
#' 
#' @author
#' Hanna Meyer and Tim Appelhans
#' 
#' @examples
#' #create two random vectors with classes "yes" and "no" to simulate a model
#' #with random performance. Expected POD and PFD  
#' pred_vals <- factor(sample(c("Yes","No"), 50, replace = TRUE),levels=c("Yes","No"))
#' obs_vals <- factor(sample(c("Yes","No"), 50, replace = TRUE),levels=c("Yes","No"))
#' 
#' result <- classicationStats(pred_vals, obs_vals, plot=TRUE)
#' result$plot
#' result$stats
#' 
#' @export classicationStats
#' @aliases classicationStats
#' @seealso \code{\link{regressionStats}}

classicationStats <- function(prd, obs, prob=NULL, plot=FALSE) {
  tab <- (table(prd,obs))/100
  TP <- tab[1,1]
  FP <- tab[1,2]
  TN <- tab[2,2]
  FN <- tab[2,1]
  
  bias <- (TP+FP)/(TP+FN)
  POD <- TP/(TP+FN)
  PFD <- FP/(FP+TN)
  FAR <- FP/(TP+FP)
  CSI <- TP/(TP+FP+FN)
  ph <- ((TP+FN)*(TP+FP))/(sum(tab))
  ETS <- (TP-ph)/((TP+FP+FN)-ph)
  HSS <- (TP*TN-FP*FN)/(((TP+FN)*(FN+TN)+(TP+FP)*(FP+TN))/2)
  HKD <- (TP/(TP+FN))-(FP/(FP+TN))
   (!is.null(prob)){
    require(pROC)
    AUC <- as.numeric(roc(obs,prob)$auc)
    df_all <- data.frame(bias,PFD,FAR,POD,CSI,ETS,HSS,HKD,AUC)
    names(df_all) <- c("Bias","PFD","FAR","POD","CSI","ETS","HSS","HKD","AUC")
  }else{
    df_all <- data.frame(bias,PFD,FAR,POD,CSI,ETS,HSS,HKD)
    names(df_all) <- c("Bias","PFD","FAR","POD","CSI","ETS","HSS","HKD")
  }
  
   (plot) {
    df_melt <- reshape2::melt(df_all)
    ## panel.fun modied from 
    ## http://thebiobucket.blogspot.de/2011/04/r-graphs-lattice-use-of-panel-functions.html
    panel.fun <- function(...) {
       (panel.number() == 1) { 
        at<-pretty(rng)
        panel.axis("top", at = at, outside = FALSE,
                   labels = TRUE, half = FALSE)
        panel.abline(v = 0, lty = 3, lwd = 1)
        panel.dotplot(..., lwd = 0.5)
      }
       (panel.number() == 2) {
        at <- pretty(c(0, 1))
        panel.axis("bottom", at = at, outside = FALSE,
                   labels = TRUE, half = FALSE)
        panel.lines(x = c(0, 0), y = c(0.75, 2.25), lty = 3, lwd = 1)
        panel.lines(x = c(1, 1), y = c(2.75, 7.25), lty = 3, lwd = 1)
        #panel.abline(v = 0, lty = 3, lwd = 1)
        panel.dotplot(..., lwd = 0.5)
      }
    }
    
    nms <- names(df_all)[c(-1)]
    
    rng <- c(df_all$Bias - 1, df_all$Bias + 1)
    rsq_plt <- dotplot("Bias" ~ df_all$Bias, #asp = 0.5, 
                       xlab = "Value", ylab = "",
                       col = "grey20",
                       scales = list(x = list(draw = FALSE)),
                       par.settings = envinmr.theme(),
                       cex = 1.2, as.table = TRUE)
    
    
    err_plt <- dotplot(variable ~ value, data=df_melt[-1,],
                       xlab = "Value", ylab = "",
                       xlim=c(-0.05,1.05),
                       col = "grey20",
                       pch = 20,
                       par.settings = envinmr.theme(),
                       cex = 1.2, as.table = TRUE)
    
    out_plt <- resizePanels(latticeCombineGrid(list(rsq_plt, err_plt),
                                               layout = c(1, 2)), 
                            h = c(1/4, 3/4))
    
    out_plt <- update(out_plt, panel = panel.fun)
    
  }
   (!plot) return(df_all) else 
    return(list(stats = df_all,
                plot = out_plt))
}

#' Environmental Informatics Marburg lattice plotting theme
#'
#' @description This theme is a modied version of \code{\link{theEconomist.theme}}.
#' Especially the regions colour setting has been modied to a
#' conceptually pleasant rainbow colour palette (optimised for temperature).
#'
#' @param win.fontfamily character font family.
#' @param with.bg logical.  \code{FALSE}, the background is transparent.
#' @param box colour of the box around the main plot and,  present, the color key
#' @param ... Further arguments passed on to \code{\link{simpleTheme}}.
#'
#' @author
#' Tim Appelhans
#'
#' @seealso
#' \code{\link{theEconomist.theme}}
#'
#' @examples
#' library(lattice)
#' levelplot(matrix(rnorm(10000), 100, 100),
#'           par.settings = envinmr.theme(), at = seq(-5, 5, 0.1))
#'
#' @export envinmr.theme
#' @aliases envinmr.theme

envinmr.theme <- function(win.fontfamily = NULL,
                          with.bg = FALSE,
                          box = "black",
                          ...) {
  
  theme <- list(background = list(col =  (with.bg) "#D5E2E9" else "transparent"),
                plot.line = list(col = "#00526D", lwd = 2.5),
                superpose.line = list(col = c("#00526D", "#00A3DB",
                                              "#7A2713", "#939598",
                                              "#6CCFF6"), lwd = 2.5),
                plot.symbol = list(col = "#00526D", pch = 16),
                superpose.symbol = list(col = c("#00526D", "#00A3DB",
                                                "#7A2713", "#939598",
                                                "#6CCFF6"), pch = 16),
                plot.polygon = list(col = "#00526D"),
                superpose.polygon = list(col = c("#5F92A8", "#00526D",
                                                 "#6CCFF6", "#00A3DB",
                                                 "#A7A9AC")),
                regions = list(col = colorRampPalette(c("#ebeaf7", "#83b0d6",
                                                        "#55A1B1", "#63AD99",
                                                        "#7FB972", "#B5BD4C",
                                                        "#D9AD3C", "#E68E34",
                                                        "#E6642C", "#D92120",
                                                        "#460000"))(100)),
                regions.fun = function(n) {
                  colorRampPalette(c("#ebeaf7", "#83b0d6",
                                     "#55A1B1", "#63AD99",
                                     "#7FB972", "#B5BD4C",
                                     "#D9AD3C", "#E68E34",
                                     "#E6642C", "#D92120",
                                     "#460000"))(n)
                },
                topo.cols = function(n) {
                  colorRampPalette(c("#00434B","#048354",
                                     "#878F3F","#C4906D",
                                     "#DE93B2","#CCA9E0",
                                     "#9FC5E6","#99D6D1",
                                     "#C2DBC3"))(n)
                },
                par.main.text = list(font = 1),
                reference.line = list(col =  (with.bg) "white" else "#aaaaaa",
                                      lwd = 1.75),
                dot.line = list(col =  (with.bg) "white" else "#aaaaaa",
                                lwd = 1.75),
                add.line = list(col = "#ED1C24", lwd = 1.5),
                axis.line = list(col = box), box.3d = list(col = box),
                strip.border = list(col = box),
                strip.background = list(col =  (with.bg) "white" else "#CBDDE6"),
                strip.shingle = list(col =  (with.bg) "#CBDDE6" else "#00A3DB",
                                     alpha = 0.5),
                axis.text = list(cex = 0.8),
                box.dot = list(col = "#00526D", pch = "|", lwd = 1.75),
                box.rectangle = list(fill = "#00526D", alpha = 0.5,
                                     col = "#00526D", lwd = 1.75),
                box.umbrella = list(col = "#00526D", lty = 1, lwd = 1.75))
   (.Platform$OS.type == "windows" && !is.null(win.fontfamily)) {
    windowsFonts(TheEconomistLike = win.fontfamily)
    theme$grid.pars$fontfamily <- "TheEconomistLike"
  }
  else {
  }
  modyList(modyList(lattice::standard.theme("pdf"), theme), lattice::simpleTheme(...))
}
