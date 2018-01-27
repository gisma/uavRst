#' combine multiple lattice plots layerwise
#' 
#' @description
#' this function combines multiple lattice plot objects drawing 
#' each as a layer on top of the previous plots. Note that the 
#' global plot settings (e.g. xlim, ylim, ...) is taken from the 
#' first object. This is particularly useful when looping over large amounts of data
#' using \code{\link{lapply}} (see examples).
#' 
#' @param trellis.list a list containing lattice plot objects
#' @param ... additional arguments passed to \code{\link{as.layer}}
#' 
#' @return
#' a single lattice plot object
#' 
#' @author
#' Tim Appelhans
#' 
#' @seealso
#' \code{\link{as.layer}}
#' 
#' @examples
#' library(latticeExtra)
#' dat <- list(1:10,
#'             10:1,
#'             3:7,
#'             7:3)
#' 
#' plist <- lapply(seq(dat), function(i) {
#'  tmp <- xyplot(dat[[i]] ~ seq(dat[[i]]),
#'                type = "l", col = i)
#' })
#' 
#' p <- latticeCombineLayer(plist)
#' 
#' print(p)
#' 
#' @export latticeCombineLayer
#' @aliases latticeCombineLayer

latticeCombineLayer <- function(trellis.list, ...) {
  
  outLayer <- function(x, y, ...) {
    x + as.layer(y, ...)
  }
  
  out <- Reduce(outLayer, trellis.list, ...)
  return(out)
}