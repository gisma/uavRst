#' combine multiple lattice plots in a facetted grid (panels)
#' 
#' @description
#' this function combines multiple lattice plot objects in a facetted
#' grid. Note that the global plot settings (e.g. xlim, ylim, ...) 
#' are taken from the first object though the user can specify whether
#' \code{scales} should be identical or not. 
#' This is particularly useful when looping over large amounts of data
#' using \code{\link{lapply}} (see examples).
#' 
#' @param trellis.list a list containing lattice plot objects
#' @param between space between panels
#' @param as.table if TRUE (default) drawing is top left to bottom right
#' @param ... additional arguments passed to \code{\link{c.trellis}}
#' 
#' @return
#' a single lattice plot object
#' 
#' @author
#' Tim Appelhans
#' 
#' @seealso
#' \code{\link{c.trellis}}
#' 
#' @examples
#' #load data
#' #Use a probability map assuming high potential for city expansion is just 
#' #resulting from proximity to current urban area:
#' pred <- raster(system.file("probability.rst", package = "Rsenal"))
#' 
#' #observed city growth between 1990 and 2006
#' obs <- raster(system.file("citygrowth.tif", package = "Rsenal"))
#' 
#' #masking current urban area since these pixels have no potential for change
#' mask <- raster(system.file("citymask.tif", package = "Rsenal"))
#' 
#' #create data list
#' dat <- list(pred, obs, mask)
#' 
#' #create list of lattice plots
#' plist <- lapply(seq(dat), function(i) {
#'   tmp <- spplot(dat[[i]], scales = list(draw = TRUE))
#' })
#' 
#' #draw individually
#' plist[[1]]
#' plist[[2]]
#' plist[[3]]
#' 
#' #combine to grid
#' p <- latticeCombineGrid(plist)
#' print(p)
#' 
#' #change layout
#' p2 <- latticeCombineGrid(plist, layout = c(1, 3))
#' print(p2)
#' 
#' @export latticeCombineGrid
#' @aliases latticeCombineGrid

latticeCombineGrid <- function(trellis.list,
                               between = list(y = 0.3, x = 0.3),
                               as.table = TRUE,
                               ...) {
  
  outLayout <- function(x, y) {
    lattice:::update.trellis(latticeExtra:::c.trellis(x, y, ...), 
                             between = between, as.table = as.table)
  }
  
  out <- Reduce(outLayout, trellis.list)
  return(out)
}