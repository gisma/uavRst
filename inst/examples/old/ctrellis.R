
##
## Copyright (c) 2007 Felix Andrews <felix@nfrac.org>
## GPL version 2 or newer


xyplot.list <-
  function(x, data = NULL, ..., FUN = xyplot,
           y.same = TRUE, x.same = NA, layout = NULL,
           merge.legends = FALSE)
  {
    if (length(x) == 0) return(NULL)
    ## NOTE lapply here causes problems with eval.parent and `...` later.
    #objs <- lapply(x, FUN, data = data, ...)
    objs <- vector(mode = "list", length = length(x))
    for (i in as.numeric(seq_along(x))) {
      ## this is what we had previously, but it seemed to cause failures
      ## in complex call structures (e.g. pch=pch ==> object 'pch' not found)
      ## (use substitute to get reasonable ylab)
      #objs[[i]] <- eval.parent(substitute(FUN(x[[i]], data = data, ...)))
      ## check for 'data' to avoid warnings in e.g. qqmath.numeric
      objs[[i]] <-
        if (!is.null(data)) FUN(x[[i]], data = data, ...) else FUN(x[[i]], ...)
    }
    names(objs) <- names(x)
    ok <- unlist(lapply(objs, inherits, "trellis"))
    if (any(!ok))
      stop("FUN returned object of class ",
           toString(class(objs[[ which(!ok)[1] ]])),
           ", not trellis.")
    ans <- do.call("c", c(objs,
                          list(x.same = x.same, y.same = y.same,
                               layout = layout, merge.legends = merge.legends)))
    ans$call <- match.call()
    ans
  }

c.trellis <-
  function(..., x.same = NA, y.same = NA,
           layout = NULL, merge.legends = FALSE,
           recursive = FALSE)
  {
    objs <- list(...)
    if (length(objs) == 0) return(NULL)
    if (length(objs) == 1) {
      ## only one object
      obj <- objs[[1]]
      ## set dimnames if given and only one panel
      if (!is.null(names(objs)) && (prod(dim(obj)) == 1))
        rownames(obj) <- names(objs)
      return(obj)
    }
    if (length(objs) > 2) {
      ## merge first two objects, and call again
      first2Merged <-
        do.call("c.trellis", c(objs[1:2],
                               list(x.same = x.same, y.same = y.same,
                                    merge.legends = merge.legends)))
      return(do.call("c.trellis", c(list(first2Merged), objs[-(1:2)],
                                    list(x.same = x.same, y.same = y.same,
                                         layout = layout, merge.legends = merge.legends))))
    }
    ## now exactly 2 objects
    obj1 <- objs[[1]]
    obj2 <- objs[[2]]
    ## number of packets in object, i.e. offset
    NPACK1 <- prod(dim(obj1))
    NPACK2 <- prod(dim(obj2))
    ## first panel function
    panel <- obj1$panel
    PANEL1 <- if (is.function(panel)) panel
    else if (is.character(panel)) get(panel)
    else eval(panel)
    ## second panel function
    panel <- obj2$panel
    PANEL2 <- if (is.function(panel)) panel
    else if (is.character(panel)) get(panel)
    else eval(panel)
    obj1$panel <- function(...) {
      if (packet.number() <= NPACK1)
        PANEL1(...)
      else PANEL2(...)
    }
    ## TODO: treat 'prepanel' the same way as 'panel'?
    ## flatten the trellis objects (make 1 dimensional)
    flatIC <- function(index.cond) {
      dim <- sapply(index.cond, length)
      ic <- do.call(expand.grid, index.cond)
      if (length(dim) >= 2)
        ic[,2] <- (ic[,2] - 1) * dim[1]
      if (length(dim) >= 3)
        ic[,3] <- (ic[,3] - 1) * prod(dim[1:2])
      rowSums(ic)
    }
    flatCL <- function(condlevels, newname=NULL) {
      ## paste names of variables to their values (for strips)
      #for (i in seq_along(condlevels))
      #    condlevels[[i]] <- paste(names(condlevels)[i], ## may be NULL
      #                             condlevels[[i]], sep=" = ")
      ## convert shingle levels to character strings
      condlevels <- lapply(condlevels, as.character)
      cl <- do.call(expand.grid, condlevels)
      cl <- apply(cl, 1, paste, sep=" / ")
      if (!is.null(newname) && (nchar(newname) > 0)) {
        if (length(cl) == 1) cl <- newname
        else cl <- paste(newname, cl, sep=": ")
      }
      cl
    }
    obj1$index.cond <- list(c(flatIC(obj1$index.cond),
                              flatIC(obj2$index.cond) + NPACK1))
    obj1$condlevels <- list(c(flatCL(obj1$condlevels, names(objs)[1]),
                              flatCL(obj2$condlevels, names(objs)[2])))
    obj1$perm.cond <- 1
    
    ## make scales nominally "free", so they look like original objects
    makeFreeScales <- function(obj, npack, x.y)
    {
      obj[[paste(x.y, "scales", sep=".")]]$relation <- "free"
      .limits <- paste(x.y, "limits", sep=".")
      .num.limit <- paste(x.y, "num.limit", sep=".")
      .used.at <- paste(x.y, "used.at", sep=".")
      if (is.null(obj[[.limits]])) obj[[.limits]] <- NA
      if (is.null(obj[[.num.limit]])) obj[[.num.limit]] <- NA
      if (is.null(obj[[.used.at]])) obj[[.used.at]] <- NA
      if (!is.list(obj[[.limits]])) {
        obj[[.limits]] <- rep(list(obj[[.limits]]), length=npack)
        obj[[.num.limit]] <- rep(list(obj[[.num.limit]]), length=npack)
        obj[[.used.at]] <- rep(list(obj[[.used.at]]), length=npack)
      }
      obj
    }
    ## set relations to "free" if the first object has "free" scales
    ## or if the limits in the two objects are not identical
    xlimItems <- c("x.limits", "x.num.limit", "x.used.at")
    ylimItems <- c("y.limits", "y.num.limit", "y.used.at")
    if (is.na(x.same)) {
      x.same <- FALSE
      if (!is.list(obj1$x.limits) &&
          identical(unclass(obj1)[xlimItems],
                    unclass(obj2)[xlimItems]))
        x.same <- NA
    }
    if (is.na(y.same)) {
      y.same <- FALSE
      if (!is.list(obj1$y.limits) &&
          identical(unclass(obj1)[ylimItems],
                    unclass(obj2)[ylimItems]))
        y.same <- NA
    }
    if (identical(x.same, FALSE) ||
        is.list(obj1$x.limits))
    {
      obj1 <- makeFreeScales(obj1, npack=NPACK1, x.y="x")
      obj2 <- makeFreeScales(obj2, npack=NPACK2, x.y="x")
      obj1$x.limits <- c(obj1$x.limits, obj2$x.limits)
      obj1$x.num.limit <- c(obj1$x.num.limit, obj2$x.num.limit)
      obj1$x.used.at <- c(obj1$x.used.at, obj2$x.used.at)
    }
    if (identical(y.same, FALSE) ||
        is.list(obj1$y.limits))
    {
      obj1 <- makeFreeScales(obj1, npack=NPACK1, x.y="y")
      obj2 <- makeFreeScales(obj2, npack=NPACK2, x.y="y")
      obj1$y.limits <- c(obj1$y.limits, obj2$y.limits)
      obj1$y.num.limit <- c(obj1$y.num.limit, obj2$y.num.limit)
      obj1$y.used.at <- c(obj1$y.used.at, obj2$y.used.at)
    }
    
    ## merge common panel args into panel.args
    ## check for identical() args
    commonNames <- intersect(names(obj1$panel.args.common),
                             names(obj2$panel.args.common))
    identNames <- commonNames[unlist(lapply(commonNames, function(x)
      identical(obj1$panel.args.common[[x]],
                obj2$panel.args.common[[x]])))]
    obj1Common <- names(obj1$panel.args.common) %in% identNames
    obj2Common <- names(obj2$panel.args.common) %in% identNames
    obj1$panel.args <- lapply(obj1$panel.args, c,
                              obj1$panel.args.common[!obj1Common])
    obj2$panel.args <- lapply(obj2$panel.args, c,
                              obj2$panel.args.common[!obj2Common])
    obj1$panel.args.common <- obj1$panel.args.common[obj1Common]
    ## the actual data
    obj1$panel.args <- c(obj1$panel.args, obj2$panel.args)
    obj1$packet.sizes <- c(obj1$packet.sizes, obj2$packet.sizes)
    ## some prepanel functions require a 'subscripts' argument in each 'panel.args'
    if ("subscripts" %in% c(names(formals(obj1$prepanel.default)),
                            names(formals(obj1$prepanel)))) {
      for (i in seq_along(obj1$panel.args)) {
        if ("subscripts" %in% names(obj1$panel.args[[i]]) == FALSE) {
          obj1$panel.args[[i]]$subscripts <- TRUE
        }
      }
    }
    
    ## recalculate panel limits using all data
    if ((isTRUE(x.same) || isTRUE(y.same))) {
      scalesSpec <- list()
      if (isTRUE(x.same)) scalesSpec$x$relation <- "same"
      if (isTRUE(y.same)) scalesSpec$y$relation <- "same"
      obj1 <- update(obj1, scales = scalesSpec)
    }
    
    if (identical(obj1$strip.left, FALSE)) {
      ## turn strips on if either object has strips, or names were given
      if (identical(obj1$strip, FALSE) &&
          !identical(obj2$strip, FALSE))
        obj1$strip <- obj2$strip
      if (identical(obj1$strip, FALSE) &&
          !is.null(names(objs)))
        obj1$strip <- "strip.default"
    }
    
    ## TODO: can use 'par.settings' from obj2 only for obj2 panels?
    obj1$par.settings <- modifyList(as.list(obj2$par.settings),
                                    as.list(obj1$par.settings))
    if (merge.legends)
      obj1$legend <- mergeTrellisLegends(obj1$legend, obj2$legend)
    
    obj1$layout <- layout
    obj1$call <- call("c", obj1$call, obj2$call,
                      x.same = x.same, y.same = y.same,
                      layout = layout)
    ## need this to allow further calls to update() to insert arguments:
    obj1$call <- call("update", obj1$call)
    obj1
  }