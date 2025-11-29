spatial.plot <- function (G, x, y, names = NULL, plot = TRUE, col = "lightblue", 
                           cex.text = 0.4, cex = 1, arrow.col = "lightblue", arrow.lwd = 1, 
                           plot.bg = "white", pch = 21, pt.bg = "orange", grid = FALSE, grid.col = "white", grid.lwd = 2, 
                           plot.dry = FALSE, col.dry = gray(0.7), cex.dry = 1, pch.dry = 19, 
                           arrow.col.dry = gray(0.7), arrow.lwd.dry = 1, cnw = NULL, 
                           xlim = NULL, ylim = NULL, arrow.warn = TRUE, angle = 30, node.over.arrow = 
                             FALSE,...) 
{
  if (!arrow.warn) {
    op <- options()
    options(warn = -1)
  }
  if (is.null(names)) 
    names <- 1:length(x)
  # step 1: no nodes
  if (length(V(G)$name) == 0) {
    if (plot) {
      plot(x, y, type = "n", xlim = xlim, ylim = ylim, 
           ...)
      out <- par("usr")
      if(grid){grid(col = grid.col, lwd = grid.lwd)}
      rect(out[1], out[3], out[2], out[4], col = plot.bg)
      if (plot.dry) {
        points(x, y, pch = pch.dry, col = col.dry, cex = cex.dry)
      }
    }
  } # end step 1 
  # step 2 >= 1 node
  else {
    a <- attributes(E(G))$vnames
    b <- attributes(V(G))$names
    if (all(is.na(match(b, names)))) 
      stop("names do not match node names in G.")
    #  step 2a subsect x and y, if necessary 
    if (length(b) != length(names)) {
      m <- match(b, names)
      w <- which(!(names %in% b))
      ids <- 1:length(b)
      x.d <- x[w]
      y.d <- y[w]
      x.n <- x[m]
      y.n <- y[m]
      names <- names[match(b, names)]
    }
    else {
      x.n <- x
      y.n <- y
      x.d <- NA
      y.d <- NA
    }
    # end step 2a
    # step 2b graph components 
    if (length(attributes(E(G))$vnames) > 0) {
      bounds <- matrix(ncol = 2, data = unlist(strsplit(a, 
                                                        "\\|")), byrow = TRUE)
      x0 <- y1 <- y0 <- x1 <- 1:nrow(bounds)
      for (i in 1:nrow(bounds)) {
        x0[i] = x.n[which(names == bounds[, 1][i])]
        x1[i] = x.n[which(names == bounds[, 2][i])]
        y0[i] = y.n[which(names == bounds[, 1][i])]
        y1[i] = y.n[which(names == bounds[, 2][i])]
      }
    }
    # end step 2b
    # step 2c use xlim and ylim if user specified
    if (is.null(xlim)) {
      xlim <- range(c(x.n, x.d), na.rm = TRUE)
      xspace <- (xlim[2] - xlim[1])/20
      xlim <- c(xlim[1] - xspace, xlim[2] + xspace)
    }
    if (is.null(ylim)) {
      ylim <- range(c(y.n, y.d), na.rm = TRUE)
      yspace <- (ylim[2] - ylim[1])/20
      ylim <- c(ylim[1] - yspace, ylim[2] + yspace)
    }
    x <- x.n
    y <- y.n
    # step 3 make plot
    if (plot) {
      plot(x, y, type = "n", xlim = xlim, ylim = ylim, 
           ...)
      out <- par("usr")
      rect(out[1], out[3], out[2], out[4], col = plot.bg)
      if(grid){grid(col = grid.col, lwd = grid.lwd)}
      # step 3a, plot dry nodes
      # step 3a_1 no NA for x.d, and complete network provided 
      if (plot.dry & all(!is.na(x.d)) & !is.null(cnw)) {
        points(cnw$x, cnw$y, pch = pch.dry, col = col.dry, 
               cex = cex.dry)
        points(x, y, pch = pch, col = col, cex = cex, bg = pt.bg)
        if (length(attributes(E(G))$vnames) > 0) {
          arrows(cnw$x0, cnw$y0, cnw$x1, cnw$y1, col = arrow.col.dry, 
                 length = 0.08, lwd = arrow.lwd.dry, angle = angle)
        }
        if (length(attributes(E(G))$vnames) > 0) {
          arrows(x0, y0, x1, y1, col = arrow.col, length = 0.08, 
                 lwd = arrow.lwd, angle = angle)
        }
        if(node.over.arrow) {
          points(cnw$x, cnw$y, pch = pch.dry, col = col.dry, 
                 cex = cex.dry)
          points(x, y, pch = pch, col = col, cex = cex, bg = pt.bg)
        }
      } # end 3ai
      #step 3a_ii plot dry nodes, no NA for x.d, and complete network NOT provided
      if (plot.dry & all(!is.na(x.d)) & is.null(cnw)) {
        points(x.d, y.d, pch = pch.dry, col = col.dry, 
               cex = cex.dry)
        points(x, y, pch = pch, col = col, cex = cex, bg = pt.bg)
        if (length(attributes(E(G))$vnames) > 0) {
          arrows(x0, y0, x1, y1, col = arrow.col, length = 0.08, 
                 lwd = arrow.lwd, angle = angle)
        }
        if(node.over.arrow) {points(x, y, pch = pch, col = col, cex = cex, bg = pt.bg)}
      } 
      # step 3b, don't plot dry nodes 
      if(!plot.dry){
        points(x, y, pch = pch, col = col, cex = cex, bg = pt.bg)
        if (length(attributes(E(G))$vnames) > 0) {
          arrows(x0, y0, x1, y1, col = arrow.col, length = 0.08, 
                 lwd = arrow.lwd, angle = angle)
        }
        if(node.over.arrow) {points(x, y, pch = pch, col = col, cex = cex, bg = pt.bg)}
      } # end step 3b
      if (cex.text != 0) {
        thigmophobe.labels(x.n, y.n, names, cex = cex.text)      
      }
    }
    if (!arrow.warn) 
      options(op)
    outL <- list()
    outL$x <- x
    outL$y <- y
    if (length(attributes(E(G))$vnames) > 0) {
      outL$x0 <- x0
      outL$x1 <- x1
      outL$y0 <- y0
      outL$y1 <- y1
    }
    invisible(outL)
  }
}


spatial.plot.sf <- function (x, y, names, shapefile = NULL, cex = 1, arrow.col = "lightblue", 
          arrow.lwd = 1, pch = 21, pt.bg = "orange") 
{
  if (requireNamespace(c("ggplot2", "sf"), quietly = TRUE)) {
    coords <- data.frame(x = x, y = y, Object.ID = names)
    g1 <- ggplot2::ggplot(shapefile) + ggplot2::geom_sf(lwd = arrow.lwd, 
                                                        colour = arrow.col) + ggplot2::geom_point(data = coords, 
                                                                                                  ggplot2::aes(x = x, y = y), shape = pch, fill = pt.bg, 
                                                                                                  size = cex * 1.8) + ggplot2::ylab("") + ggplot2::xlab("")
    g1
  }
  else {
    stop("Please install and load packages ggplot2 and sf.")
  }
}