spatial.plot <- function(G, x, y, names = NULL, 
                         plot = TRUE,
                         plot.shapefile = FALSE, 
                         shapefile = NULL, col = "blue", 
                         cex.text = .4, cex = 1,
                         arrow.col = "blue", arrow.lwd = 1, 
                         plot.bg = gray(.9), pch = 21, 
                         pt.bg = "orange", grid.lwd = 2, 
                         plot.dry = FALSE,
                         col.dry = gray(.7),
                         cex.dry = 1, pch.dry = 19, 
                         arrow.col.dry = gray(.7), arrow.lwd.dry = 1,
                         cnw = NULL,...)
  {
 
if(is.null(names)) names <- 1:length(x)

if(!plot.shapefile){
    a <- attributes(E(G))$vnames
    bounds <- matrix(ncol = 2, data = unlist(strsplit(a, "\\|")), byrow = TRUE)
    b <- attributes(V(G))$names
    if(all(is.na(match(b, names))))stop("names do not match node names in G.")
    
  if(length(b) != length(names)){ # is G a subgraph?
    m <- match(b, names)
    w <- which(!(names %in% b))
    ids <- 1:length(b)
    x.d <- x[w]
    y.d <- y[w]
    x.n <- x[m]
    y.n <- y[m]
    names <- names[match(b, names)]
  } else {x.n <- x; y.n <- y; x.d <- NA; y.d <- NA}
    
  x0 <- 1:nrow(bounds) -> x1 -> y0 -> y1
  
    for(i in 1:nrow(bounds)){    
    x0[i] = x.n[which(names == bounds[,1][i])]
    x1[i] = x.n[which(names == bounds[,2][i])]
    y0[i] = y.n[which(names == bounds[,1][i])]
    y1[i] = y.n[which(names == bounds[,2][i])]
    }
  
  xlim <- range(c(x.n,x.d), na.rm = TRUE)
  xspace <- (xlim[2]-xlim[1])/20
  ylim <- range(c(y.n,y.d), na.rm = TRUE)
  yspace <- (ylim[2]-ylim[1])/20
  xlim <- c(xlim[1]-xspace, xlim[2]+xspace)
  ylim <- c(ylim[1]-yspace, ylim[2]+yspace)
  
  x <- x.n; y <- y.n
  
  if(plot){
    plot(x, y, type = "n", xlim = xlim, ylim = ylim, ...)
    out <- par("usr")
    rect(out[1], out[3], out[2], out[4], col = plot.bg)
    grid(col = "white", lwd = grid.lwd)
    
      if(plot.dry & all(!is.na(x.d)) & !is.null(cnw)){
        points(cnw$x, cnw$y, pch = pch.dry, col = col.dry, cex = cex.dry)
        arrows(cnw$x0, cnw$y0, cnw$x1, cnw$y1, col = arrow.col.dry, length = .08, lwd = arrow.lwd.dry)
        }
    
      if(plot.dry & all(!is.na(x.d)) & is.null(cnw)){
        points(x.d, y.d, pch = pch.dry, col = col.dry, cex = cex.dry)}
    
    points(x, y, pch = pch, col = col, cex = cex, bg = pt.bg)
    arrows(x0, y0, x1, y1, col = arrow.col, length = .08, lwd = arrow.lwd)
    
    if(cex.text != 0){thigmophobe.labels(x.n, y.n, names, cex = cex.text)}
  }
  outL <- list()
  outL$x <- x; outL$y <- y
  outL$x0 <- x0; outL$x1 <- x1; outL$y0 <- y0; outL$y1 <- y1
  outI <- invisible(outL)
  }

  if(plot.shapefile){
    if(plot){
      if(requireNamespace(c("ggplot2", "sf"), quietly = TRUE)) {
        coords <- data.frame(x = x, y = y, Object.ID = names)
        g1 <- ggplot2::ggplot(shapefile) +
          ggplot2::geom_sf(lwd = arrow.lwd, colour = arrow.col) +
          ggplot2::geom_point(data = coords, ggplot2::aes(x = x, y = y), shape = 21,
                              fill = pt.bg, size = cex * 1.8) +
          ggplot2::ylab("") + ggplot2::xlab("") 
        g1  
      } else {stop("Please install and load packages ggplot2 and sf.")}
    }
  }
outI
}
