spatial.plot <- function(G, x, y, names = NULL, 
                         plot.shapefile = FALSE, 
                         shapefile = NULL, col = NULL, 
                         text.cex = .6, cex = 1,
                         arrow.col = "blue", plot.bg = "gray", 
                         pch = 21, pt.bg = "white", ... ){

if(!plot.shapefile){

  a <- attributes(E(G))$vnames
  bounds <- matrix(ncol = 2, data = unlist(strsplit(a, "\\|")), byrow = TRUE)

  b <- attributes(V(G))$names
  
if(length(b) != length(names)){
  x <- x[match(b, names)]
  y <- y[match(b, names)]
  names <- names[match(b, names)]
}
  
x0 <- 1:nrow(bounds) -> x1 -> y0 -> y1  
for(i in 1:nrow(bounds)){    
x0[i] = x[which(names == bounds[,1][i])]
x1[i] = x[which(names == bounds[,2][i])]
y0[i] = y[which(names == bounds[,1][i])]
y1[i] = y[which(names == bounds[,2][i])]
}
xlim <- range(x)
xspace<-(xlim[2]-xlim[1])/20
ylim <- range(y)
yspace<-(ylim[2]-ylim[1])/20
xlim<-c(xlim[1]-xspace,xlim[2]+xspace)
ylim<-c(ylim[1]-yspace,ylim[2]+yspace)

plot(x, y, type = "n", xlim = xlim, ylim = ylim, ...)
out <- par("usr")
rect(out[1], out[3], out[2], out[4], col = "gray")
grid()
points(x, y, pch = pch, col = col, cex = cex, bg = pt.bg)
arrows(x0, y0, x1, y1, col = arrow.col, length = .08)
thigmophobe.labels(x, y, names, cex = .4)
}
if(plot.shapefile){
  if(requireNamespace(c("ggplot2", "sf"), quietly = TRUE)) {
    coords <- data.frame(x = x, y = y, Object.ID = names)
    g1 <- ggplot2::ggplot(shapefile) +
      ggplot2::geom_sf(lwd = .1, colour = "blue") +
      ggplot2::geom_point(data = coords, ggplot2::aes(x = x, y = y), shape = 21,
      fill = "orange", size = 1.2) +
      ggplot2::ylab("") + ggplot2::xlab("") 
    g1  
  } else {stop("Please install and load packages ggplot2 and sf.")
}
}
}
