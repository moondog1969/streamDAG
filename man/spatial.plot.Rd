\name{spatial.plot}
\alias{spatial.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spatial plot of an igraph object
}
\description{
Makes a spatial plot of a igraph object, given nodal coordinates
}
\usage{
spatial.plot(G, x, y, names = NULL, plot.shapefile = FALSE, 
shapefile = NULL, col = 1, text.cex = 0.7, cex = 1, arrow.col = "blue", 
arrow.lwd = 1, plot.bg = gray(.9), pch = 21, pt.bg = "orange", grid.lwd = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
Graph object, see \code{\link[igraph]{graph_from_literal}}.
}
  \item{x}{
X-coordinates of nodes.
}
  \item{y}{
Y-coordinates of nodes.
}
  \item{names}{Names of nodes, must use the same names as \code{G} and correspond to the order of coordinates in \code{x} and \code{y}.
%%     ~~Describe \code{names} here~~
}
  \item{plot.shapefile}{
Logical.  Plot shapefile (if provided) instead of arrows showing DAG flow.  Requires library \emph{ggplot2} and \emph{sf}.
}
  \item{shapefile}{
Shapefile object brought in using library \emph{sf}
}

  \item{col}{
point symbol color.
}

  \item{text.cex}{
Character expansion for text.
}

  \item{cex}{
Chahracter expnansion of point symbols.
}

  \item{arrow.col}{
Color of plot arrows.
}

  \item{arrow.lwd}{
Arrow line width.
}

  \item{plot.bg}{
Background color of plot.
}
  \item{pch}{
Plotting character.
}
  \item{pt.bg}{
Background color for plotting character.
}
  \item{grid.lwd}{
Grid line width; \code{grid.lwd = 0} suppresses grid.
}
  \item{\dots}{
Other arguments to \code{\link{plot}}
}
}
\details{
Makes a graph showing arc flow directions to and from spatial node locations, or a spatially explicit graph generated from a stream shapefile showing the stream outlay and node points.
}
\author{
Ken Aho
}

\examples{
murphy_spring <- graph_from_literal(IN_N --+ M1984 --+ M1909, IN_S --+ M1993, 
M1993 --+ M1951 --+ M1909 --+ M1799 --+ M1719 --+ M1653 --+ M1572 --+ M1452,
M1452--+ M1377 --+ M1254 --+ M1166 --+ M1121 --+ M1036 --+ M918 --+ M823, 
M823 --+ M759 --+ M716 --+ M624 --+ M523 --+ M454 --+ M380 --+ M233 --+ M153,
M153 --+ M91 --+ OUT)

data(mur_coords)

x <- mur_coords[,2]
y <- mur_coords[,3]
names <- mur_coords[,1]
spatial.plot(murphy_spring, x, y, names)

\dontrun{
# using shapefiles
library(ggplot2); library(sf)
mur_sf <- st_read("Murphy_shapefile/Murphy/Murphy_Creek.shp")
spatial.plot(murphy_spring, x, y, names, 
plot.shapefile = TRUE, shapefile = mur_sf)

# modify ggplot
g1 <- spatial.plot(murphy_spring, x, y, names, 
plot.shapefile = TRUE, shapefile = mur_sf)
g1 + theme_classic()
}
}

