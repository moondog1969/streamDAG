\name{spatial.plot}
\alias{spatial.plot}
\alias{spatial.plot.sf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spatial plot of an igraph object or stream shapefile
}
\description{
Makes a spatial plot of a igraph object or stream shapefile, given nodal coordinates and node IDs.
}
\usage{

spatial.plot(G, x, y, names = NULL, 
                         plot = TRUE,
                         col = "lightblue", 
                         cex.text = .4, cex = 1,
                         arrow.col = "lightblue", arrow.lwd = 1, 
                         plot.bg = "white", pch = 21, 
                         pt.bg = "orange", grid.lwd = 2, 
                         plot.dry = FALSE,
                         col.dry = gray(.7),
                         cex.dry = 1, pch.dry = 19, 
                         arrow.col.dry = gray(.7), arrow.lwd.dry = 1,
                         cnw = NULL, xlim = NULL, ylim = NULL, ...)


spatial.plot.sf(x, y, names, shapefile = NULL, cex = 1, arrow.col = "lightblue", 
                arrow.lwd = 1, pch = 21, pt.bg = "orange")
}

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
}
  \item{plot}{Logical. Create plot?  
}

  \item{shapefile}{
Shapefile object brought in using library \emph{sf}
}

  \item{col}{
point symbol color.
}

  \item{cex.text}{
Character expansion for node labels in plot; \code{cex.text = 0} suppresses labels.
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
  \item{plot.dry}{
Logical.  Should ``dry'' nodes, i.e., nodes in \code{names} (and \code{x} and \code{y}) that are not also in \code{G} be plotted?  
}

  \item{col.dry}{
Color of ``dry'' nodes in plot.     
}
  \item{cex.dry}{
Symbol sizer of ``dry'' nodes in plot.     
}
  \item{pch.dry}{
Plotting character (symbol) of ``dry'' nodes in plot.  
}
  \item{arrow.col.dry}{
Arrow color for "dry" arcs. Dry arrow rendering requires \code{cnw} designation (see Examples). 
}
  \item{arrow.lwd.dry}{
Arrow line width for "dry" arcs. Dry arrow rendering requires \code{cnw} designation (see Examples). 
}
  \item{cnw}{
Complete network \code{spatial.plot} object. 
}
  \item{xlim}{
A numeric vector of length 2, giving the lower and upper \emph{y}-axis limits. 
}
  \item{ylim}{
A numeric vector of length 2, giving the lower and upper \emph{x}-axis limits. 
}
  \item{\dots}{
Other arguments to \code{\link{plot}}
}
}
\details{ 
The function \code{spatial.plot} makes a plot of stream DAG, showing arc flow directions to and from spatial node locations. The function can also be used to identify node and arc arrow coordinates for plotting (see Examples). The function \code{spatial.plot.sf} can create a spatially explicit graph from a stream shapefile with the stream outlay under a ggplot framework (see Examples). The function \code{spatial.plot} currently has gretly flexibility (for instance, it can be used to distinguish dry and wet nodes and arcs) because it is based on underlying \emph{igraph} DAGs. 
}

\value{
A plot and an \code{invisible} list containing the \code{x} and \code{y} coordinates of nodes: the objects \code{$x} and \code{$y}, respectively, and the \code{x} and \code{y} coordinates of start and end points of arc arrows:the objects \code{$x0}, \code{$y0}, \code{$x1}, and \code{$y1}, respectively.
}

\author{
Ken Aho
}

\examples{
G <- graph_from_literal(IN_N --+ M1984 --+ M1909, IN_S --+ M1993, 
M1993 --+ M1951 --+ M1909 --+ M1799 --+ M1719 --+ M1653 --+ M1572 --+ M1452,
M1452--+ M1377 --+ M1254 --+ M1166 --+ M1121 --+ M1036 --+ M918 --+ M823, 
M823 --+ M759 --+ M716 --+ M624 --+ M523 --+ M454 --+ M380 --+ M233 --+ M153,
M153 --+ M91 --+ OUT)

data(mur_coords)

x <- mur_coords[,2]
y <- mur_coords[,3]
names <- mur_coords[,1]
spatial.plot(G, x, y, names)

# using shapefiles
\donttest{
library(ggplot2); library(sf)
mur_sf <- st_read(system.file("shape/Murphy_Creek.shp", package="streamDAG"))
g1 <- spatial.plot.sf(x, y, names, shapefile = mur_sf)

# modify ggplot
library(ggrepel)
g1 <- spatial.plot(G, x, y, names, 
plot.shapefile = TRUE, shapefile = mur_sf)
g1 + theme_classic()
}

#-- Distinguishing wet and dry arcs and nodes --#

data(mur_node_pres_abs) # STIC H2O presence/absence
npa <- mur_node_pres_abs[650,][,-1] # STC data from 8/9/2019 22:30 
G1 <- delete.nodes.pa(G, npa) # delete nodes based STIC data

# Example 1 (only show wet nodes and arcs with associated wet nodes)
spatial.plot(G1, x, y, names)

# Example 2 (show wet nodes and arcs with associated wet nodes, and dry nodes)
spatial.plot(G1, x, y, names, plot.dry = TRUE)

# Example 3 (show wet nodes and arcs with associated wet nodes, and underlying network)
entire <- spatial.plot(G, x, y, names, plot = FALSE)
spatial.plot(G, x, y, names, plot.dry = TRUE, cnw = entire)

#-- Animation: drying of Johnson Draw drainage --#
\donttest{
data(jd_lengths)
jd_graph <- streamDAGs("jd_stic_full")
data(AIMS.node.coords)
jd_coords <- AIMS.node.coords[AIMS.node.coords$site == "JD",]
data(jd_node_pres_abs)
jd_coords <- jd_coords[-c(1:7),] # remove piezometer coordinates

pb = txtProgressBar(min = 1, max = 250, initial = 1, style = 3) 
times <- round(seq(1,50322, length = 250),0)

for(i in 1:250){
  dev.flush()
  jd_sub <- delete.nodes.pa(jd_graph, jd_node_pres_abs[times[i],][-1])
  spatial.plot(jd_sub, 
               x = jd_coords[,3], 
               y = jd_coords[,2], 
               names = jd_coords[,1], 
               ylim = c(43.122, 43.129), 
               xlim = c(-116.8, -116.775), 
               plot.dry = TRUE, main = jd_node_pres_abs[times[i],][1], 
               xlab = "Longitude", ylab = "Latitude")
  dev.hold()
  Sys.sleep(.05)
  setTxtProgressBar(pb, i)
  }
}
}

