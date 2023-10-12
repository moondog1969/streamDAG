\name{vector_segments}
\alias{vector_segments}
\alias{assign_pa_to_segments}
\alias{vector_segments}
\alias{print.network_to_sf}

\title{
Functions for overlaying networks on shapefiles
}
\description{
The function \code{vector_segments} and \code{assign_pa_to_segments} were written to facilitate the generation of plots (including ggplots) that overlay user defined digraphs (based on arc designations) on GIS shapefiles or other tightly packed cartesian coordinate structures. 
}
\usage{
vector_segments(sf.coords, node.coords, realign = TRUE, arcs, arc.symbol = " --> ", 
nneighbors = 40, remove.duplicates = FALSE)

assign_pa_to_segments(input, n, arc.pa, datetime = NULL)
}

\arguments{
  \item{sf.coords}{A two column dataframe containing shapefile Cartesian coordinates (or other tightly packed Cartesian coordinates, see Examples). The first column should define \code{x} locations and the second column define \code{y} locations.
}
  \item{node.coords}{
A two column dataframe containing network node Cartesian coordinates, with the first column defining \code{x} location and the second column defining \code{y} location.  The coordinates should use the same coordinate system as \code{sf.coords}, e.g., UTM easting and northing, longitude and latitude, etc. The \code{row.names} attribute should contain the correct node names (i.e., they should correspond to names used in the argument \code{arcs}.
}
  \item{realign}{
Logical.  If \code{node.coords} do not exist in \code{sf.coords} should they be assigned to the closest location in \code{sf.coords}?  The default option \code{realign = TRUE} is strongly reccomended, and may be set permenantly in later versions of \code{vector_segments}.
}
  \item{arcs}{
A character vector of arc names in the network.  In particular, designations of nodes which serve arcs bounds, seperated by a user-defined \code{arc.symbol}.  For example, to designate the arc \eqn{\overrightarrow{uv}} using the \code{arc.symbol} \code{ --> }, I would use: \code{u --> v}. Node names used to define arcs in the character vector should correspond to those in \code{row.names(node.coords)}. 
}
  \item{arc.symbol}{A symbol indicating the directional arc connecting two nodes. For example, to designate the arc \eqn{\overrightarrow{uv}}, the package \emph{igraph} uses \code{u|v}, while \emph{streamDAG} generally uses \code{u --> v}. 
}
  \item{nneighbors}{
Number of nearest neighbor points to potentially consider as the next point in an evolving arc path.  
}
  \item{remove.duplicates}{
Logical.  For duplicate coordinates, should the second point be removed?
}
  \item{input}{
The first argument for \code{assign_pa_to_segments}. Ideally, the output from \code{vector_segments}.  For example, let \code{output <- vector_segments(...)}, then \code{input = output}.
}
  \item{n}{
The number of repeated presence/absence timeframe observations for surface water contained in \code{arc.pa}.  
}
  \item{arc.pa}{
An \eqn{n \times m} matrix or dataframe of stream arc surface water presence/absence = \eqn{\{0, 1\}} outcomes, where \eqn{n} denotes the number of observed timeframes in which arcs were observed, and \eqn{m} is the number of arcs.  The \code{names} of the dataframe should correspond to those given in the \code{arcs} argument from \code{vector_segments}.} 
\item{datetime}{Optional \code{unique()} time classes corresping to rows in \code{arc.pa}.}
}
\details{
The function \code{vector_segments} assigns network arc designations (from the argument \code{arcs}) to shape
file coordinates.  The function \code{assign_pa_to_segments} presence/absence surface water designations to these arcs based on information from \code{arc.pa}.
}
\value{
The function \code{vector_segments} creates an object of class \code{network_to_sf}. It also returns a list with two components, with only the first being \code{visible}. 
\item{df}{Is a dataframe with four columns: 1) \code{point} (referring an original \code{sf.coord} location), 2) \code{arc.label}, an assigned arc name for the location, 3) \code{x} the \emph{x} coordinates,  and 4) \code{y} the \emph{x} coordinates.}
\item{node.coords}{Is dataframe with the \code{node.coords} for stream arcs.  These will have been potentially shifted, if \code{realign = TRUE}, hence their inclusion as function output.}

The function \code{assign_pa_to_segments} returns a dataframe that adds a stream/presence absence column to the to the \code{df} dataframe output from \code{vector_segments}, based on the argument \code{arc.pa} 
}
\author{
Ken Aho
}
\note{
The \code{assign_pa_to_segments} function will return a \code{warning} (but will try to run anyway)  if \code{input} is not the output from \code{vector_segments}.  
}
\seealso{
\code{\link{spatial.plot}}
}
\examples{
# Data

sfx <- c(-3,0,1.5,2,2.9,4,5,6)
sfy <- c(5,2,1.7,1.6,1.5,1.4,1.2,1)
sf.coords <- data.frame(x = sfx, y = sfy) 
node.coords <- data.frame(x = c(-2.1,2,4,6), y = c(3.75,1.6,1.4,1))
row.names(node.coords) <- c("n1","n2","n3","n4") # must be consistent with arc names
arc.pa <- data.frame(matrix(ncol = 3, data = c(1,1,1, 0,1,1, 1,1,1, 0,0,1), byrow = TRUE))
names(arc.pa) <- c("n1 --> n2", "n2 --> n3", "n3 --> n4")

# Use of vector_segments
vs <- vector_segments(sf.coords, node.coords, realign = TRUE, names(arc.pa))
vs

# Plotting example
plot(sf.coords, pch = 19, col = c(rep(1,4),rep(2,2),rep(3,2)))

vsd <- vs$df
fal <- as.factor(vsd$arc.label)
lvls <- levels(fal)

for(i in 1:nlevels(fal)){
  temp <- vsd[fal == lvls[i],]
  lines(temp$x, temp$y, col = i) 
}

vs4 <- assign_pa_to_segments(vs, 4, arc.pa)
head(vs4)
}
