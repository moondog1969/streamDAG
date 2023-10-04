\name{ICSL}
\alias{ICSL}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Integral connectivity scale length (ICSL)
}
\description{
Integral connectivity scale lengths (ICSL, Western et al. 2013) is the average distance between wet locations using either (1) Euclidean distance or (2) topographically-defined hydrologic distance, e.g., instream hydrologic distance, subsurface distance (Ali and Roy 2009) and outlet distance, in which connected saturated paths must reach the catchment outlet. 
}
\usage{
ICSL(G, coords = NULL, names = NULL, lengths = NULL, 
dist.matrix = NULL, show.dist = FALSE)
}

\arguments{

  \item{G}{
A graph object of class "igraph", see \code{\link[igraph]{graph_from_literal}} 
}
  \item{coords}{
Spatial coordinates to allow computation of nodal Euclidean distances
}
  \item{names}{
Nodal names
}
  \item{lengths}{
Stream arc lengths or hydrologic arc lengths 
}
  \item{show.dist}{
Logical.  Show distance matrix?
}
  \item{dist.matrix}{
An optional distance matrix, potentially providing non-Euclidean node distances (e.g., node subsurface distance, etc.). Distance matrix \code{Labels} in \code{dist.matrix} must be analogous to those used in \code{G}. Note that dimensions in \code{dist.matrix} can be larger than the number of nodes in \code{G} if, for instance, \code{dist.matrix} represents distances of the complete wetted network and \code{G} is a subgraph of the complete wetted network after drying.
}
}
\details{
Computes either: 
1) the average Euclidean distance of connected nodal locations as defined in \code{G}, if \code{coords} are provided, 2) if \code{dist.matrix} is provided, the average nodal distances of a distance matrix provided in \code{dist.matrix} for nodes that remain in \code{G}, or 3) the instream distances of connected nodal locations if stream lenghts are provided in \code{lengths}.  For 3), the \code{length} vector will need to be trimmed as arcs disappear within intermittent streams (see Examples).
}
\value{Returns a global distance scalar.  See \bold{Details}.}
\references{
Ali, G. A., & Roy, A. G. (2010). Shopping for hydrologically representative connectivity metrics in a humid temperate forested catchment. \emph{Water Resources Research, 46}(12).

Western, A. W., Bloschl, G., & Grayson, R. B. (2001). Toward capturing hydrologically significant connectivity in spatial patterns. \emph{Water Resources Research, 37}(1), 83-97.
}
\author{
Ken Aho, Gabor Csardi wrote underlying functions \code{\link[igraph]{distances}} and \code{\link[igraph]{E}} 
}
\seealso{
\code{\link[igraph]{distances}}
}
\examples{
murphy_spring <- graph_from_literal(IN_N --+ M1984 --+ M1909, IN_S --+ M1993,  
M1993 --+ M1951 --+ M1909 --+ M1799 --+ M1719 --+ M1653 --+ M1572 --+ M1452, 
M1452 --+ M1377 --+ M1254 --+ M1166 --+ M1121 --+ M1036 --+ M918 --+ M823, 
M823 --+ M759 --+ M716 --+ M624 --+ M523 --+ M454 --+ M380 --+ M233 --+ M153, 
M153 --+ M91 --+ OUT)

#---- ICSL based on nodal Euclidean distances ----#
data(mur_coords)
ICSL(murphy_spring, coords = mur_coords[,2:3], names = mur_coords[,1])

#---- ICSL based on in-stream length data ----#
data(mur_lengths)
ICSL(murphy_spring, lengths = mur_lengths[,2], names = mur_coords[,1])

# or, simply
ms <- murphy_spring
E(ms)$weight <- mur_lengths[,2]
ICSL(ms)

# Arcs 1 and 3 dry
B <- graph_from_literal(IN_N, M1984, IN_S --+ M1993 --+ M1951 --+ M1909, 
M1909 --+ M1799 --+ M1719 --+ M1653 --+ M1572 --+ M1452 --+ M1377 --+ M1254,
M1254 --+ M1166 --+ M1121 --+ M1036 --+ M918 --+ M823 --+ M759 --+ M716, 
M716 --+ M624 --+ M523 --+ M454 --+ M380 --+ M233 --+ M153 --+ M91 --+ OUT)
ICSL(B, lengths = mur_lengths[,2][-c(1,3)], show.dist = TRUE)
}

