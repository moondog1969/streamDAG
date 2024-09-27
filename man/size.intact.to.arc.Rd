\name{size.intact.to.arc}
\alias{size.intact.to.arc}

\title{
Size of the intact network above an arc
}
\description{
The function measures the ``size'' of the intact network or sub-network (either number of upstream nodes, or user defined defined length, e.g., m, km) with respect to network arcs.  
}
\usage{
size.intact.to.arc(G, arc.node = "in")
}
\arguments{

  \item{G}{
Graph object of class "igraph", see: \code{\link[igraph]{graph_from_literal}}.
}
  \item{arc.node}{
One of \code{"in"} or \code{"out"}, indicating whether the upstream network or sub-network will be defined with respect to input nodes of directional arcs (\code{arc.node = "in"}) or output (end) nodes of arcs (\code{arc.node = "out"}).  The former (default) option is reccomended (see Details).}
}
\details{
For an unweighted graph, the upstream network ``size'' equates to the number of nodes in the intact network or sub-network upstream of an arc.  For a graph whose arcs are weighted with actual stream segment lengths (see Examples), this will be the length (in measured units of length given in the weights) of the intact network or sub-network upstream of the arc. The argument \code{arc.node} allows upstream network size to be calculated with respect to either the upstream (\code{"in"}) nodes of arcs or the downstream (\code{"out"}) nodes of arcs. This designation will be applied to define the end (outlet) of the network or sub-network.  Thus, option \code{"out"} may produce unexpectedly large results when these downstream \code{"out"} nodes of arcs occur at confluences.   
}
\value{
Output is a numeric vector whose length will be equal to the number of arcs in \code{G}. 
}
\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{distances}}
}

\seealso{
\code{\link{local.summary}}
}
\examples{
mur <- streamDAGs("mur_full")
data(mur_lengths)
E(mur)$weight <- mur_lengths[,2]
size.intact.to.arc(mur) # upstream network sizes are in meters
}
