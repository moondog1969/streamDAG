\name{size.intact.to.sink}
\alias{size.intact.to.sink}
\alias{size.intact.to.node}

\title{
Size of intact network that feeds into the sink or a particular node
}
\description{
The length of the subgraph network that ends (feeds into) a particular node, e.g., the sink.  For a weighted graph, the sum of the weights of the subgraph are given.  Thus, if weights are stream lengths the function will give the stream length of the portion of the intact stream network that feeds into a particular node. 
}
\usage{
size.intact.to.sink(G, sink = NULL)

size.intact.to.node(G, node = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
A graph object of class "igraph", see \code{\link[igraph]{graph_from_literal}}  
}
  \item{sink}{
The sink node of \code{G}.
}
  \item{node}{
A node of interest.
}
}

\value{
Returns the size of the graph or subgraph that includes a \code{node} of interest.
}

\author{
Ken Aho, Gabor Csardi wrote several important function components including \code{\link[igraph]{subgraph}}. 
}

\examples{
# Murphy Cr. data, no arc from M1799 to M1719
G <- graph_from_literal(IN_N --+ M1984 --+ M1909, IN_S --+ M1993 --+ M1951, 
M1951 --+ M1909 --+ M1799, M1719 --+ M1653 --+ M1572 --+ M1452 --+ M1377, 
M1377 --+ M1254 --+ M1166 --+ M1121 --+ M1036 --+ M918 --+ M823 --+ M759, 
M759 --+ M716 --+ M624 --+ M523 --+ M454 --+ M380 --+ M233 --+ M153 --+ M91, 
M91 --+ OUT)

data(mur_coords) # coordinate data
spatial.plot(G, mur_coords[,2], mur_coords[,3], names =  mur_coords[,1])

data(mur_lengths) # segment length data 

lengths_new <- mur_lengths[-7,] # Drop M1799 -> M1719 arc length
E(G)$weight <- lengths_new[,2]
size.intact.to.sink(G, sink = "OUT")
size.intact.to.node(G, node = "OUT")
}
