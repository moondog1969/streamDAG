\name{path.lengths}
\alias{path.lengths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Path Lengths
}
\description{
Path lengths from all possible nodes to a designated node.  Weighted path length are possible, including weighted path lengths based on field-observed instream arc lengths (see Examples).  The result are "actual" path length in observed units.  
}
\usage{

path.lengths(G, node = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
Graph of class "igraph".  See \code{\link[igraph]{graph_from_literal}}
}
  \item{node}{
Designated node. 
}
}

\value{
Lengths of paths to a node of interest.  
}

\author{
Ken Aho , Gabor Csardi wrote \code{\link[igraph]{distances}}
}

\examples{
data(mur_lengths)
mur <- streamDAGs("mur_full")
path.lengths(mur, "M1653")
E(mur)$weight <- mur_lengths[,2] # weighted (actual in-stream lengths)
path.lengths(mur, "M1653")
}
