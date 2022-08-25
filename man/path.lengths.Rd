\name{path.lengths}
\alias{path.lengths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Path Lengths
}
\description{
Path lengths from all possible nodes to or from a designated node, i.e., in-paths and out-paths repsectively.  Weighted path length are possible, including weighted path lengths based on field-observed instream arc lengths (see Examples).  This results in "actual" path lengths in observed units.  
}
\usage{

path.lengths(G, node = NULL, mode = "in", ignore.inf = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
Graph of class "igraph".  See \code{\link[igraph]{graph_from_literal}}
}
  \item{node}{
Designated node. 
}

  \item{mode}{
One of \code{"in"} or \code{"out"}.  The former gives in-paths, whereas the latter gives out-paths.  
}

  \item{ignore.inf}{
Logical.  Whether infinite distances are to be ignored.  By default \code{ignore.inf = TRUE}, allowing impossible upstream distances to be ignored in stream DAGs.  
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
