\name{path.lengths}
\alias{path.lengths}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Path Lengths
}
\description{
Path lengths from all possible nodes to a designated node.
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

