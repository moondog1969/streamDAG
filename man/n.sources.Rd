\name{n.sources}
\alias{n.sources}
\alias{sources}
\alias{sink}

\title{
Identify source and sink nodes
}
\description{
Identify the number of sources and the source and sink nodes.  Sources are assumed to be linked to the sink.
}
\usage{

n.sources(G)

sources(G)

sink(G)
}

\arguments{

  \item{G}{
A graph object of class "igraph", see \code{\link[igraph]{graph_from_literal}
}
}
}

\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{degree}}
}

\examples{
sources(streamDAGs("konza_full"))
}

