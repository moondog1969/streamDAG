\name{n.sources}
\alias{n.sources}
\alias{sources}

\title{
Identify source and sink nodes
}
\description{
Identify the number of sources and the source nodes.  Sources are assumed to be linked to the sink.
}
\usage{
n.sources(G, sink = NULL)
sources(G, sink = NULL)
}

\arguments{

  \item{G}{
A graph object of class "igraph", see \code{\link[igraph]{graph_from_literal}}
}
 \item{sink}{
The name of the sink node.
}
}
\value{Returns a character vector listing streamDAG source nodes (those linked to the sink with indegree 0).}

\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{degree}}
}

\examples{
sources(streamDAGs("konza_full"), sink = "SFM01_1")
}

