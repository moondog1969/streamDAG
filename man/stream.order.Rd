\name{stream.order}
\alias{stream.order}
\alias{sink.G}

\title{
Strahler or Shreve stream order of a stream DAG
}
\description{
The function \code{stream.order} calculates Strahler or Shreve number for each each in a stream DAG.  The function \code{sink.G} is a utility algorithm that subsets the graph if the sink node is part of a sub-graph that is disconnected from other nodes. 
}
\usage{
sink.G(G, sink = NULL)

stream.order(G, sink = NULL, method = "strahler")
}

\arguments{

  \item{G}{
  Graph object of class "igraph", see: See \code{\link[igraph]{graph_from_literal}}.
}
  \item{sink}{
  Sink node from \code{G}.
}
  \item{method}{
  One of \code{"strahler"} or \code{"shreve"}.
}
}
\details{

Strahler stream order (Strahler 1957) is a "top down" system in which first order stream sections occur at the outermost tributaries.  A stream section resulting from the merging of tributaries of the same order will have a Strahler number one unit greater than the order of those tributaries. A stream section resulting from the merging of tributaries of different order will have the Strahler stream order of the tributary with the larger Strahler number.   Under Shreve stream order, (Shreve 1966) a stream section resulting from the merging of tributaries will have an order that is the sum of the order of those tributaries.  

The function can currently only handle graphs with confluences (which, as noted above, serve to define the stream order) and simple islands (those without sub-islands and those whose downstream endpoint does not occur at a join). Under the current version, islands will not change the order of a reach.   
}
\value{
Returns Stahler or Shreve numbers for each stream DAG node.
}
\references{
Shreve, R. L. (1966). Statistical law of stream numbers. \emph{The Journal of Geology, 74}(1), 17-37.

Strahler, A. N. (1952). Hypsometric (area-altitude) analysis of erosional topology. \emph{Geological Society of America Bulletin, 63} (11): 1117-1142
}
\author{
Ken Aho
}
\note{
May be slow for extremely large and complex streams due to a reliance on loops.
}

\examples{

stream.order(G = streamDAGs("konza_full"), sink = "SFM01_1", method = "strahler")

stream.order(G = streamDAGs("konza_full"), sink = "SFM01_1", method = "shreve")

}

