\name{isle}
\alias{isle}

\title{
Detects and defines islands in a streamDAG
}
\description{
The function was written primarilly to recognize DAG islands to allow correct implementation of the function \code{\link[streamDAG]{stream.order}} and is still early in its development.  
}
\usage{

isle(G)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
   Graph object of class \code{igraph}. See \code{\link[igraph]{graph_from_literal}}.
}
}
\details{
The function currently allows detection of simple island structures (those that don't contain sub-islands).  One of the output objects from the function is a new graph object with island nodes into a single node(s). 
}
\value{
Output consists of the following:
\item{test}{Logical indicating whether or not \code{G} contains islands.}
\item{island}{List of islands with their nodal components}
\item{input.id}{Neighboring node(s) directly upstream from island(s).}
\item{output.id}{Neighboring node(s) directly downstream from island(s).}
\item{new.graph}{New graph object created from \code{G} in which nodes constituing islands a combined into a single node}
\item{island.names}{Names of island nodes created in new output graph (that combines nodes constituing islands into a single node).  Follows the naming system \code{"i-1"}, \code{"i-2"}, etc.}
\item{splits}{The number of islands detected.}
}
\author{
Ken Aho
}
\seealso{
\code{\link[streamDAG]{stream.order}}, \code{\link[igraph]{delete.vertices}}, code{\link[igraph]{add.vertices}}, code{\link[igraph]{add.edges}}
}
\examples{
G <- graph_from_literal(a --+ c --+ e, b --+ d --+ e --+ f --+ p, g --+ i --+ j --+ m, 
i --+ k --+ m, m --+ n --+ o --+ p, h --+ l --+ n, p --+ q --+ r)
plot(G)
isle(G)
}
