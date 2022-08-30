\name{global.summary}
\alias{global.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Global Summary
}
\description{
This function calculates useful DAG global summaries including size, diameter, number of paths to sink, mean path length, mean alpha centrality, mean PageRank centrality, Randic index, first Zagreb Index, second Zagreb index, atom-bond connectivity, geometric-arithmatic index, the harmonic index, the assortativity correlation (+, -), and the assortativity correlation (+, +).
}
\usage{
global.summary(G, sink, inf.paths = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
   graph object of class "igraph".  See \code{\link[igraph]{graph_from_literal}}.
}
  \item{sink}{
     sink node from graph G
}
  \item{inf.paths}{
    logical, consider infinite paths?
}
}
\details{
Simple global graph measures of complexivity and/or connectivity of a stream DAG include \emph{size}, \emph{diameter}, and number of paths to a sink. The \emph{size} is equal to the number of arcs in the stream network. The \emph{diameter} equals the lenght of the longest path, \emph{height} of the sink, and \emph{in eccentricity} of the sink. The number of paths to the sink is equivalent to the number of nodes from which the sink node is reachable, which will be \eqn{n-1} for a fully active stream. For more information om \eqn{I(D)} metrics see \code{\link{I.D}}.  Links describing other metrics are provided below.
}

\references{
Kunkler, S. J., LaMar, M. D., Kincaid, R. K., & Phillips, D. (2013). Algorithm and complexity for a network assortativity measure. arXiv Preprint \emph{arXiv:1307.0905}.

Das, K. C., Gutman, I., & Furtula, B. (2011). On atom-bond connectivity index. \emph{Chemical Physics Letters, 511}(4-6), 452-454.

Li, X., & Shi, Y. (2008). A survey on the randic index. \emph{MATCH Commun. Math. Comput. Chem, 59}(1), 127-156.
}
\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{alpha_centrality}} and other underlying functions.
}



\seealso{
\code{\link[igraph]{alpha_centrality}}, 
\code{\link{I.D}}, \code{\link{path.lengths}} 
}

\examples{
network_a <- graph_from_literal(a --+ b, c --+ d, d --+ e, b --+ e, 
e --+ j, j --+ m, f --+ g, g --+ i, h --+ i, i --+ k, k --+ l, 
l --+ m, m --+ n,  n --+ o)

global.summary(network_a, sink ="o")
}
