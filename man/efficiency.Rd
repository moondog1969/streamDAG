\name{efficiency}
\alias{efficiency.matrix}
\alias{avg.efficiency}
\alias{global.efficiency}

\title{Local and global efficiency
}
\description{
Efficiency is the reciprocal of internodal distance.  Thus, the efficiency beween nodes \emph{i} and \emph{j} is defined as \eqn{e_{i,j} = \frac{1}{d_{i,j}}} where \eqn{d_{i,j}} denotes the distance between nodes \emph{i} and \emph{j} for all \eqn{i \neq j}.   
}
\usage{
efficiency.matrix(G, mode = "in")

avg.efficiency(G, mode = "in")

global.efficiency(G, mode = "in")
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
Graph object of class "igraph".  See \code{\link[igraph]{graph_from_literal}}.
}
  \item{mode}{
One of \code{"in"} or \code{"out"}.  The former considers in-path efficiencies, whereas the latter considers out-paths.
}
}
\details{
The function \code{efficiency.matrix} calculates an efficiency matrix whose elements correspond to elements in the graph distance matrix. The function \code{avg.efficiency} calculates average efficiencies of nodes to all other nodes, thus providing a local measure of graph connectedness. The function \code{global.efficiency} calculates the mean of the of all pairwise efficiencies, thus providing a global measure of graph connectedness. For all three functions, reciprocals of infinite distances are taken to be zero.   
}
\references{
Ek, B., VerSchneider, C., & Narayan, D. A. (2015). Global efficiency of graphs. \emph{AKCE International Journal of Graphs and Combinatorics, 12}(1), 1-13.
}
\author{
Ken Aho.  Gabor Csardi wrote the function \code{\link[igraph]{distances}} in \emph{igraph}.
}
\value{The function \code{efficiency.matrix} returns a reciprocal distance matrix for nodes in \code{G}. The function \code{avg.efficiency} treats efficiency as a local measure, and thus returns a vector whose entries are average efficiencies for each node.  The function \code{global.efficiency} returns a scalar (the mean of the reciprocal distance matrix).}
\examples{
kon_full <- streamDAGs("konza_full")
efficiency.matrix(kon_full)
avg.efficiency(kon_full)
global.efficiency(kon_full)
}

