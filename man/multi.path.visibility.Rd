\name{path.visibility}
\alias{visibility}
\alias{path.visibility}
\alias{multi.path.visibility}

\title{ 
Path Visibilities
}
\description{
Functions detect and summarize visibilities of path nodes from one or several source nodes to an sink.  Specifically, the function The function \code{path.visibility} determines path visibilities from single source node to a single sink. \code{multi.path.visibility} Generates tables of path visibilities and visibility summaries for multiple source nodes to a single sink. 

Ordering of nodes, vitally important to the calculation of visibility is currently obtained by identifying paths from each source node to the sink.  The sum of node distances in each path are then sorted decreasingly to define an initial order for calculating visibilities. It is currently assumed that the user will manually handle disconnected paths via the \code{source} argument of visibility functions.  Use of source nodes disconnected to the sink will result in the message: \code{"only use source nodes connected to sink"}. Because of this situation disconnected graphs will be handled by a function in development \code{single.node.visibility}.       
}
\usage{

path.visibility(G, degree = "in", source = NULL, sink = NULL, weights = NULL)

multi.path.visibility(G, degree = "in", source = NULL, sink = NULL, 
weights = NULL, autoprint = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
Graph of class "igraph".  See \code{\link[igraph]{graph_from_literal}}
}
  \item{degree}{
One of \code{"out"} for outdegree, \code{"in"} for indegree or \code{"all"} for the sum of the two.
}
  \item{source}{
A starting node for a path.  The function \code{multi.path.visibility} allows multiple starting nodes.
}
  \item{sink}{
An ending node for a path.
}
  \item{weights}{If \code{!null}, refers to a \eqn{1 \times n} \code{data.frame} of weights, with the \code{data.frame} \code{\link[base]{name}} attribute in \code{weights} corresponding to node names in \code{G}.
}

  \item{autoprint}{Logical.  Should table summary of nodal visibilities be automatically printed or made ?
}
}

\details{Following Lacasa et al. (2008), let \eqn{t_a} represent the occurrance number of the \eqn{a}th node in a time series or stream path, and let \eqn{y_a} represent a data outcome from the \eqn{a}th node. Nodes \eqn{a} and \eqn{b} will have visibility if all other data, \eqn{y_c}, between \eqn{a} and \eqn{b} fufill:

\deqn{y_c < y_b + (y_a - y_b)\frac{t_b - t_c}{t_b - t_a}.}
}


\value{
The function \code{path.visibility} returns a symmetric matrix whose upper triangle denotes nodal co-visibilities.  The lower triangle is left empty for efficiency.  Reading down a column in the upper triangle shows upstream visibilites to and from a node, while reading across rows shows downstream visibilities. 

The function \code{multi.path.visibility} returns a list containing the three objects.  The first is printed and the latter two are \code{\link[base]{invisible}} by default.   
\item{\code{visibility.summary}}{The printed result is a matrix of path visibity counts for a node, with respect to upstream (to), downstream (from), and combined directions (both).}
\item{\code{complete.matrix}}{Analogous, to \code{path.visibility}, this result attempts to synthesize visibilities within source-to-sink paths for all requested sources into a single matrix. }
\item{\code{all.matrices}}{A list containing \code{path.visibility} summary matrices for each source-to-sink path.}

Output is summarized based on a deduced ordering of nodes from sources to sin.  The ordering is based on nodal path lengths.
}

\references{
Lacasa, L., Luque, B., Ballesteros, F., Luque, J., & Nuno, J. C. (2008). From time series to complex networks: The visibility graph. \emph{Proceedings of the National Academy of Sciences}, 105(13), 4972-4975.
}

\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{degree}} and \code{\link[igraph]{shortest_paths}}.
}


\seealso{
\code{\link[igraph]{degree}}, \code{\link[igraph]{shortest_paths}}
}

\examples{

A <- graph_from_literal(a --+ b, c --+ d, d --+ e, b --+ e, 
e --+ j, j --+ m, f --+ g, g --+ i, h --+ i, i --+ k, k --+ l, 
l --+ m, m --+ n,  n --+ o)

path.visibility(A, source = "a", sink = "o")
  
multi.path.visibility(A, source = c("a","c","f","h"), 
sink = "o")

# From Lacasa et al. (2008)

B <- graph_from_literal(a --+ b --+ c --+ d --+ e --+ f --+ g)
weights <- data.frame(matrix(nrow = 1, data = c(0.87, 0.49, 0.36, 0.83, 0.87, 0.49, 0.36))) 
names(weights) = letters[1:7]
path.visibility(B, source = "a", sink = "g", weights = weights) 
}

