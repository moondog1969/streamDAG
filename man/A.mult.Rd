\name{A.mult}
\alias{A.mult}

\title{
Raise an adjacency matrix to some power
}
\description{
When applying the definition of matrix multiplication to an adjacency matrix 
\eqn{\mathbf{A}}, the \eqn{i,j} entry in \eqn{\mathbf{A}^k} will give the number of paths in the graph from node \eqn{i} to node \eqn{j} of length \eqn{k}.
}
\usage{
A.mult(G, power, text.summary = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
    Graph object of class \code{igraph}. See \code{\link[igraph]{graph_from_literal}}.
}
  \item{power}{
The power to rise the adjacency matrix to.
}
  \item{text.summary}{
Logical.  If \code{TRUE} the function returns a summary of the paths of length \code{power}.  If \code{FALSE}.  The adjacency matrix raised to \code{power} is returned. 
}
}
\value{Returns either a character vector of paths of a specified length or, if \code{text.summary = TRUE}, the adjacency matrix raised to a specified power.}
\author{
Ken Aho
}
\examples{
kon_full <- streamDAGs("konza_full")
A.mult(kon_full, power = 6)
}

