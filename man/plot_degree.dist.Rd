\name{plot_degree.dist}
\alias{plot_degree.dist}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot degree distributions
}
\description{
Plots bserved degree distribution against models for uncorrelated random, chaotic and correlated random processes.
}
\usage{

plot_degree.dist(G, mode = "all", exp.lambda = c(1.1, 3/2, 2), leg.loc = "topright")
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
Graph object of class "igraph".  See \code{\link[igraph]{graph_from_literal}}
}
  \item{mode}{
 Character string, one of \code{"out"} for out-degree, \code{"in"} for in-degree or \code{"all"} for the sum of the two. For undirected graphs this argument is ignored.
}
  \item{exp.lambda}{
log.lamda = if not NULL, allows specification of chaotic \code{exp.lambda < 3/2} and correlated stochastic processes \code{exp.lambda < 3/2}
}
  \item{leg.loc}{
placement of \code{\link{legend}},
}
}

\value{
Plots processes for observed versus distributions under random or chaotic degrees.
}

\author{
Ken Aho
}

\seealso{
\code{\link{degree.dists}}, \code{\link[igraph]{degree.distribution}}
}
\examples{
network_a <- graph_from_literal(a --+ b, c --+ d, d --+ e, b --+ e, 
e --+ j, j --+ m, f --+ g, g --+ i, h --+ i, i --+ k, k --+ l, 
l --+ m, m --+ n,  n --+ o)
plot_degree.dist(network_a)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
