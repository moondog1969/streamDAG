\name{degree.dists}
\alias{degree.dists}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Potential degree distributions
}
\description{
Calculates degree distribution probability density.  By default calculates an uncorrelated (random) density for a given degree.  
}
\usage{

degree.dists(d, exp.lambda = 3/2, normalize = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{d}{
degree
}
  \item{exp.lambda}{
if not NULL, allows specification of chaotic \code{exp.lambda < 3/2} and correlated stochastic processes \code{exp.lambda < 3/2}.  
}
  \item{normalize}{
ensures that sum of demsities = 1
}
}
\details{
In general \eqn{f(d) = \exp(-\lambda d)} where \eqn{d} is the degree. For random degree distributions, \eqn{\lambda = \log(3/2)}.
}
\value{
Returns a density plot for a degree.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Ken Aho
}

\seealso{
\code{\link[igraph]{degree.distribution}}, \code{\link{plot_degree.dist}}.
}

