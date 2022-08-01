\name{local.summary}
\alias{local.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
local (generally nodal) summaries of a DAG
}
\description{
Obtains local (generally nodal) summaries from a DAG
}
\usage{

local.summary(G)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
Graph of class "igraph".  See \code{\link[igraph]{graph_from_literal}}
}
}

\value{
Nodes are returned with values measuring the in degree, alpha centrality, improved path closeness, height, and path length mean, variability, skew, and kurtosis. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{degree}} and \code{\link[igraph]{alpha_centrality}} functions.
}



\seealso{
\code{\link[igraph]{degree}}, \code{\link[igraph]{alpha_centrality}}, \code{\link{imp.closeness}}
}

\examples{
network_a <- graph_from_literal(a --+ b, c --+ d, d --+ e, b --+ e, 
e --+ j, j --+ m, f --+ g, g --+ i, h --+ i, i --+ k, k --+ l, 
l --+ m, m --+ n,  n --+ o)
local.summary(network_a)

}

