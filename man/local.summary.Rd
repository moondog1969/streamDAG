\name{local.summary}
\alias{local.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
local (nodal) summaries of a DAG
}
\description{
Obtains local (nodal) summaries from a DAG
}
\usage{

local.summary(G, metric = "all", mode = "in")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{G}{
Graph of class "igraph".  See \code{\link[igraph]{graph_from_literal}}
}
\item{metric}{One of \code{"all"}, \code{"alpha.cent"}, \code{"imp.closeness"}, \code{"n.paths"}, \code{"n.nodes"}, \code{"page.rank"}, \code{"path.len.summary"}, \code{"path.deg.summary"}, \code{"size.intact.in"}.  Partial matches allowed. 
}
\item{mode}{One of \code{"in"} or \code{"out"}} 
}

\value{
Nodes are returned with values measuring the indegree, alpha centrality, PageRank centrality, improved closeness centrality, betweenness centrality, upstream network length, and upstream in-path length mean, variance, max (i.e., in-eccentricity), skew, kurtosis, and mean efficiency. 
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{degree}}, \code{\link[igraph]{page_rank}} and \code{\link[igraph]{alpha_centrality}} functions.
}



\seealso{
\code{\link[igraph]{degree}}, \code{\link[igraph]{alpha_centrality}}, \code{\link[igraph]{page_rank}}, \code{\link[igraph]{betweenness}}, 
\code{\link{imp.closeness}},
\code{\link[asbio]{skew}}, \code{\link[asbio]{kurt}}
}

\examples{
network_a <- graph_from_literal(a --+ b, c --+ d, d --+ e, b --+ e, 
e --+ j, j --+ m, f --+ g, g --+ i, h --+ i, i --+ k, k --+ l, 
l --+ m, m --+ n,  n --+ o)
local.summary(network_a)
}

