\name{Strength}
\alias{Strength}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Strength or weighted nodal degree.
}
\description{
Sums the weights of the adjacent arcs for each node.
}
\usage{

Strength(G, nodes = V(G), mode = c("all", "out", "in"), weights = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
The input graph
}
  \item{nodes}{
The nodes for which the strength will be calculated.
}
  \item{mode}{
Character string, \code{"out"} for the out degree, \code{"in"} for the in degree, or \code{"all"} for the sum of the two.
}
  \item{weights}{
  Weight vector. If the graph has a weight edge attribute, then this is used by default. If the graph does not have a weight edge attribute and this argument is NULL, then a warning is given and degree is called.
}
}

\value{
A numerical vector of the strength of the stream nodes.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\author{
Gabor Csardi
}
\seealso{
\code{\link[igraph]{strength}}
}
\examples{
G <- graph_from_literal(a --+ b, c --+ d, d --+ e, b --+ e, e --+ j, j --+ m, f --+ g, 
g --+ i, h --+ i, i --+ k, k --+ l, l --+ m, m --+ n,  n --+ o)
E(G)$weight <- rnorm(14,10,2) # define edege, i.e., arc weights

Strength(G, mode="out")
Strength(G, mode="in")
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
