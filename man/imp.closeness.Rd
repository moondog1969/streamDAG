\name{imp.closeness}
\alias{imp.closeness}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Improved Closeness Centrality
}
\description{
Calculates improved closeness centrality of individual nodes in a DAG.
}
\usage{
imp.closeness(G)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
    Graph object of class "igraph", see See \code{\link[igraph]{graph_from_literal}}.
}
}
\details{
\emph{Improved closeness centrality} (Beauchamp, 1965) was developed for weakly connected or disconnected digraphs.   The measure is based on the reciprocal of nodal shortest path distances from the \emph{j}th node to the \emph{k}th node, \eqn{1/\delta_{j,k}}.  For the \emph{j}th node this is:
\deqn{H_j=(n-1) \sum_{j \neq k} 1/\delta_{j,k}}
where, for disconnected nodes, the reciprocal distance \eqn{1/\infty} is taken to be zero.
}
\value{
Improved closeness centrality of a node
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Beauchamp, M. A. (1965). An improved index of centrality. \emph{Behavioral Science, 10}(2), 161-163.
}
\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{distances}}
}


\seealso{
\code{\link[igraph]{distances}}
}

\examples{
network_a <- graph_from_literal(a --+ b, c --+ d, d --+ e, b --+ e, 
e --+ j, j --+ m, f --+ g, g --+ i, h --+ i, i --+ k, k --+ l, 
l --+ m, m --+ n,  n --+ o)
imp.closeness(network_a)

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
