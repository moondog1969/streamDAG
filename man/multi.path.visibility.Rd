\name{path.visibility}
\alias{path.visibility}
\alias{multi.path.visibility}

\title{ 
Path Visibilities
}
\description{
Shows visibilities of path nodes from one or several source nodes to an sink. 
}
\usage{

path.visibility(G, degree = "in", source = NULL, sink = NULL, directed = TRUE)

multi.path.visibility(G, degree = "in", source = NULL, sink = NULL, directed = TRUE, 
weights = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
Graph of class "igraph".  See See \code{\link[igraph]{graph_from_literal}}
}
  \item{degree}{
One of \code{"out"} for out-degree, \code{"in"} for in-degree or \code{"all"} for the sum of the two.
}
  \item{source}{
A starting node for a path.  The function \code{multi.path.visibility} allows multiple starting nodes.
}
  \item{sink}{
An ending node for a path.
}
  \item{directed}{
Logical, indicating if \code{G} is a directed graph.
}
  \item{weights}{
Refers to \eqn{n \times 1} matrix of weights, with rownames corresponding to node names in \code{G}.
}
}

\value{
\code{multi.path.visibility} Generates tables of path visibilities and visibility summaries for multiple sources to a single sink. The function \code{path.visibility} calculates path visibilities from single source to a single sink.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{degree}}
}


\seealso{
\code{\link[igraph]{degree}}
}

\examples{

network_a <- graph_from_literal(a --+ b, c --+ d, d --+ e, b --+ e, 
e --+ j, j --+ m, f --+ g, g --+ i, h --+ i, i --+ k, k --+ l, 
l --+ m, m --+ n,  n --+ o)
  
multi.path.visibility(network_a, source = c("a","c","f","h"), 
sink = "o")

weights <- matrix(nrow = 15, runif(15,0,1), 
dimnames = list(letters[1:15], "weights"))
multi.path.visibility(network_a, source = c("a","c","f","h"), 
sink = "o", weights = weights)

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
