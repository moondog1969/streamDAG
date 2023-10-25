\name{path.lengths.sink}
\alias{path.lengths.sink}

\title{
Path Lengths
}
\description{
Obtains all shortest in paths to a sink
}
\usage{
path.lengths.sink(G, sink = NULL, inf.paths = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
  Graph object of class "igraph", see: \code{\link[igraph]{graph_from_literal}}.
}
  \item{sink}{
  sink node from \code{G}.
}
  \item{inf.paths}{
Logical, consider infinite paths?
}
}

\value{ Length of path to a sink
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{distances}}
}

\examples{
murphy_spring <- graph_from_literal(IN_N --+ M1984 --+ M1909, IN_S --+ M1993, 
M1993 --+ M1951 --+ M1909 --+ M1799 --+ M1719 --+ M1653 --+ M1572 --+ M1452,
M1452--+ M1377 --+ M1254 --+ M1166 --+ M1121 --+ M1036 --+ M918 --+ M823, 
M823 --+ M759 --+ M716 --+ M624 --+ M523 --+ M454 --+ M380 --+ M233 --+ M153,
M153 --+ M91 --+ OUT)

path.lengths.sink(murphy_spring, sink = "OUT")

# with stream lengths as weights
data(mur_lengths)

E(murphy_spring)$weights <- mur_lengths[,2]
path.lengths.sink(murphy_spring, "OUT")
}
