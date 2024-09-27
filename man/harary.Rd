\name{harary}
\alias{harary}

\title{
Harary Index
}
\description{
Computes the Harary global metric for a stream DAG.
}
\usage{
harary(G)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{G}{Graph object of class \code{igraph}. See \code{\link[igraph]{graph_from_literal}}.
}
}
\details{
The Harary index is computed as:
\deqn{\frac{1}{2}\sum_i^m\sum_j^m (RD)_{ij}}
where \eqn{(RD)_{ij}} is the reciprocal of the \eqn{ij}th element of the graph distance matrix.  Reciprocals of infinite values in the distance matrix are taken to be zero.  Users should be aware that the graph object \code{G} is assumed to be DAG, and that distances are based on in-paths.   
}
\value{Returns a scalar: the global Harary index.}
\references{
Plavsic, D., Nikolic, S., Trinajstic, N., & Mihalic, Z. (1993). On the Harary index for the characterization of chemical graphs. Journal of Mathematical Chemistry, 12(1), 235-250.
}
\author{
Ken Aho, Gabor Csardi wote \code{\link[igraph]{distances}}
}

\examples{
harary(streamDAGs("konza_full"))
}
