\name{streamDAGs}
\alias{streamDAGs}

\title{
Stream DAG datasets
}
\description{
Stream direct acyclic graph datasets written in \emph{igraph} format.  See: \code{\link[igraph]{graph_from_literal}}
}
\usage{

streamDAGs(graph = c("mur_full", "konza_full"))
}

\arguments{

  \item{graph}{
Currently, one of \code{"mur_full"} or code{"konza_full"}.  Option "mur_full" is a codification of the complete Murphy Creek dataset from the Owyhee Mountains in SW Idaho (\eqn{43.256^\circ}N, \eqn{116.817^\circ}W).  Option \code{"konza_full"} provides codification of a complete intermittent stream network in the northern Flint Hills region of Kansas (\eqn{39.11394^\circ} N, \eqn{96.61153^\circ}W).
}
}
\author{
Ken Aho, Rob Ramos 
}
\examples{
streamDAGs("mur_full")
}
