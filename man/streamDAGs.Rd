\name{streamDAGs}
\alias{streamDAGs}

\title{
Stream DAG datasets
}
\description{
Stream direct acyclic graph datasets written in \emph{igraph} format.  See: \code{\link[igraph]{graph_from_literal}}
}
\usage{

streamDAGs(graph = c("mur_full", "konza_full", "KD0521","KD0602","KD0621"))
}

\arguments{

  \item{graph}{
Currently, one of \code{"mur_full"}, \code{"konza_full"}, \code{"KD0521"},\code{"KD0602"}, or \code{"KD0621"}.  Option \code{"mur_full"} is a codification of the complete Murphy Creek dataset from the Owyhee Mountains in SW Idaho (\eqn{43.256^\circ}N, \eqn{116.817^\circ}W).  Option \code{"konza_full"} provides codification of a complete intermittent stream network of Konza Prairie in the northern Flint Hills region of Kansas (\eqn{39.11394^\circ} N, \eqn{96.61153^\circ}W).  Options \code{"KD0521"},\code{"KD0602"}, and \code{"KD0621"} provide networks for Konza at 05/21/2021 (before spring snow melt), 06/02/2021 (during spring snow melt) and 06/21/2021 (drying following snowmelt), respectively 
}
}
\author{
Ken Aho, Rob Ramos 
}
\examples{
streamDAGs("mur_full")
}
