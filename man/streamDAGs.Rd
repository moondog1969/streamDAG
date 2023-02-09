\name{streamDAGs}
\alias{streamDAGs}

\title{
Stream DAG datasets
}
\description{
Stream direct acyclic graph datasets written in \emph{igraph} format. See: \code{\link[igraph]{graph_from_literal}}
}
\usage{
streamDAGs(graph = c("mur_full", "konza_full", "KD0521", "KD0528","KD0604", 
"gj_full", "td_full"))
}
\arguments{
\item{graph}{
Currently, one of \code{"mur_full"}, \code{"konza_full"}, \code{"KD0521"}, \code{"KD0528"}, \code{"KD0604"}, \code{"gj_full"}, 
or \code{"td_full"}.  Option \code{"mur_full"} is an \emph{igraph} codification of the complete Murphy Creek dataset from the Owyhee Mountains in SW Idaho (outlet coordinates: 43.256\eqn{^\circ}N, 116.817\eqn{^\circ}W) established in 2019 by Warix et al. (2021), also see Aho et al. (2023).  Option \code{"konza_full"} provides codification of a complete intermittent stream network of Konza Prairie in the northern Flint Hills region of Kansas (outlet coordinates: 39.11394\eqn{^\circ}N, 96.61153\eqn{^\circ}W).  Options \code{"KD0521"},\code{"KD0528"}, and \code{"KD0604"} provide networks for Konza Prairie at 05/21/2021 (before spring snow melt), 05/28/2021 (during spring snow melt) and 06/04/2021 (drying following snow melt), respectively.  Option \code{"gj_full"} codifies nodes established at the Gibson Jack drainage as defined in 2016 (outlet coordinates: 42.767180\eqn{^\circ}N, 112.480240\eqn{^\circ}W) in southeast Idaho in 2016.  Option \code{"td_full"} codifies the Talladega stream network in southcentral Alabama (outlet coordinates: 33.76218\eqn{^\circ}N, 85.59552\eqn{^\circ}W).  
}
}
\references{
Aho, K., Derryberry, D., Godsey, S. E., Ramos, R., Warix, S., & Zipper, S. (2023). The communication distance of non-perennial streams.  \emph{EarthArvix} doi: 10.31223/X5Q367

Warix, S. R., Godsey, S. E., Lohse, K. A., & Hale, R. L. (2021), Influence of groundwater and topography on stream drying in semi-arid headwater streams. \emph{Hydrological Processes, 35}(5), e14185.
}
\author{
Ken Aho, Rob Ramos, Rebecca L. Hale, Charlie Bond 
}
\examples{
streamDAGs("mur_full")
}
