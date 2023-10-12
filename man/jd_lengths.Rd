\name{jd_lengths}
\alias{jd_lengths}
\alias{jd_lengths_full}
\docType{data}
\title{
Lengths of Johnson Draw stream (arc) segments
}
\description{
Lengths of stream (arc) segments from Johnson Draw in southwest Idaho (outlet coordinates: 43.12256\eqn{^\circ}N, 116.77630\eqn{^\circ}W). The dataframe \code{jd_lengths} contains segment lengths in the absence of piezos [nodes are currently defined by STIC (Stream Temperature, Intermittency, and Conductivity) locations only] and thus correspond to the network in \code{streamDAGs("jd_full")}.  A network that includes piezos is depicted by \code{streamDAGs("jd_piezo_full")}.  

}
\usage{data("jd_lengths")}
\format{
  A data frame with observations on the following 2 variables.
  \describe{
    \item{\code{Arcs}}{Arc names, arrows directionally connect nodes.}
    \item{\code{Lengths}}{Lengths in meters}
  }
}
\source{
Arya Legg, Maggie Kraft
}
\keyword{datasets}

