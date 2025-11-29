\name{gj_lengths}
\alias{gj_lengths}
\alias{gj_lengths_piezo_full}
\docType{data}
\title{
Lengths of Gibson Jack stream (arc) segments
}
\description{
Lengths of stream (arc) segments from the Gibson Jack watershed in southeast Idaho (outlet coordinates: 42.767180\eqn{^\circ}N, 112.480240\eqn{^\circ}W).  The dataframe \code{gj_lengths} contains arc lengths for a network including STICs, but excluding piezometers. The dataframe \code{gj_lengths_piezo_full} contains arc lengths for a network that includes both STICs and piezometers. 
}
\usage{data("gj_lengths")}
\format{
  A data frame with 28 observations \code{jd_lengths} or 35 observations \code{jd_lengths_piezo_full} on the following 2 variables.
  \describe{
    \item{\code{Arcs}}{Arc names, arrows directionally connect nodes.}
    \item{\code{Lengths}}{Stream segment (arc) length in kilometers.}
  }
}

\source{
Maggie Kraft
}
\keyword{datasets}

