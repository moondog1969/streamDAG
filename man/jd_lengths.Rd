\name{jd_lengths}
\alias{jd_lengths}
\alias{jd_lengths_full}
\alias{jd_lengths_2023}
\docType{data}
\title{
Lengths of Johnson Draw stream (arc) segments
}
\description{
Lengths of stream (arc) segments from Johnson Draw in southwest Idaho (outlet coordinates: 43.12256\eqn{^\circ}N, 116.77630\eqn{^\circ}W). The dataframe \code{jd_lengths} contains segment lengths in the absence of piezos [nodes are currently defined by STIC (Stream Temperature, Intermittency, and Conductivity) locations only] and thus correspond to the network in \code{streamDAGs("jd_full")}. The dataframe \code{jd_lengths_full} contains segment lengths for node designated by both STICs and piezos. A corresponding network that includes piezos is depicted by \code{streamDAGs("jd_piezo_full")}. The dataframe \code{jd_lengths_2023} contains segment lengths for node designated by both STICs, piezos \emph{and} additional STC sites established in 2023. A correspding network can be obtained with \code{streamDAGs("jd_piezo_full_2023")}.   

}
\usage{data("jd_lengths")}
\format{
  A data frame with observations on the following 2 variables.
  \describe{
    \item{\code{Arcs}}{Arc names, arrows directionally connect nodes.}
    \item{\code{Lengths}}{Lengths in in km.}
  }
}
\source{
Arya Legg, Maggie Kraft
}
\keyword{datasets}

