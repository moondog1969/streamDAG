\name{jd_lengths}
\alias{jd_lengths}
\alias{jd_lengths_full}
\docType{data}
\title{
Lengths of Johnson Draw stream (arc) segments
}
\description{
Lengths of Johnson Draw stream (arc) segments. The dataframe \code{jd_lengths} contains segment lengths in the absence of piezos (nodes are defines by STIC locations only) and thus corresponds to the network in \code{streamDAGs("jd_stic_full")}.  The dataframe \code{jd_lengths_full} corresponds to a network that includes piezos and thus is represented by the network \code{streamDAGs("jd_full")}.  

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

