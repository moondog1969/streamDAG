\name{bern.length}
\alias{bern.length}

\title{
Botter and Durighetto Bernoulli stream length
}
\description{
A simple function for calculating the dot product of a vector of stream arc lengths and a corresponding vector of either binary (stream presence or absence) outcomes, probabilities of stream presence or inverse probabilites of stream presence.
}
\usage{
bern.length(lengths, pa, mode = "local")
}

\arguments{
  \item{lengths}{
A numeric vector of stream arc lengths
}
  \item{pa}{
A numeric vector of either binary (stream presence or absence) outcomes, probabilities of stream presence or inverse probabilites of stream presence.  A vector outcome in \code{lengths} should correspond to an outcome for the same arc in \code{pa}.
}
  \item{mode}{
One of \code{"local"} of \code{"global"}
}
}
\value{
When \code{pa} is a vector of binary (stream presence or absence) data, the function provides a measure of instantaneous stream length (in the units used in \code{lengths}). When \code{pa} is a vector of probabilities of stream presence, the function provides average stream length (in units used in \code{lengths}). When \code{pa} is a vector of inverse probabilites of stream presence, the function provides average communication distance (in units used in \code{lengths}).
}
\references{
Botter, G., & Durighetto, N. (2020). The stream length duration curve: A tool for characterizing the time variability of the flowing stream length. \emph{Water Resources Research, 56}(8), e2020WR027282.
}
\author{
Ken Aho
}

\examples{
lengths <- rexp(10, 10)
pa <- rbinom(10, 11, 0.4)
bern.length(lengths, pa)
}
