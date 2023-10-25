\name{AIMS.node.coords}
\alias{AIMS.node.coords}
\docType{data}
\title{
Nodal coordinates for graphs in the AIMS project 
}
\description{
Contains spatial coordinates for graph nodes for stream networks in the Aquatic Intermittency effects on Microbiomes in Streams (AIMS) project 
}
\usage{data("AIMS.node.coords")}
\format{
  A data frame with 307 observations on the following 7 variables.
  \describe{
    \item{\code{Object.ID}}{Nodal identifier}
    \item{\code{lat}}{Latitude}
    \item{\code{long}}{Longitude}
    \item{\code{site}}{Stream network name, currently includes: \code{"KZ"} = Konza Prairie, \code{"TD"} = Talladega, \code{"WH"} = Weyerhauser, \code{"PR"} = Painted Rock, \code{"JD"} = Johnson Draw, \code{"DC"} = Dry Creek, and \code{"GJ"} = Johnson Draw.}
    \item{\code{piezo}}{Logical, indicating whether the location contains a peizometer.}
    \item{\code{microbial_seasonal_network}}{Logical, whether the location was sampled as part of AIMS seasonal microbial sampling.}
    \item{\code{STIC_inferred_PA}}{Logical, whether surface water presence/absence data were obtained from STIC  (Stream Temperature, Intermittency, and Conductivity) sensors at the location.}
  }
}
\keyword{datasets}

