\name{get.AIMS.data}
\alias{get.AIMS.data}

\title{
Loads AIMS dataset associated with a particular AIMS graph
}
\description{
The function creates a list of associated dataframes for particular AIMS graph objects. Currently these include one of more of \code{$coords} \code{$arc.length}
\code{$node.pa}.}
\usage{
get.AIMS.data(graph = "mur_full", supress.message = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{graph}{
A character string defining one of the AIMS graphs codified in \code{\link{streamDAGs}.}
}
  \item{supress.message}{
Logical. Supress message detailing objects created by function.
}
}
\details{
The function radically simplifies code gymnastics required to obtain datasets associated AIMS graphs (see, for instance, Detaails in \code{\link{streamDAGs}}).}
\value{
Returns a list containg up to three dataframes: 
\item{coords}{Spatial coordinates and other information from \code{\link{AIMS.node.coords}}.}
\item{arc.length}{Lengths of network, generally in km.}
\item{node.pa}{Presence(1)/absence(0) of surface water at the node.}
}

\author{
Ken Aho
}
\seealso{
\code{\link{streamDAGs}}
}
\examples{
jd <- get.AIMS.data("jd_full", TRUE)
head(jd$coords)
head(jd$arc.length)
head(jd$node.pa)
}
