\name{arc.pa.from.nodes}
\alias{arc.pa.from.nodes}

\title{
Obtain arc stream activity outcomes based on bounding nodes
}
\description{
Given nodal water presence absence data \eqn{\in \{0,1\}} for a graph, \emph{G}, the function calculates arc water presence probabilities using particular rules (see approaches in Details). }

\usage{
arc.pa.from.nodes(G, node.pa, approach = "aho", na.rm = TRUE)
}

\arguments{

  \item{G}{
 Graph object of class \code{igraph}. See \code{\link[igraph]{graph_from_literal}}.}
 
  \item{node.pa}{A data frame or matrix of nodal presence absence data with column names corresponding to node names in \code{G}. 
}
\item{approach}{One of \code{"aho"}, \code{"dstream"}, or \code{"ustream"} (see Details).}
\item{na.rm}{For \code{approach = "aho"}, one of \code{TRUE} or \code{FALSE} indicating whether \code{NA} values should be stripped before calculating means.  Ignored for other appraches.}
}

\details{The \code{approach} argument currently supports three alternatives \code{"aho"}, \code{"dstream"} and \code{"ustream"}. Let \eqn{x_{k}} represent the \eqn{k}th arc with bounding nodes \eqn{u} and \eqn{v}.  

Under \code{approach = "aho"} there are three possibilities: 
\eqn{x_{k} = 1.0} if both \eqn{u} and \eqn{v} are wet, 
\eqn{x_{k} = 0} if both \eqn{u} and \eqn{v} are dry, and 
\eqn{x_{k} = 0.5} if only one of \eqn{u} or \eqn{v} is wet.


Under \code{approach = "dstream"}, \eqn{x_{k} = 1.0} if \eqn{v} is wet, and \eqn{x_{k} = 0} if \eqn{v} is dry. 

Conversely, if \code{approach = "ustream"}, \eqn{x_{k} = 1.0} if \emph{u} is wet, and \eqn{x_{k} = 0} if \emph{u} is dry.   
}


\value{Returns a matrix whose entries are estimated probabilities of success (e.g. surface water presence) based on the rules given in Aho et al. (2023).  Matrix columns specify arcs and rows typically represent time series observations.}  
\author{
Ken Aho
}
\references{Aho, K., Derryberry, D., Godsey, S. E., Ramos, R., Warix, S., Zipper, S. (2023) The communication distance of non-perennial streams. EarthArXiv \url{https://eartharxiv.org/repository/view/4907/}}
\examples{
murphy_spring <- graph_from_literal(IN_N --+ M1984 --+ M1909, IN_S --+ M1993,  
M1993 --+ M1951 --+ M1909 --+ M1799 --+ M1719 --+ M1653 --+ M1572 --+ M1452, 
M1452 --+ M1377 --+ M1254 --+ M1166 --+ M1121 --+ M1036 --+ M918 --+ M823, 
M823 --+ M759 --+ M716 --+ M624 --+ M523 --+ M454 --+ M380 --+ M233 --+ M153, 
M153 --+ M91 --+ OUT)

data(mur_node_pres_abs)
pa <- mur_node_pres_abs[400:405,][,-1]
arc.pa.from.nodes(murphy_spring, pa)
arc.pa.from.nodes(murphy_spring, pa, "dstream")
}
