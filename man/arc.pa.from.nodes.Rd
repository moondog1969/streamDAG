\name{arc.pa.from.nodes}
\alias{arc.pa.from.nodes}

\title{
Obtain arc activity outcomes based on bounding nodes
}
\description{
Given nodal presence absence outcomes for a graph, \emph{G}, the function calculates arc activity probabilities.  For the \emph{k}th arc with bounding nodes \emph{u} and \emph{v}, there are three possibilities for the arc: 1.0, if both \emph{u} and \emph{v} are present, 0.0 if both \emph{u } and \emph{v} are absent, and 0.5 if only one of \emph{u} or \emph{v} is present and the other is absent.
}

\usage{
arc.pa.from.nodes(G, node.pa)
}

\arguments{

  \item{G}{
 Graph object of class \code{igraph}. See \code{\link[igraph]{graph_from_literal}}.}
 
  \item{node.pa}{A data frame or matrix of nodal presence absence data with column names corresponding to node names in \code{G}. 
}
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
arc.pa.from.nodes(murphy_spring, mur_node_pres_abs[400:405,][,-1])
}
