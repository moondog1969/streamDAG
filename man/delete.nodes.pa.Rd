\name{delete.nodes.pa}
\alias{delete.nodes.pa}

\title{
Delete nodes based on presence absence data 
}
\description{
Create a new graph after deleting stream graph nodes based on presence/absence data, e.g., data based on outcomes from STIC (Stream Temperature, Intermittency, and Conductivity) loggers. 
}
\usage{

delete.nodes.pa(G, pa, na.response = "none")
}

\arguments{

  \item{G}{A graph object of class "igraph", see \code{\link[igraph]{graph_from_literal}}

}
  \item{pa}{
A vector of binary = 0,1 values indicating the absence or presence of nodes from \code{V(G)}.
}
\item{na.response}{
One of \code{"none"}, \code{"treat.as.0"}, or \code{"treat.as.0"}.
}
}
\details{A perennial problem with STIC (Stream Temperature, Intermittency, and Conductivity) sensors is the presence of missing data.  If \code{na.response = "none"} and NAs exist, then the function is stopped and the message \code{"NAs in data need to be addressed"} is printed.  One can also choose \code{na.response = "treat.as.0"} or \code{na.response = "treat.as.1"} which converts NAs to zeroes or ones.  Clearly, neither of these draconian approaches is optimal.  Thus, an attribute is added to the output graph object returned by function which lists the nodes with missing data. This attribute can be obtained with \code{out$$NA.vertices} where \code{out <- delete.nodes.pa(...)}, see Examples below.  An alternative is to use a classification algorithm for imputation e.g., \code{missForest::missForest}. 
}
\value{Returns a \emph{igraph} graph object, missing the nodes indicated with 0 in \code{pa}.}
\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{delete.vertices}}
}

\examples{
G <- graph_from_literal(a--+b--+c--+d--+e)
delete.nodes.pa(G, c(0,0,1,1,1)) 
# delete.nodes.pa(G, c(0,0,NA,1,1)) # stops function with warning 
d <- delete.nodes.pa(G, c(0,0,NA,1,1), "treat.as.0")
d
d$NA.vertices

}
