\name{delete.arcs.pa}
\alias{delete.arcs.pa}

\title{
Delete arcs based on presence absence data 
}
\description{
Create a new graph after deleting stream graph arcs based on presence/absence data, e.g., data based on outcomes from STIC (Stream Temperature, Intermittency, and Conductivity) loggers.
}
\usage{

delete.arcs.pa(G, pa)
}

\arguments{

  \item{G}{A graph object of class "igraph", see \code{\link[igraph]{graph_from_literal}}

}
  \item{pa}{
A vector of binary = 0,1 values indicating the absence or presence of arcs from \code{E(G)}.
}
}
\value{Returns a \emph{igraph} graph object missing the arcs indicated with 0 in \code{pa}.}
\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{delete.edges}}
}

\examples{
G <- graph_from_literal(a--+b--+c--+d--+e)
delete.arcs.pa(G, c(0,0,1,1)) 
}
