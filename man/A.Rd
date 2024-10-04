\name{A}
\alias{A}
\title{
Arcs of a directed graph
}
\description{
This function and its documentation have been lifted from the \emph{igraph} function \code{\link[igraph]{E}} with arguments according to DAG conventions. An arc sequence is a vector containing numeric arc ids, with a special class attribute that allows custom operations: selecting subsets of arcs based on attributes, or graph structure, creating the intersection, union of arcs, etc.  
}
\usage{
A(G, P, path)
}
\arguments{

  \item{G}{
    Graph object of class \code{igraph}. See \code{\link[igraph]{graph_from_literal}}.
}
  \item{P}{
A list of node to select arcs via pairs of nodes. The first and second nodes select the first arc, the third and fourth node select the second arc, etc.
}
  \item{path}{
A list of nodes, to select arcs along a path. Note that this only works reliably for simple graphs. If the graph has multiple arcs, one of them will be chosen arbitrarily to be included in the arc sequence.
}
}
\details{
Arc sequences are usually used as function arguments that refer to arcs of a graph.

An arc sequence is tied to the graph it refers to: it really denoted the specific arcs of that graph, and cannot be used together with another graph.

An arc sequence is most often created by the \code{A()} function. The result includes arcs in increasing arc id order by default (if none of the \code{P} and \code{path} arguments are used). An arc sequence can be indexed by a numeric vector, just like a regular R vector. 

}
\value{
An arc sequence of the graph.
}
\author{
Gabor Csardi
}
\seealso{
See \code{\link[igraph]{E}}
}
\examples{
 G <- graph_from_literal(a --+ b, c --+ d, d --+ e, b --+ e, e --+ j, j --+ m, f --+ g, g --+ i, 
 h --+ i, i --+ k, k --+ l, l --+ m, m --+ n,  n --+ o)
 
A(G)

}
