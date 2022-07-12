delete.nodes.pa <- function(G, pa){
  w <- which(pa == 1)
  nodes <- attributes(V(G))$names[w]
  d <- delete.vertices(G, nodes)
  d
}