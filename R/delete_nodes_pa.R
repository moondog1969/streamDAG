delete.nodes.pa <- function(G, pa){
  w <- which(pa == 0)
  nodes <- attributes(V(G))$names[w]
  d <- delete.vertices(G, nodes)
  d
}