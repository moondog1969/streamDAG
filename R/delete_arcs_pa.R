delete.arcs.pa <- function(G, pa){
  w <- which(pa == 1)
  arcs <- attributes(A(G))$vnames[w]
  d <- delete.edges(G, arcs)
  d
}