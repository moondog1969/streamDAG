delete.arcs.pa <- function(G, pa){
  w <- which(pa == 0)
  d <- delete.edges(G, as.character(w))
  d
}