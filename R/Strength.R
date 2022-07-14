Strength <- function(G, nodes = V(G), mode = c("all", "out", "in"), weights = NULL){
  strength(G, vids = nodes, mode, loops = FALSE, weights = weights)
}