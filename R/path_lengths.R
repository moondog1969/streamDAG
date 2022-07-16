path.lengths <- function(G, node = NULL){
  D <- distances(G, mode = "out") 
  length <- D[,colnames(D) == node]
  length[length != Inf & length != 0]
}