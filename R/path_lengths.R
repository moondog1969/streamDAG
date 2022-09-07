path.lengths <- function(G, node = NULL, mode = "in", ignore.inf = TRUE){
  mode <- ifelse(mode == "in", "out", "in")
  D <- distances(G, mode = mode) 
  length <- D[,colnames(D) == node]
  if(ignore.inf)length <- length[length != Inf & length != 0]
  length
}