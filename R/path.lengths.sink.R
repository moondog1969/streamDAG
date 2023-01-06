path.lengths.sink <- function(G, sink = NULL, inf.paths = TRUE){
  D <- distances(G, mode = "out")
  sink.dist <- D[,which(colnames(D) == sink)]
  out <- sink.dist[which(sink.dist != 0)]
  if(!inf.paths) out <- out[which(out != Inf)]
  out
}
