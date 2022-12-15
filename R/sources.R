n.sources <- function(G, sink = NULL){
  length(sources(G, sink = sink))
}

sources <- function(G, sink = NULL){
  d <- degree(G, mode = "in")
  w <- which(d == 0)
  SOURCES <- names(d[w])
  pl <- path.lengths.sink(G, sink = sink)
  pl.1 <- pl[pl != Inf]  
  eval <- which(SOURCES %in% names(pl.1))
  SOURCES[eval]
}

