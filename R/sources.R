n.sources <- function(G){
  length(sources(G))
}

sources <- function(G){
  d <- degree(G, mode = "in")
  sin <- sink(G)
  w <- which(d == 0)
  SOURCES <- names(d[w])
  SINK <- sink(G)
  w.eval <- 1 : length(SOURCES)
  
  for(i in 1:length(SOURCES)){
   w.eval[i] <- length(unlist(suppressWarnings(shortest_paths(G, from = SOURCES[i], to = SINK)$vpath)))
  }
  
  eval <- w.eval > 1
  SOURCES[eval]
}

sink <- function(G){
  d <- degree(G, mode = "out")
  w <- which(d == 0)
  names(d[w])
}
