spath.lengths <- function(G, node = NULL, mode = "in", ignore.inf = TRUE){
  mode <- ifelse(mode == "in", "out", "in")
  D <- distances(G, mode = mode) 
  length <- D[,colnames(D) == node]
  if(ignore.inf)length <- length[length != Inf & length != 0]
  if(length(length) == 0) res <- NA else res <- length
  res
}

n.tot.paths <- function(G, mode = "in", sink = NULL){
  names <- attributes(V(G))$names
  n <- length(names)
  A.cum <- matrix(ncol = n, nrow = n, 0)
  
  for(i in 1:n){
    A.temp <- A.mult(G, power = i, text.summary = FALSE)
    A.cum <- A.cum + A.temp
  }
  out <- matrix(ncol = n, nrow = n, A.cum)
  rownames(out) <- names
  colnames(out) <- names
  if(mode == "out") res <- apply(out, 1, sum)
  if(mode == "in") res <- apply(out, 2, sum)
  if(!is.null(sink)) res <- res[names(res) == sink]  else res <- res
    res
}

