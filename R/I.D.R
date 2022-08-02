I.D <- function(G, mode = "gen.rand", alpha = -1/2, mult = FALSE){
  A <- as_adjacency_matrix(G)
  out.deg <- degree(G, mode = "out")
  in.deg <- degree(G, mode = "in")
  n <- length(out.deg)
  
  outm <- matrix(ncol = n, rep(colnames(A),n), byrow = FALSE)
  inm <- matrix(ncol = n, rep(colnames(A),n), byrow = TRUE)
  w <- which(as.matrix(A) == 1)
  
  outmw <- outm[w]; matchOut <- match(outmw, names(out.deg))
  inmw <- inm[w]; matchIn <- match(inmw, names(in.deg))
  
  x <- out.deg[matchOut]
  y <- in.deg[matchIn]
  
  if(mode == "gen.rand") omega <- (x * y)^alpha
  if(mode == "sum.con") omega <- (x + y)^alpha
  if(mode == "ABC") omega <- sqrt((x + y - 2)/(x * y))
  if(mode == "GA") omega <- sqrt(x * y)/(1/2 * (x + y))
  if(mode == "harm") omega <- 2/(x + y)
  
  if(mult  == FALSE) out <- 1/2 * sum(omega)
  if(mult) out <- 1/2 * prod(omega)
  out
}
