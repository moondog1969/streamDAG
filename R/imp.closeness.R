imp.closeness <- function(G){
  D <- distances(G, mode = "out")
  Dinv <- ifelse(D == Inf, 0, 1/D) 
  Dinv.1 <- ifelse(Dinv == Inf, 0, Dinv)
  n <- ncol(Dinv.1)
  apply((n - 1)*(Dinv.1), 2, sum) 
}