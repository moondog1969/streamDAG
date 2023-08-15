I.D <- function(G, mode = "gen.rand", alpha = -1/2, mult = FALSE, degrees = "out.in"){
  A <- as_adjacency_matrix(G)
  out.deg <- degree(G, mode = "out")
  in.deg <- degree(G, mode = "in")
  n <- length(out.deg)
  
  if(length(attributes(E(G))$vnames)==0){out <- NA}
  else{
  uv <- matrix(ncol = 2, unlist(strsplit(attributes(E(G))$vnames,"\\|")), byrow = TRUE)
  colnames(uv) <- c("u","v")
  inu <- in.deg[match(uv[,1],names(in.deg))]
  outu <- out.deg[match(uv[,1],names(out.deg))]
  inv <- in.deg[match(uv[,2],names(in.deg))]
  outv <- out.deg[match(uv[,2],names(out.deg))]
  
  if(degrees == "out.in"){
    x <- outu
    y <- inv
  }
  
  if(degrees == "in.in"){
    x <- inu
    y <- inv
  }
  
  if(degrees == "in.out"){
    x <- inu
    y <- outv
  }
  
  if(degrees == "out.out"){
    x <- outu
    y <- outv
  }
  
    if(mode == "gen.rand") omega <- (x * y)^alpha
  if(mode == "gen.sum.con") omega <- (x + y)^alpha
  if(mode == "harm") omega <- 2 * (x + y)^(-1)
  if(mode == "ABC") omega <- sqrt((x + y - 2)/(x * y))
  if(mode == "GA") omega <- sqrt(x * y)/(1/2 * (x + y))
  if(mode == "rand.aug") omega <- ((x * y)/(x + y -2))^3

  if(mult  == FALSE) out <- 1/2 * sum(omega)
  if(mult) out <- 1/2 * prod(omega)}
  out
}
