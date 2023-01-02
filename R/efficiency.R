efficiency.matrix <- function(G, mode = "in"){
  if(mode == "in") mode <- "out"; if(mode == "out") mode <- "in"
  D <- distances(G, mode = mode)
  E <- 1/D; E <- ifelse(E == Inf, NA, E)
  E
}

avg.efficiency <- function(G, mode = "in"){
  E <- efficiency.matrix(G, mode = mode)
  apply(E, 1, function(x)mean(x, na.rm = TRUE))
}

global.efficiency <- function(G, mode = "in"){
  E <- efficiency.matrix(G, mode = mode)
  mean(E, na.rm = TRUE)
}