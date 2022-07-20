max_r <- function(p1,p2){
  x <- max(p1,p2)
  y <- min(p1,p2)
  mr <- sqrt(y*(1-x)/(x*(1-y)))
  out <- ifelse(mr > 1, 1, mr)
  out
}