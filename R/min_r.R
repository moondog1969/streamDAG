min_r <- function(p1,p2){
  x <- max(p1,p2)
  y <- min(p1,p2)
  beta <- ifelse(x+y <= 1, 1/2, -1/2)
  mr <- -((x*y)/((1-x)*(1-y)))^beta
  out <- ifelse(mr < -1, -1, mr)
  out
}