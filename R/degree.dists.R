degree.dists <- function(d, exp.lambda = 3/2, normalize = TRUE){
  f.d <- exp(-log(exp.lambda) * d)  
  if(normalize & length(f.d) > 1) f.d/sum(f.d) else f.d
}