STIC.RFimpute <- function(p.a, ...){
  new <- data.frame(apply(p.a, 2, function(x) as.factor(x)), stringsAsFactors = TRUE)
  mf <- missForest(new, ...)
  mf
}