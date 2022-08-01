biv.bern <- function(p11,p10,p01,p00,y1,y2){
  if(sum(p11,p10,p01,p00) != 1) stop("Joint probs. must sum to one")
  f.1.2 <- p11^(y1*y2)*p10^(y1*(1-y2))*p01^((1-y1)*y2)*p00^((1-y1)*(1-y2))
  f.1.2
}
