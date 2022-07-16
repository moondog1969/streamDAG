R.bounds <- function(p, R){
  max.R <- R
  min.R <- R
  nodes <- length(p)
  R.replace <- R
  
  for(i in 1:(nodes-1)){
    for(j in (i+1):nodes){
      max.R[i,j] <- max_r(p[i],p[j])
      max.R[j,i] <- max_r(p[i],p[j])
    }
  }
  
  for(i in 1:(nodes-1)){
    for(j in (i+1):nodes){
      min.R[i,j] <- min_r(p[i],p[j])
      min.R[j,i] <- min_r(p[i],p[j])
    }
  }
  
  for(i in 1:(nodes-1)){
    for(j in (i+1):nodes){
      R.replace[i,j] <- ifelse(R[i,j] > max.R[i,j], max.R[i,j], R[i,j])
      R.replace[i,j] <- ifelse(R[i,j] < min.R[i,j], min.R[i,j], R.replace[i,j])
    }
  }
  R.replace
}