A.mult <- function(G, power, text.summary = TRUE){
  A <- as.matrix(G, matrix.type ="adjacency")
  out <- mat.pow(A, 6)
  u <- unlist(apply(out, 2, function(x) which(x == 1)))
  n <- length(u)
  summary1 <- matrix(nrow = n, data = unlist(strsplit("\\.", x = names(u))), byrow = T)
  summary2 <- paste(summary1[,2], "to", summary1[,1])
  if(text.summary){
    cat("\n")
    cat(paste("Paths of length ", power, ":", sep = ""),"\n")
    print(summary2)
  } else(out)
} 
