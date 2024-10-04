A.mult <- function(G, power, text.summary = TRUE){
  A <- as.matrix(G, matrix.type ="adjacency")
  out <- mat.pow(A, power)

  if(text.summary){
    u <- unlist(apply(out, 2, function(x) which(x != 0)))
    n <- length(u)
    content <- ifelse(out != 0, out, 0)
    w <- which(content != 0)
    summary1 <- matrix(nrow = n, data = unlist(strsplit("\\.", x = names(u))), byrow = T)
    summary2 <- paste(summary1[,2], " to ", summary1[,1], " (", content[w], " path(s))", sep = "")
    cat("\n")
    cat(paste("Paths of length ", power, ":", sep = ""),"\n")
    print(summary2)
  } 
else(out)
} 


n.tot.paths <- function(G, mode = "in"){
names <- attributes(V(G))$names
n <- length(names)
A.cum <- matrix(ncol = n, nrow = n, 0)

for(i in 1:n){
A.temp <- A.mult(G, power = i, text.summary = FALSE)
A.cum <- A.cum + A.temp
}
out <- matrix(ncol = n, nrow = n, A.cum)
rownames(out) <- names
colnames(out) <- names
if(mode == "out") res <- apply(out, 1, sum)
if(mode == "in") res <- apply(out, 2, sum)
res
}
  
