arc.pa.from.nodes <- function(G, node.pa, approach = "aho", na.rm = TRUE){
  if(length(attributes(E(G))$vnames) == 0) stop("graph has no arcs") 
    a <- attributes(A(G))$vnames
    v <- attributes(V(G))$names
    bounds <- matrix(ncol = 2, data = unlist(strsplit(a, "\\|")), byrow = TRUE)  
    names <- colnames(node.pa)
    
    arc.pa <- matrix(ncol = nrow(bounds), nrow = nrow(node.pa))
    
    for(i in 1:nrow(node.pa)){
    temp <- as.matrix(node.pa[i,])
    m1 <- 1:nrow(bounds) -> m2  
    for(j in 1:nrow(bounds)){ 
    m1[j] <- which(names == bounds[,1][j])
    m2[j] <- which(names == bounds[,2][j])
    }
   
    temp1 <- temp[m1]
    temp2 <- temp[m2]
    temp3 <- rbind(temp1, temp2)
    if(approach == "aho") arc.pa[i,] <- as.matrix(colMeans(temp3, na.rm = na.rm))
    if(approach == "dstream") arc.pa[i,] <- as.matrix(temp3[2,])
    if(approach == "ustream") arc.pa[i,] <- as.matrix(temp3[1,])
    }
  
  colnames(arc.pa) <- gsub("\\|", " -> ", a)
  arc.pa
  
  }