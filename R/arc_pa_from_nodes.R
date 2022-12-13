arc.pa.from.nodes <- function(G, node.pa){
  if(length(attributes(E(G))$vnames) == 0) stop("graph has no arcs") 
    a <- attributes(A(G))$vnames
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
    arc.pa[i,] <- as.matrix(colMeans(temp3, na.rm = TRUE))
    }
  
  colnames(arc.pa) <- gsub("\\|", " -> ", a)
  arc.pa
  
  }