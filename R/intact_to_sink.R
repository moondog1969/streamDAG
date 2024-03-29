size.intact.to.sink <- function(G, sink = NULL){
  if(all(attributes(V(G))$name != sink)){
    length <- NA
  }
  else{
  groups <- components(G, "weak")
  members <- groups$membership
  w <- which(names(members) == sink)
  node.group <- which(members == members[w])
  node.members <- attributes(V(G))$names[node.group]
  sG <- subgraph(G, node.members)
  
  if(is.null(E(G)$weight)){length <- ecount(sG)}
  else {length <- sum(E(sG)$weight)}
  } 
  length
}


size.intact.to.node <- function(G, node = NULL){
  ndf <- function(G, node){
    sp <- distances(G, to = node, mode = "out")
    sp1 <- sp[,which(colnames(sp) == node)]
    node.members <- sp1[which(sp1 != Inf)]
    sG <- subgraph(G, names(node.members))
    if(is.null(E(G)$weight)){size <- ecount(sG)}
    else {size <- sum(E(sG)$weight)}
    size
  }
    if(node == "all"){
    nnames <- attributes(V(G))$names
      size <- 1:length(nnames)
      for(i in 1: length(nnames)){
      size[i] <- ndf(G, node = nnames[i])   
      }
      names(size) <- nnames
    }
    else{
      if(all(attributes(V(G))$name != node)){
        size <- NA
      }
      else size <- ndf(G, node) 
    }
  size
}