size.intact.to.sink <- function(G, node = NULL){
  groups <- components(G, "weak")
  members <- groups$membership
  w <- which(names(members) == node)
  node.group <- which(members == members[w])
  node.members <- attributes(V(G))$names[node.group]
  sG <- subgraph(G, node.members)
  
  if(is.null(E(G)$weights)) 
    length <- ecount(sG)
    length <- sum(E(sG)$weights) 
  length
}


