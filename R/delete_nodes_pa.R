delete.nodes.pa <- function(G, pa, na.response = "none"){
  NAs <- which(is.na(pa))
  if(length(NAs) > 0 & na.response == "none"){ 
    warning("NAs in data need to be addressed. NAs converted 0.")
    pa <- unlist(ifelse(is.na(pa), 0, pa))}
  if(na.response == "treat.as.0") pa <- unlist(ifelse(is.na(pa), 0, pa))
  if(na.response == "treat.as.1") pa <- unlist(ifelse(is.na(pa), 1, pa)) 
  g.nodes <- attributes(V(G))$names
  if(is.matrix(pa)){pa <- data.frame(pa)}
  if(!is.null(names(pa))){
    pa.nodes <- names(pa)
      if(any(pa.nodes !=  g.nodes)){
      warning("Node names in G and pa do not match.")}
  }
  w <- which(pa == 0)
  nodes <- g.nodes[w]
  d <- delete.vertices(G, nodes)
  if(length(NAs) > 0) d$NA.vertices <- attributes(V(G))$names[NAs]
  d
}