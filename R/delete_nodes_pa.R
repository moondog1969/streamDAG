delete.nodes.pa <- function(G, pa, na.response = "none"){
  NAs <- which(is.na(pa))
  if(length(NAs) > 0 & na.response == "none"){ 
    stop("NAs in data need to be addressed")}
  if(na.response == "treat.as.0") pa <- unlist(ifelse(is.na(pa), 0, pa))
  if(na.response == "treat.as.1") pa <- unlist(ifelse(is.na(pa), 1, pa)) 
  w <- which(pa == 0)
  nodes <- attributes(V(G))$names[w]
  d <- delete.vertices(G, nodes)
  if(na.response != "none") d$NA.vertices <- attributes(V(G))$names[NAs]
  d
}