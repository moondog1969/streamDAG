harary <- function(G){
if(nrow(distances(G)) == 0){har = NA}
if(nrow(distances(G)) > 0){
  inv.dist <- 1/distances(G, mode = "in")
har <- 0.5 * sum(ifelse(inv.dist==Inf, 0, inv.dist))}
  har
}
