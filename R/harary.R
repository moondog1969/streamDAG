harary <- function(G){
inv.dist <- 1/distances(G, mode = "in")
0.5 * sum(ifelse(inv.dist==Inf, 0, inv.dist))
}
