#------------ Assortativity coefficient ------------#
# G = graph object of class "igraph", see: ?igraph::graph_from_literal
# mode = c("in.in", "in.out", "out.out", "out.in", "all")

assort <- function(G, mode = "in.out"){
  cor.pop <- function(x, y){
    dev <- function(x) (x - mean(x))
    s.pop <- function(x) sqrt(sum(dev(x)^2)/length(x))
    (sum(dev(x) * dev(y))/(s.pop(x) * s.pop(y)))/length(x)
  }
  
  A <- as_adjacency_matrix(G)
  out.deg <- degree(G, mode = "out")
  in.deg <- degree(G, mode = "in")
  n <- length(out.deg)
  
  um <- matrix(ncol = n, rep(colnames(A),n), byrow = FALSE)
  vm <- matrix(ncol = n, rep(colnames(A),n), byrow = TRUE)
  w <- which(as.matrix(A) == 1)
  
  u.in.name <- um[w]; match.u.in <- match(u.in.name, names(in.deg))
  u.out.name <- um[w]; match.u.out <- match(u.out.name, names(out.deg))
  v.in.name <- vm[w]; match.v.in <- match(v.in.name, names(in.deg))
  v.out.name <- vm[w]; match.v.out <- match(v.out.name, names(out.deg))
  
  u.in <- in.deg[match.u.in]
  u.out <- out.deg[match.u.out]
  v.in <- in.deg[match.v.in]
  v.out <- out.deg[match.v.out]
  
  r.in.in <- cor.pop(u.in, v.in); if(mode == "in.in") out <- r.in.in
  r.in.out <- cor.pop(u.in, v.out); if(mode == "in.out") out <- r.in.out
  r.out.in <- cor.pop(u.out, v.in); if(mode == "out.in") out <- r.out.in
  r.out.out <-  cor.pop(u.out, v.out); if(mode == "out.out") out <- r.out.out
  if(mode == "all") out <- data.frame(in.in = r.in.in, in.out = r.in.out,
                                      out.in = r.out.in, out.out = r.out.out)
  out
}
