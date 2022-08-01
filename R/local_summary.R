#------------ Obtain n summodal summary from a DAG -------#
# G = graph object of class "igraph", see: ?igraph::graph_from_literal

local.summary <- function(G){
  names <- attributes(V(G))$name
  path.means <- 1:length(names) -> path.vars -> path.kurt -> 
  path.skew -> n.paths -> height

  for(i in 1:length(names)){
  temp <- path.lengths(G, node = names[i])
  path.means[i] <- mean(temp)  
  path.vars[i] <- var(temp)
  path.kurt[i] <- kurt(temp)
  path.skew[i] <- skew(temp)
  n.paths[i] <- length(temp)
  height[i] <- ifelse(length(temp) == 0, 0, max(temp))
  }
 
  out <- data.frame(indegree = degree(G, mode = "in"), 
                    alpha.centrality = alpha.centrality(G),
                    imp.closeness.centrality = imp.closeness(G),
                    n.paths = n.paths,
                    path.length.mean = path.means,
                    path.length.var = path.vars,
                    path.length.skew = path.skew,
                    path.length.kurt = path.kurt,
                    height = height)
  row.names(out) <- names
  t(out)
}
