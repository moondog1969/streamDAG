#------------ Obtain n summodal summary from a DAG -------#
# G = graph object of class "igraph", see: ?igraph::graph_from_literal

local.summary <- function(G){
  if(length(V(G)) == 0){
  stop("Graph has no nodes or arcs")  
  }
  else{
  names <- attributes(V(G))$name
  path.means <- 1:length(names) -> path.vars -> path.kurt -> 
  path.skew -> n.paths -> height -> upstream.network.length

  for(i in 1:length(names)){
  temp <- path.lengths(G, node = names[i])
  path.means[i] <- mean(temp)  
  path.vars[i] <- var(temp)
  path.kurt[i] <- kurt(temp)
  path.skew[i] <- skew(temp)
  n.paths[i] <- length(temp)
  height[i] <- ifelse(length(temp) == 0, 0, max(temp))
  upstream.network.length[i] <- size.intact.to.node(G, node = names[i])
  }
 
  out <- data.frame(#indegree = degree(G, mode = "in"), 
                    alpha.cent = alpha.centrality(G),
                    page.rank = page_rank(G)$vector, 
                    imp.closeness.cent = imp.closeness(G),
                    betweenness.cent = betweenness(G),
                    # Strahler.number = stream.order(G, sink = sink, method = "strahler"),
                    # Shreve.number = stream.order(G, sink = sink, method = "shreve"),
                    n.paths = n.paths,
                    upstream.network.length = upstream.network.length,
                    in.path.length.mean = path.means,
                    in.path.length.var = path.vars,
                    in.path.length.skew = path.skew,
                    in.path.length.kurt = path.kurt,
                    in.eccentricity = height,
                    mean.efficiency = avg.efficiency(G))
                    
  row.names(out) <- names
  }
  t(out)
}
