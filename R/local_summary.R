#------------ Obtain nodal summary from a DAG -------#



local.summary <- function(G, metric = "all", mode = "in"){
  if(length(V(G)) == 0) stop("Graph has no nodes or arcs")  
  string <- c("all", "alpha.cent", "page.rank", "arc.betweenness", "betweenness", "imp.closeness", "n.paths", "n.nodes", "path.len.summary", "path.deg.summary", "size.intact.in", "avg.efficiency")
  
  w <- pmatch(metric, string)
  which <- string[w]
  
  names <- attributes(V(G))$name
  path.means <- 1:length(names) -> path.length.mean -> path.length.var -> path.length.kurt -> path.length.skew -> n.nodes -> n.paths -> height -> upstream.network.length -> path.degree.mean -> path.degree.var -> path.degree.kurt -> path.degree.skew
  

  varP <- function(x){
    x <- na.omit(x)
    sum((x - mean(x))^2)/length(x)
  }
    
  
  get.degree <- function(G, mode = mode, dnames){
    if(is.null(dnames)) out <- NA
    else{omits <- which(!names %in% dnames)
    G.new <- delete_vertices(G, omits)
    out <- degree(G.new)
    }
    out
  }
  
  
  if(which == "all" | which == "path.len.summary" | which == "n.nodes"| which == "size.intact.in"){
    for(i in 1:length(names)){
      temp <- spath.lengths(G, node = names[i])
      if(mode == "out") temp <- spath.lengths(G, node = names[i], mode = "out")
      if(all(is.na(temp))){temp <- temp} else {temp <- na.omit(temp)}
      path.length.mean[i] <- ifelse(all(is.na(temp)), 0, mean(temp))  
      path.length.var[i] <- varP(temp)
      path.length.kurt[i] <- kurt(temp)
      path.length.skew[i] <- skew(temp)
      n.nodes[i] <- length(temp)
      height[i] <- ifelse(all(is.na(temp)), 0, max(temp))
      upstream.network.length[i] <- out <- size.intact.to.node(G, node = names[i])
    }
  } 
  
  
  
  if(which == "all" | which == "path.deg.summary"){
    for(i in 1:length(names)){
      temp <- spath.lengths(G, node = names[i])
      if(mode == "out") temp <- spath.lengths(G, node = names[i], mode = "out")
      gdtemp <- get.degree(G, mode = mode, names(temp))
      path.degree.mean[i] <- mean(gdtemp)  
      path.degree.var[i] <- varP(gdtemp)
      path.degree.kurt[i] <- kurt(gdtemp)
      path.degree.skew[i] <- skew(gdtemp)
    }
  }  
  
  if(which == "n.nodes"){out <- n.nodes; names(out) <- names}
  if(which == "size.intact.in"){out <- upstream.network.length; names(out) <- names}
  if(which == "all" | which == "n.paths"){ 
    if(mode == "in"){out <- n.paths <- n.tot.paths(G, "in")}
    else if(mode == "out"){out <- n.paths <- n.tot.paths(G, "out")}}
  if(which == "all" | which == "alpha.cent") out <- alpha.cent <- alpha.centrality(G)
  if(which == "all" | which == "page.rank") out <- page.rank <- page_rank(G)$vector 
  if(which == "all" | which == "imp.closeness") out <- imp.closeness.cent <- imp.closeness(G)
  if(which == "all" | which == "betweenness") out <- betweenness.cent <- betweenness(G)
  if(which == "all" | which == "arc.betweenness") {out <- edge_betweenness(G); names(out) <- attributes(E(G))$vname}
  if(which == "all" | which == "avg.efficiency" | which == "path.len.summary"){
    if(mode == "in"){out <- mean.efficiency <- avg.efficiency(G)}
    else if(mode == "out"){out <- mean.efficiency <- avg.efficiency(G, "out")}}
  
  
  if(which == "path.deg.summary"){
    out <- data.frame(
      path.degree.mean =  path.degree.mean,
      path.degree.var = path.degree.var,
      path.degree.skew = path.degree.skew,
      path.degree.kurt = path.degree.kurt)
    
    row.names(out) <- names
    
  } 
  
  
  if(which == "path.len.summary"){
    out <- data.frame(n.nodes.in.path = n.nodes,
                      n.paths = n.paths,
                      upstream.network.length = upstream.network.length,
                      path.length.mean =  path.length.mean,
                      path.length.var = path.length.var,
                      path.length.skew = path.length.skew,
                      path.length.kurt = path.length.kurt,
                      eccentricity = height,
                      mean.efficiency = mean.efficiency)
    row.names(out) <- names
      } 
  
  
  if(which == "all"){
    out <- data.frame(alpha.cent, page.rank, imp.closeness.cent, betweenness.cent,
                      n.nodes.in.paths = n.nodes,
                      n.paths = n.paths,
                      upstream.network.length = upstream.network.length,
                      path.length.mean =  path.length.mean,
                      path.length.var = path.length.var,
                      path.length.skew = path.length.skew,
                      path.length.kurt = path.length.kurt,
                      path.degree.mean =  path.degree.mean,
                      path.degree.var = path.degree.var,
                      path.degree.skew = path.degree.skew,
                      path.degree.kurt = path.degree.kurt,
                      in.eccentricity = height,
                      mean.efficiency = mean.efficiency)
    out <- t(out)
      }
  if(which != "path.len.summary" & which != "path.deg.summary" & which != "all"){
    out <- as.matrix(out)
    colnames(out) <- which
  }
  out
}
