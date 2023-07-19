path.visibility <- function(G, degree = "in", source = NULL, sink = NULL, weights = NULL){
  if(!is.connected(G)) stop("G is disconnected.  Use function single.node.visibility (in development)")
  if(is.null(source) | is.null(sink)) stop("source and sink must be defined")
  if(any(distances(G, source, sink)==Inf)) stop("only use source nodes connected to sink")
  ts <- shortest_paths(G, source, sink)[[1]][[1]]
  path <- names(ts)
  deg <- degree(G, mode = degree)
  if(!is.null(weights)){
  if(all(sort(names(weights)) != sort(attributes(V(G))$names))){stop("row names in weights must correspond to node names in G")}
  deg <- weights}
  w <- match(path, names(deg))
  y <- deg[w]
  n <- length(w)
  if(n == 1) stop("source and sink must be distinct")
  time <- 1:n
  vis.alg <- matrix(FALSE, nrow = n, ncol = n, dimnames = list(path, path))
  diag(vis.alg) <- NA
  vis.alg[lower.tri(vis.alg)] <- NA
  
  if(n > 2){
  for(i in 1:(n-1)){
    vis.alg[i,][(i+1)] <- TRUE}
  
  vis <- function(ya,yb,yc,ta,tb,tc){
    yc < yb + (ya - yb) * ((tb - tc)/(tb - ta))
  }
  for(i in 1:(n-2)){
    for(j in (i+2):(n)){
      out <- 1:length((i+1):(j-1))
      k <- i+1
      for(m in 1:length(out)){
        out[m] <- data.frame(vis(y[i],y[j],y[k], time[i], time[j], time[k]))
        
        k <- k+1
      } 
      out <- unlist(out)
      vis.alg[i,][j] <- ifelse(any(!out), FALSE, TRUE)
    }
  }
  # if(!directed)  {
  #   mat <- vis.alg
  #   for(i in 1:(ncol(vis.alg)-1)){ 
  #     for(j in (i+1):(ncol(vis.alg)-1)){ 
  #       mat[,i][j] <- vis.alg[i,][j]  
  #     }
  #     vis.alg <- mat 
  #   }  
  # }
  }
  if(n == 2){vis.alg <- matrix(ncol = 2, c(NA,NA,TRUE,NA), dimnames = list(path, path))}
  vis.alg
}
