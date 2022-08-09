multi.path.visibility <- function(G, degree = "in", source = NULL, sink = NULL, directed = TRUE, weights = NULL){
  list.paths <- vector(mode='list', length = length(source)) 
  list.names <- vector(mode='list', length = length(source)) 
  list.t <- vector(mode='list', length = length(source)) 
  for(i in 1:length(source)){ 
    out <- path.visibility(G, degree = degree, source = source[i], sink = sink, directed = directed)
    list.paths[[i]] <- out
    list.names[[i]] <- colnames(out)
    list.t[[i]] <- data.frame(Names = colnames(out), Time = 1:length(colnames(out)))
  }
  
  time.matrix <- list.t[[1]]
  for(i in 2:length(list.t)){
    temp <- rbind(time.matrix, list.t[[i]])
    time.matrix <- temp
  }  
  
  v <- list.paths  
  time.summary <- sort(tapply(time.matrix[,2], time.matrix[,1], sum))
  ts.mat <- matrix(FALSE, ncol = length(time.summary), nrow = length(time.summary), dimnames = list(names(time.summary), names(time.summary)))
  
  
  for(i in 1:length(v)){
    one <- v[[i]]
    w <- which(colnames(ts.mat) %in% colnames(one))
    ts.mat[,w][w,] <- one
  }
  
  list.out <- list()
  # list.out$visibilities <- list.paths
  # list.out$names <- unique(unlist(list.names))
  list.out$all.visibilities <- ts.mat
  
  fr <- apply(ts.mat, 1, function(x)sum(x,na.rm=T))
  to <- apply(ts.mat, 2, function(x)sum(x,na.rm=T))
  sum <- to + fr
  summary <- rbind(fr,to,sum)
  row.names(summary) <- c("out", "in", "both")
  if(!is.null(weights)){ if(all(sort(row.names(weights)) == sort(row.names(summary)))) stop("row names in weights must correspond to node names in G")
    m <- match(colnames(summary), row.names(weights))
    out <- summary
    for(i in 1:3){
      out[i,] <- summary[i,] * weights[,1][m] 
    }
    summary <- out
  }
  list.out$visibility.summary <- summary
  list.out
}
