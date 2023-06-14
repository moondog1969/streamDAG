sink.G <- function(G, sink = NULL){
  groups <- components(G, "weak")
  members <- groups$membership
  w <- which(names(members) == sink)
  node.group <- which(members == members[w])
  node.members <- attributes(V(G))$names[node.group]
  subgraph(G, node.members)
}

#---------------------------------------------------------------------#


stream.order <- function(G, sink = NULL, method = "strahler"){
  oldG <- G
  if(isle(G)$test){
    isl <- isle(G)
    G <- isl$new.graph
  }  
  
  order.rule <- function(G, tracker, j.input, n.join, method = "strahler"){
    temp <- ego(G, j.input, order = 1, mode = "in")
    n.neighbor <- unlist(lapply(temp, function(x) x[-1]))
    w <- which(colnames(tracker) %in% names(n.neighbor))
    
    ord <- tracker[w]
    sord <- all(outer(ord, ord, "-") == 0)
    if(method == "strahler") {
      if(sord){out <- ord[1] + 1} else out <- max(ord)
    }
    if(method == "shreve") {
      out <- sum(ord)
    }
    lout <- list()
    if(is.na(n.join)){sp <- data.frame(j.input = out); names(sp) <- j.input}  else{
      sp1 <- shortest_paths(G, from = j.input, to = n.join)$vpath
      sp2 <- unlist(sp1)
      sp <- sp2[-length(sp2)]}
    lout$sp <- sp
    lout$out <- out
    lout
  }
  
  if(all((attributes(V(G))$names)!=sink)){
  nodes <- attributes(V(G))$names
  tracker <- matrix(NA, ncol = length(nodes))
  colnames(tracker) <- nodes
  } else{
  
  groups <- components(G, "weak")
  if(groups$no > 1){
    G <- sink.G(G, sink = sink)  
  }
  
  nodes <- attributes(V(G))$names
  tracker <- matrix(NA, ncol = length(nodes))
  colnames(tracker) <- nodes
  
  di <- degree(G, mode = "in")
  joins <- names(which(di >= 2))
  
  if(length(joins) == 0){tracker <- t(apply(tracker,2,function(x)x = 1))}
  else{
    source <- sources(G, sink = sink)
    list.out <- vector(mode='list', length = length(source))
    
    # loop 1: create list containing paths from sources to joins   
    for(i in 1:length(source)){
      for(j in 1:length(joins)){
        sp <- suppressWarnings(shortest_paths(G, from = source[i], to = joins[j]))
        if(length(sp$vpath[[1]]) >= 1){
          list.out[[i]][[j]] <- names(unlist(sp$vpath))
        }  
      }
    }
    
    # loop 2: organize list.out 
    for(i in 1:length(source)){
      if(length(list.out[[i]]) == 0){
        list.out[[i]] <- names(unlist(shortest_paths(G, from = source[i], to = sink)$vpath))
      }
      u <- unlist(lapply(list.out[[i]], is.null))
      w <- which(u)
      if(length(w) > 0) list.out[[i]] <- list.out[[i]][-w]
    }
    
    merge <- vector(mode='list', length = length(source)) 
    
    # loop 3: further organize list.out 
    for(i in 1:length(merge)){
      temp <- list.out[[i]]
      names(temp) <- lapply(temp, function(x)x[length(x)])
      if(length(list.out[[i]]) > 1){
        o <- order(unlist(lapply(temp, length)), decreasing = F)  
        tempo <- temp[o]
        merge[[i]] <- tempo
      }
      else merge[[i]] <- list.out[[i]]
    }
    
    names(merge) <- source      
    
    # define order of source catchments to be 1     
    step1 <- lapply(merge, function(x)x[[1]])
    step2 <- lapply(step1, function(x)x[-length(x)])
    step3 <- as.vector(unlist(step2)) 
    w <- which(colnames(tracker) %in% step3)
    tracker[w] <- 1
    
    if(length(joins) == 1){
      nas <- which(is.na(tracker))
      tracker[nas] <- ifelse(method == "strahler", 2, degree(G, joins, "in"))
    }
    
    else{
      # distances of joins to other joins
      dw <- distances(G, v = joins, to = sink)
      o <- order(dw, decreasing = T)
      joinso <- names(dw[o,])
      d <- distances(G, joinso, joinso, "out")
      a <- apply(d, 1, function(x) min(x[x!=0]))
      
      out <- 1:length(a)
      
      # loop 4: define nearest join to other joins
      for(i in 1:nrow(d)){
        if(a[i] == Inf){out[i] <- NA}
        else{out[i] <- colnames(d)[which(d[i,]==a[i])]
        }
      }  
      
      nearest.join <- cbind(joinso,out)
      colnames(nearest.join) <- c("join", "next.join")
      
      # loop 5: run stream order decision algorithm
      for(i in 1:nrow(nearest.join)){
        orr <- order.rule(G = G, tracker = tracker, j.input = nearest.join[i,][1], 
                          n.join = nearest.join[i,][2], method = method) 
        w <- which(colnames(tracker) %in% names(orr$sp))
        tracker[w] <- orr$out
      }
      w.na <- which(is.na(tracker))
      tracker[w.na] <- max(tracker, na.rm = TRUE)
    }}}
  #row.names(tracker) <- "stream.order"
  out <- t(tracker)
  
  if(isle(oldG)$test){
    nodes <- attributes(V(oldG))$names
    for(i in 1:length(isl$splits)){
    m <- match(isl$island.name[[i]], row.names(out))
    w <- which(row.names(out) == isl$input.id[i])
    out1 <- as.matrix(out[-m,])
    inodes <- nodes[which(!(nodes %in% row.names(out1)))]
    out2 <- matrix(nrow = length(inodes), ncol = 1, data = out[w,])
    row.names(out2) <- inodes
    out <- rbind(out1, out2)}
  }
out  
}