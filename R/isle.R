isle <- function(G){
  out <- list()
  if(any(degree(G, mode = "out") > 1)){
    w <- which(degree(G, mode = "out") > 1)
    names <- V(G)$name
    scs <- sources(G, sink = "r")
    splits <- names[w]
    n.splits <- length(w)
    id.a <- 1:n.splits
    id.b <- 1:n.splits
    inames <- 1:n.splits
    island <- vector("list", n.splits) 
    meg <- make_ego_graph(G, order = diameter(G), nodes = splits, mode = "out")
    for(i in 1:n.splits){
      temp <- meg[[i]]
      temp.a <- splits[i]
      ida <- ego(G, 1, temp.a, "in")
      id.a[i] <- names(ida[[1]][length(ida[[1]])])
      w <- which(degree(temp, mode = "in")>1)
      joins <- V(temp)$name[w]
      idb <- ego(G, 1, joins, "out")
      id.b[i] <- names(idb[[1]][length(idb[[1]])])
      asp <- all_simple_paths(temp, from = temp.a, to = joins, mode = "out")
      is.1 <- unique(names(unlist(asp)))
      if(any(scs == is.1)){
        island[[i]] <- is.1[-which(scs == is.1)] 
      } else {island[[i]] <- unique(names(unlist(asp)))}
      inames[i] <- paste("i",i, sep = "-")
    }  
    
    new <- G
    for(i in 1:n.splits){
      new <- delete.vertices(new, island[[i]])
      new <- add.vertices(new, 1, name = inames[i])
      new <- add.edges(new, c(id.a[i], inames[i]))
      new <- add.edges(new, c(inames[i], id.b[i]))          
    }
    
    m <- match(island, names)
    out$test <- TRUE
    out$island <- island
    out$input.id <- id.a
    out$output.id <- id.b
    out$new.graph <- new
    out$island.name <- inames
    out$splits <- splits
    } else out$test <- FALSE
  out
}




