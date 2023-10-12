vector_segments <- function(sf.coords, node.coords, realign = TRUE, arcs, arc.symbol = " --> ", nneighbors = 40, remove.duplicates = FALSE){
  colnames(sf.coords) <- c("x","y"); colnames(node.coords) <- c("x","y") 
  row.names(sf.coords) <- 1:nrow(sf.coords)
  
  nn <- function(D, label, k = 1){
    D1 <- as.matrix(D)[labels(D) == label,]
    sort(D1[names(D1) != label])[1:k]
  }
  
  reassign <- function(sf.coords, node.coords){
    nc <- node.coords
    nodes <- row.names(node.coords) 
    for(i in 1:length(nodes)){
      node <- nodes[i]
      sfc <- sf.coords
      nr <- node.coords[row.names(node.coords) == node,]
      Dr <- dist(rbind(sfc, nr))
      sub <- as.matrix(Dr)[labels(Dr) == node,]
      ssub <- sort(sub)
      lb <- ssub[1:2]
      lb1 <- lb[names(lb) != node][1]
      nc[rownames(node.coords) == node,] <- sfc[rownames(sfc) == names(lb1),]
    }
   nc 
  }

if(realign){node.coords <- reassign(sf.coords, node.coords)}

endpoints <- data.frame(matrix(nrow = length(arcs), 
                    unlist(strsplit(arcs, arc.symbol)), byrow = T))
names(endpoints) <- c("from", "to")
row.names(endpoints) <- arcs

#---------------------------------------------------------------------------------------#

stepmat <- list()
for(i in 1:nrow(endpoints)){
    temp.s <- as.character(endpoints[i,][1])
    temp.e <- as.character(endpoints[i,][2])
    all.coords <- rbind(sf.coords, 
                        node.coords[rownames(node.coords) == temp.s,],
                        node.coords[rownames(node.coords) == temp.e,])
    
    D <- dist(all.coords)  
    dmax <- as.matrix(D)[labels(D) == temp.s,][temp.e]
    enn <- nn(D, label = temp.e, k = 2)
    
      if(as.matrix(D)[labels(D) == names(enn)[1],][temp.s] <= dmax) end <- enn[1]
      else if(as.matrix(D)[labels(D) == names(enn)[2],][temp.s] <= dmax) end <- enn[2]
    
    end <- names(end)
    
    snn <- nn(D, label = temp.s, k = 2)
        
      if(as.matrix(D)[labels(D) == names(snn)[1],][temp.e] <= dmax) step <- snn[1]
      else if(as.matrix(D)[labels(D) == names(snn)[2],][temp.e] <= dmax) step <- snn[2]
    
        stepmat[[i]] <- data.frame(point = names(step), arc.label = as.character(row.names(endpoints)[i])) 
  
    crit <- names(step) -> pstep
    d.current <- as.matrix(D)[labels(D) == names(step),][end]
    j <- 1
  #-------------------------------------------------------------------------------------#
  
      while(crit != end){
      snn1 <- nn(D, label = names(step), k = min(nneighbors, nrow(sf.coords)))  
      amd <- as.matrix(D)[labels(D) == end,]
      onn <- amd[amd < d.current]
      onn <- onn[(names(onn) != endpoints$from[i]) &
                   (names(onn) != endpoints$to[i]) &
                  !(names(onn) %in% stepmat[[i]]$point)]
      
      snn <- snn1[(names(snn1) %in%  names(onn))]
      # snn <- snn[snn <= de]
        if(as.matrix(D)[labels(D) == names(snn)[1],][temp.e] <= dmax) step <- snn[1]
        else if((length(snn) > 1) & as.matrix(D)[labels(D) == names(snn)[2],][temp.e] <= dmax) step <- snn[2]
      
      temp <- data.frame(point = names(step), arc.label = as.character(row.names(endpoints)[i])) 
      stepmat[[i]] <- rbind(stepmat[[i]], temp)
      crit <- names(step)
      d.current <- as.matrix(D)[labels(D) == names(step),][end]
    }
  }
  
out <- list()
df <- do.call("rbind",stepmat)
if(remove.duplicates){u <- !duplicated(df$point); df <- df[u,]}
df$x <- sf.coords[,1][match(df$point, row.names(sf.coords))]
df$y <- sf.coords[,2][match(df$point, row.names(sf.coords))]
out$df <- df
out$node.coords <- node.coords
class(out) <- "network_to_sf"
invisible(out)
}


print.network_to_sf <- function(x, ...){
  cat("Arcs assigned to spatial coordinates:", "\n\n")
  print(x$df, ...)
}

#############################################################################


assign_pa_to_segments <- function(input, n, arc.pa, datetime = NULL){
  if(!inherits(input, "network_to_sf")) warning("Function works optimally with objects of class network_to_sf")
  if(inherits(input, "network_to_sf")) input <- input$df
  out <- data.frame(do.call("rbind", replicate(n, input, simplify = FALSE)))
  if(is.null(datetime)){out$Time <- rep(1:n, each = nrow(input))} 
    else out$Time <- rep(datetime, each = nrow(input))
  u.time <- unique(out$Time)
  lu.time <- length(unique(out$Time))
  arc.pa <- as.matrix(arc.pa)
  pa <- list()
  for(i in 1:lu.time){
    temp <- out[out$Time == u.time[i],]
    temp1 <- arc.pa[i,]
    m <- match(temp$arc.label, colnames(arc.pa))
    pa[[i]] <- data.frame(t(t(temp1[m])))
    names(pa[[i]]) <- "pres.abs"
    row.names(pa[[i]]) <- 1:nrow(input)
  }
  df <- do.call("rbind",pa)
  out$Presence <- df$pres.abs
  out
}

