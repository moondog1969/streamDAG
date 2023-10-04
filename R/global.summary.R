global.summary <- function(G, which = "all", sink, mode = "in", inf.paths = FALSE){
  if(length(V(G)$name) == 0) crit <- TRUE else crit <- FALSE
  if(crit) names <- NA else names <- attributes(V(G))$name
  
      varP <- function(x){
        x <- na.omit(x)
        sum((x - mean(x))^2)/length(x)
      } 
  
     if(which == "all"| which == "size"){ 
      if(crit){out <- size <- 0} else{ 
        out <- size <- ecount(G)}}
  
    if(which == "all"| which == "diameter"){ 
      if(crit){out <- diameter <- 0} else{ 
        out <- diameter <- diameter(G)}}
  
    if(which == "all"| which == "graph.order"){ 
      if(crit){out <- order <- 0} else{ 
        out <- order <- vcount(G)}}  
    
    if(which == "all"| which == "n.sources"){
     # if(all(names != sink)) stop("The sink node you defined is not a node in G")
      if(crit | all(names != sink)){Sources <- out <- NA} else{
        out <- Sources <- n.sources(G, sink = sink)}}

    if(which == "all"| which == "n.paths.sink" | which == "sink.path.len.summary"){
     # if(all(names != sink)) stop("The sink node you defined is not a node in G")
      if(crit | all(names != sink)){out <- n.paths.to.sink <- NA} else{ 
        out <- n.paths.to.sink <- n.tot.paths(G, mode = "in", sink = sink)}}

  
#-----------------------------------------------------------# 
    
 #   if(which == "all"| which == "sink.path.len.summary"){
    if(crit | all(names != sink)){p.lengths.sink <- NA} else{p.lengths.sink <- path.lengths.sink(G, sink = sink, inf.paths = inf.paths)}
 
      if(which == "all"| which == "sink.path.len.summary"){ 
      if(crit | all(names != sink)){path.length.mean <- NaN} else{ 
        path.length.mean <- mean(p.lengths.sink)}}
    
    if(which == "all"| which == "sink.path.len.summary"){ 
      if(crit | all(names != sink)){path.length.var <- NaN} else{ 
        path.length.var <- varP(p.lengths.sink)}}
    
    if(which == "all"| which == "sink.path.len.summary"){ 
      if(crit | all(names != sink)){path.length.skew <- NaN} else{ 
        path.length.skew <- skew(p.lengths.sink)}}
    
    if(which == "all"| which == "sink.path.len.summary"){ 
      if(crit | all(names != sink)){path.length.kurt <- NaN} else{ 
        path.length.kurt <- kurt(p.lengths.sink)}}
  #  } 


#-----------------------------------------------------------#    
    
    if(crit) deg <- NA else deg <- degree(G, mode = mode)  
    if(which == "all"| which == "deg.summary"){ 
      if(crit){deg.mean <- NaN} else{ 
        deg.mean <- mean(deg)}}
    
    if(which == "all"| which == "deg.summary"){ 
      if(crit){deg.var <- NaN} else{ 
        deg.var <- varP(deg)}}
    
    if(which == "all"| which == "deg.summary"){ 
      if(crit){deg.skew <- NaN} else{ 
        deg.skew <- skew(deg)}}
    
    if(which == "all"| which == "deg.summary"){ 
      if(crit){deg.kurt <- NaN} else{ 
        deg.kurt <- kurt(deg)}}
    
#-----------------------------------------------------------#    
    
    if(which == "all"| which == "avg.alpha.cent"){ 
      if(crit){out <- mean.a.centrality <- NaN} else{ 
        out <- mean.a.centrality <- mean(alpha_centrality(G))}}
    
    if(which == "all"| which == "shreve.num"){ 
      if(crit){out <- shreve.num <- 0} else{ 
        out <- shreve.num <- max(stream.order(G, sink = sink, method = "shreve"))}}
    
    if(which == "all"| which == "strahler.num"){ 
      if(crit){out <- strahler.num <- 0} else{ 
        out <- strahler.num <- max(stream.order(G, sink = sink, method = "strahler"))}}  
    
    if(which == "all"| which == "fst.zagreb"){ 
      if(crit){out <- fst.zagreb <- NaN} else{ 
        out <- fst.zagreb <- I.D(G, mode = "gen.sum.con", alpha = 1)}}
    
    if(which == "all"| which == "scd.zagreb"){ 
      if(crit){out <- scd.zagreb <- NaN} else{ 
        out <- scd.zagreb <- I.D(G, mode = "gen.rand", alpha = 1)}}
    
    if(which == "all"| which == "ABC"){ 
      if(crit){out <- ABC <- NaN} else{ 
        out <- ABC <- I.D(G, mode = "ABC")}}
    
    if(which == "all"| which == "harary"){ 
      if(crit){out <- Harary <- NaN} else{ 
        out <- Harary <- harary(G)}} 
    
    if(which == "all"| which == "global.efficiency"){ 
      if(crit){out <- Global.efficiency <- NaN} else{ 
        out <- Global.efficiency <- global.efficiency(G)}}
    
    if(which == "all"| which == "assort.in.out"){ 
      if(crit){out <- a.in.out <- NaN} else{ 
        out <- a.in.out <- assort(G, mode = "in.out")}}
    
    if(which == "all"| which == "assort.in.in"){ 
      if(crit){out <- a.in.in <- NaN} else{ 
        out <- a.in.in <- assort(G, mode = "in.in")}}
   
  
if(which == "all"){
        out <- t(data.frame(Size = size, 
                      Diameter = diameter,
                      Graph.order = order,
                      n.Sources = Sources,
                      Mean.a.centrality = mean.a.centrality,
                      n.Paths.to.sink = n.paths.to.sink,
                      Path.length.mean = path.length.mean,
                      Path.length.var = path.length.var,
                      Path.length.skew = path.length.skew,
                      Path.length.kurt = path.length.kurt,
                      Degree.mean = deg.mean,
                      Degree.var = deg.var,
                      Degree.skew = deg.skew,
                      Degree.kurt = deg.kurt,
                      Shreve.number = shreve.num,
                      Strahler.number = strahler.num,
                      First.Zagreb = fst.zagreb, 
                      Second.Zagreb = scd.zagreb, 
                      ABC = ABC, 
                      Harary = Harary,
                      Global.efficiency = global.efficiency(G),
                      Assort.in.out = a.in.out, 
                      Assort.in.in = a.in.in))
  colnames(out) <- "Global.metrics"
}

    if(which == "sink.path.len.summary"){
      out <- t(data.frame(n.Paths.to.sink = n.paths.to.sink,
                          Path.length.mean = path.length.mean,
                          Path.length.var = path.length.var,
                          Path.length.skew = path.length.skew,
                          Path.length.kurt = path.length.kurt))
      colnames(out) <- "Global.metrics"
    } 
    
    if(which == "deg.summary"){
      out <- t(data.frame(Degree.mean = deg.mean,
                          Degree.var = deg.var,
                          Degree.skew = deg.skew,
                          Degree.kurt = deg.kurt))
      colnames(out) <- "Global.metrics"
    }
    if(which != "sink.path.len.summary" & which != "deg.summary" & which != "all"){
      out <- as.matrix(out)
      colnames(out) <- which
    }
  out
}
