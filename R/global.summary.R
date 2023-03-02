global.summary <- function(G, sink, inf.paths = FALSE){
  if(length(V(G)) == 0){
    out <- t(data.frame(Size = NaN, 
                        Diameter = NaN, 
                        Sources = 0,
                        n.paths.to.sink = 0,
                        mean.path.length = NaN,
                        mean.a.centrality = NaN,
                        Strahler.number = NaN,
                        Shreve.number = NaN,
                        Randic = NaN, 
                        first.Zagreb = NaN, 
                        second.Zagreb = NaN, 
                        ABC = NaN, 
                        Geom.Arith = NaN, 
                        Harmonic = NaN,
                        Harary = NaN,
                        Global.efficiency = NaN,
                        Assort.in.out = NaN, 
                        Assort.in.in = NaN))
    colnames(out) <- "Global.metrics"
  }
  else{
  order <- vcount(G)
  size <- ecount(G)
  diameter <- diameter(G)
  Npaths.to.sink <- order - 1
  # Strahler.number <- max(stream.order(G, sink = sink, method = "strahler"))
  # Shreve.number <- max(stream.order(G, sink = sink, method = "shreve"))
  
  names <- attributes(V(G))$name
  
  mean.a.centrality <- mean(alpha_centrality(G))
 # mean.page.rank <- mean(page_rank(G)$vector)
  if(any(names == sink)) { p.lengths.sink <- path.lengths.sink(G, sink = sink, inf.paths = inf.paths)} else p.lengths.sink <- NA
  
  mean.p.lengths <- mean(p.lengths.sink)
  #sd.p.lengths <- sd(p.lengths)
  randic <- I.D(G, mode = "gen.rand", alpha = -1/2)
  scd.zagreb <- I.D(G, mode = "gen.rand", alpha = 1)
  sum.con <- I.D(G, mode = "gen.sum.con", alpha = -1/2)
  fst.zagreb <- I.D(G, mode = "gen.sum.con", alpha = 1)
  ABC <- I.D(G, mode = "ABC")
  GA <- I.D(G, mode = "GA")
  harm <- I.D(G, mode = "harm")
  Harary <- harary(G)
  #Centralization <- centr_degree(G, normalized = FALSE)$centralization
  a.in.out <- assort(G, mode = "in.out")
  a.in.in <- assort(G, mode = "in.in")
  Strahler.number <- max(stream.order(G, sink = sink, method = "strahler")) 
  Shreve.number <- max(stream.order(G, sink = sink, method = "shreve"))
  
    out <- t(data.frame(Size = size, 
                      Diameter = diameter, 
                      Sources = n.sources(G, sink = sink),
                      n.paths.to.sink = Npaths.to.sink,
                      mean.path.length = mean.p.lengths,
                      mean.a.centrality = mean.a.centrality,
                      Strahler.number = Strahler.number,
                      Shreve.number = Shreve.number,
                      Randic = randic, 
                      first.Zagreb = fst.zagreb, 
                      second.Zagreb = scd.zagreb, 
                      ABC = ABC, 
                      Geom.Arith = GA, 
                      Harmonic = harm,
                      Harary = Harary,
                      Global.efficiency = global.efficiency(G),
                      Assort.in.out = a.in.out, 
                      Assort.in.in = a.in.in))
  colnames(out) <- "Global.metrics"
}
  out
}
