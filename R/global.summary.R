global.summary <- function(G, sink, inf.paths = FALSE){
  order <- vcount(G)
  size <- ecount(G)
  diameter <- diameter(G)
  Npaths.to.sink <- order - 1
  mean.a.centrality <- mean(alpha_centrality(G))
  p.lengths.sink <- path.lengths.sink(G, sink = sink, inf.paths = inf.paths)
  mean.p.lengths <- mean(p.lengths.sink)
  #sd.p.lengths <- sd(p.lengths)
  randic <- I.D(G, mode = "gen.rand", alpha = -1/2)
  scd.zagreb <- I.D(G, mode = "gen.rand", alpha = 1)
  sum.con <- I.D(G, mode = "sum.con", alpha = -1/2)
  fst.zagreb <- I.D(G, mode = "sum.con", alpha = 1)
  ABC <- I.D(G, mode = "ABC")
  GA <- I.D(G, mode = "GA")
  harm <- I.D(G, mode = "harm")
  a.in.out <- assort(G, mode = "in.out")
  a.in.in <- assort(G, mode = "in.in")
  
  out <- t(data.frame(Size = size, Diameter = diameter, Npaths.to.sink = Npaths.to.sink, Mean.a.centrality = mean.a.centrality, mean.path.lengths = mean.p.lengths, Randic = randic, fst.Zagreb = fst.zagreb, scd.Zagreb = scd.zagreb, ABC = ABC, GA = GA, Harm = harm, Assort.in.out = a.in.out, Assort.in.in = a.in.in))
  colnames(out) <- "Global.metrics"
  out
}
