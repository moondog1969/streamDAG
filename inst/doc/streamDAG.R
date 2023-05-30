## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, comment = NA)

## ---- echo = F, message = F, warning = F--------------------------------------
library(igraph)
library(knitr)

## ----warning = FALSE, message = FALSE-----------------------------------------
library(devtools)
install_github("moondog1969/streamDAG")

## ---- warning = FALSE---------------------------------------------------------
library(streamDAG)

## ----mc, fig.cap = "The Reynolds Cr. experimental watershed in SW Idaho.", echo = F----
include_graphics("rc.jpg")

## -----------------------------------------------------------------------------
murphy_spring <- graph_from_literal(IN_N --+ M1984 --+ M1909, IN_S --+ M1993,
M1993 --+ M1951 --+ M1909 --+ M1799 --+ M1719 --+ M1653 --+ M1572 --+ M1452,
M1452 --+ M1377 --+ M1254 --+ M1166 --+ M1121 --+ M1036 --+ M918 --+ M823,
M823 --+ M759 --+ M716 --+ M624 --+ M523 --+ M454 --+ M380 --+ M233 --+ M153,
M153 --+ M91 --+ OUT)

## -----------------------------------------------------------------------------
streamDAGs("mur_full")

## -----------------------------------------------------------------------------
data(mur_coords) # Node spatial coords
data(mur_lengths) # Arc (stream segment) lengths
data(mur_node_pres_abs) # Node presence / absence data with explicit datetimes
data(mur_arc_pres_abs) # Arc (stream segment) simulated presence / absence data

## -----------------------------------------------------------------------------
head(A(murphy_spring))

## -----------------------------------------------------------------------------
head(mur_lengths)

## -----------------------------------------------------------------------------
head(V(murphy_spring))

## -----------------------------------------------------------------------------
names(mur_node_pres_abs)[1:7][-1] # ignoring datestamp column 1 

## ----sp1, fig.cap = "Spatially explicit graph of the completely wetted Murphy Cr. network, as it occurs in the spring."----
x <- mur_coords[,2]; y <- mur_coords[,3]
names = mur_coords[,1]
spatial.plot(murphy_spring, x, y, names)

## ----sp2, fig.cap = "Example of using a shapefile with `spatial.plot.sf`.", message = F, warning = F----
library(ggplot2); library(sf); library(ggrepel)
# Note that the directory "shape" also contains required ARC-GIS .shx,.cpg, and .prj files.
mur_sf <- st_read(system.file("shape/Murphy_Creek.shp", package="streamDAG"))

g1 <- spatial.plot.sf(x, y, names, shapefile = mur_sf)

## some ggplot customizations
g1 + expand_limits(y = c(4788562,4789700)) +
  theme(plot.margin = margin(t = 0, r = 10, b = 0, l = 0)) +
  geom_text_repel(data = mur_coords, aes(x = x, y = y, label = Object.ID), colour = "black", 
                size = 1.6, box.padding = unit(0.3, "lines"), point.padding = 
                  unit(0.25, "lines"))

## -----------------------------------------------------------------------------
mur_node_pres_abs[650:655,]

## -----------------------------------------------------------------------------
npa <- mur_node_pres_abs[650,][,-1]
G1 <- delete.nodes.pa(murphy_spring, npa)

## ----sp4, fig.cap = "Plotting a modification of `murphy_spring` after application of `delete.nodes.pa`."----
spatial.plot(G1, x, y, names)

## ----s4, fig.height = 5.5, fig.width = 6.5, fig.cap = "Dry nodes overlaid on `murphy_spring`. "----
spatial.plot(G1, x, y, names, plot.dry = TRUE, col = "orange", pt.bg = "blue")

## ---- s42, fig.height = 5.5, fig.width = 6.5, fig.cap = "Dry portions of the network underlying wet nodes and associated arcs."----
spc <- spatial.plot(murphy_spring, x, y, names, plot = FALSE)
spatial.plot(G1, x, y, names, plot.dry = TRUE, col = "orange", pt.bg = "blue", cnw = spc)

## -----------------------------------------------------------------------------
head(mur_arc_pres_abs) # 1st 6 rows of data

## -----------------------------------------------------------------------------
G2 <- delete.arcs.pa(murphy_spring, mur_arc_pres_abs[6,])

## ----sp3, fig.height = 5.5, fig.width = 6.5, fig.cap = "Plotting a modified version of `murphy_spring` after application of `delete.arcs.pa`."----
spatial.plot(G2, x, y, names)

## -----------------------------------------------------------------------------
local <- local.summary(murphy_spring)
round(local, 2)[,1:9]

## ----local1, fig.cap = "Local graph-theoretic summaries for `murphy_spring`"----
library(RColorBrewer)
cols <- brewer.pal(8,"Spectral")
cc <- complete.cases(local)
local.cc <- local[cc,]
scaled.local <- scale(t(local.cc))
barplot(t(scaled.local), col = cols, las = 2, cex.names = .6, 
        beside = T, legend.text = row.names(local)[cc], 
        args.legend = list(x = "topleft", cex = .55, bty = "n", col = cols), 
        ylab = "Standardized local measures")

## ----local2, fig.cap = "Nodal visibilities for `murphy_spring` based on nodal indegree."----
vis <- multi.path.visibility(murphy_spring, source = c("IN_N","IN_S"),
sink = "OUT", autoprint = F)

barplot(vis$visibility.summary, las = 2, cex.names = .6, ylab = "Visible nodes",
        legend.text = c("Downstream", "Upstream", "Both"),
        args.legend = list(x = "topright", title = "Direction"))

## -----------------------------------------------------------------------------
g <- print(global.summary(murphy_spring, sink = "OUT"))

## ----global1, fig.cap = "Summaries for `murphy_spring` for a subset of global metrics."----
subset <- mur_node_pres_abs[seq(1,1163, length = 100),]
subset.nodate <- subset[,-1]
# walk global.summary through node presence / absence data
global <- matrix(ncol = 23, nrow = nrow(subset))
for(i in 1:nrow(subset)){
global[i,] <- global.summary(delete.nodes.pa(murphy_spring, subset.nodate[i,]), sink = "OUT")
}
# standardize measures
scaled.global <- scale(global)
colnames(scaled.global) <- row.names(g)
scaled.global.sub <- scaled.global[,c(1:7,11,15:23)]

par(mar = c(7,4.2,1.5,2))


# plot
matplot(scaled.global.sub, xaxt = "n", type = "l", lty = 1:5, col = hcl.colors(17, palette = "spectral"), ylab = "Standardized global measures")
legend("bottomright", lty = 1:5, col = hcl.colors(17, palette = "spectral"), 
       legend = colnames(scaled.global.sub), cex = .55)
axis(side = 1, at = c(1,21,41,61,81,100), labels = subset[,1][c(1,21,41,61,81,100)], 
     las = 2, cex.axis = .7)
mtext(side = 1, "Time", line = 6)

## ----weighted1, fig.cap = "Murphy Cr. arcs colored by their probabilities of surface water presence (brown = low probability, blue = high probability)."----
prob <- apply(mur_arc_pres_abs, 2, mean)
o2 <- order(prob)
o3 <- order(o2)

col <- hcl.colors(27, palette = "Vik", rev = T)[o3]

spatial.plot(murphy_spring, x, y, names, arrow.col = col, arrow.lwd = 1.5,
             col = "white", pt.bg = "white")

## ----localw1, fig.cap = "Node strength and alpha-centrality using stream segment length and stream segment probability of activity (separately) as weights."----
G3 <- murphy_spring
E(G3)$weight <- mur_lengths[,2]
s1 <- strength(G3)
a1 <- alpha.centrality(G3)

E(G3)$weight <- prob
s2 <- strength(G3)
a2 <- alpha.centrality(G3) 

weighted.local <- cbind(s1, a1, s2, a2)
s.weighted.local <- scale(weighted.local) # standardize outcomes

barplot(t(s.weighted.local), beside = T, names = V(G3)$name, 
        col = brewer.pal(4,"Spectral"), ylab = "Standardized measures", 
        las = 2, cex.names = .8, legend.text = c("Strength_length", 
                                                 "Alpha-centrality_length",  
                                                 "Strength_prob", 
                                                 "Alpha-centrality_prob"), 
        args.legend = list(x = "topleft", cex = .7))

## ----localw1.1, fig.cap= "In-path length summaries after weighting arcs by actual in-stream lengths."----
G3 <- murphy_spring
E(G3)$weight <- mur_lengths[,2]
library(asbio)
nodes <- attributes(V(G3))$names
list.paths <- vector(mode='list', length = length(nodes)); names(list.paths) <- nodes

for(i in 1:length(nodes)){
list.paths[[i]] <- spath.lengths(G3, node = nodes[i])
}  

mean <- as.matrix(unlist(lapply(list.paths, mean)))
median <- as.matrix(unlist(lapply(list.paths, median)))
var <- as.matrix(unlist(lapply(list.paths, var)))
skew <- as.matrix(unlist(lapply(list.paths, skew)))
kurt <- as.matrix(unlist(lapply(list.paths, kurt)))

path.summary <- data.frame(Mean = mean, Median = median, Variance = var, Skew = skew, Kurtosis = kurt)

no.na <- na.omit(path.summary); scale.no.na <- scale(no.na)

barplot(t(scale.no.na), beside = TRUE, las = 2, ylim = c(-3.75, 4),   
        col = brewer.pal(5,"Spectral"),
        legend.text = c("Mean", "Median", "Variance", "Skew", "Kurtosis"),
        args.legend = list(x = "bottomright", cex = .7, bty = "n", title = "Path length"), 
        ylab = "Standardized measures")

## ----localw2, fig.cap = "Bernoulli length and communication distance using stream segment length and stream segment probability of activity (collectively) as weights"----
bsl <- bern.length(mur_lengths[,2], prob) # Bernoulli length
bcd <- bern.length(mur_lengths[,2], 1/prob) # Comm dist.

both <- cbind(bsl, bcd)
scale.both <- scale(both) # standardize outcomes

par(mar = c(7,4.5,1.5,1.5)) # allow full arc names to be seen

barplot(t(scale.both), beside = T,las = 2, cex.names = .8, 
        legend.text = c("Bernoulli length", "Commincation distance"), 
        args.legend = list(x = "topright", cex = .9), 
        ylab = "Standardized measures")


## -----------------------------------------------------------------------------
bern.length(mur_lengths[,2], prob, mode = "global") # Bernoulli length
bern.length(mur_lengths[,2], 1/prob, mode = "global") # Comm dist.

## -----------------------------------------------------------------------------
# in-stream average nodal distance
ICSL(murphy_spring, lengths = mur_lengths[,2])
# average nodal Euclidean distance
ICSL(murphy_spring, coords = mur_coords[,2:3], names = mur_coords[,1])

## ----globalw1, fig.cap = "Global weighted network connectivity measures for Murphy Cr. over time."----
icsl <- 1:nrow(subset) -> intact.to.sink -> a.cent -> harary
# walk global.summary through node presence / absence data
for(i in 1:nrow(subset)){
  temp.graph <- delete.nodes.pa(murphy_spring, subset.nodate[i,])
  # replace direction symbol for igraph comparability
  namelv <- gsub(" -> ", "|", mur_lengths[,1]) 
  a <- attributes(E(temp.graph))$vname
  w <- which(namelv %in% a)
  length.sub <- mur_lengths[,2][w]
  icsl[i] <- ICSL(temp.graph, lengths = length.sub)
  E(temp.graph)$weights <- length.sub
  intact.to.sink[i] <- size.intact.to.sink(temp.graph, "OUT")
  a.cent[i] <- mean(alpha.centrality(temp.graph), na.rm = T)
  harary[i] <- harary(temp.graph)
}

global <- cbind(icsl, intact.to.sink, a.cent, harary)

# standardize measures
scaled.global <- scale(global)
par(mar = c(7,4.2,1.5,2))

# plot
matplot(scaled.global, xaxt = "n", type = "l", col = hcl.colors(4, palette = "spectral"),
        ylab = "Standardized global measures", lty = 1:2)
legend("topright", lty = 1:2, col = hcl.colors(4, palette = "spectral"),
       legend = c("ICSL", "intact stream length to sink", "alpha-centrality", "Harary"), cex = .8)
axis(side = 1, at = c(1,21,41,61,81,100), labels = subset[,1][c(1,21,41,61,81,100)],
     las = 2, cex.axis = .7)
mtext(side = 1, "Time", line = 6)

## -----------------------------------------------------------------------------
data(mur_seasons_arc_pa)

## ----globbalw2, fig.cap = "Distributions of Bernoulli network lengths for the seasonal designations."----
springL <- matrix(nrow = 100, ncol = 27) -> summerL -> fallL

for(i in 1:100){
springL[i,] <- 
  bern.length(mur_lengths[,2], mur_seasons_arc_pa[,1:27][mur_seasons_arc_pa$Season == "Spring",][i,], "global")
summerL[i,] <- 
  bern.length(mur_lengths[,2], mur_seasons_arc_pa[,1:27][mur_seasons_arc_pa$Season == "Summer",][i,], "global")
fallL[i,] <- 
  bern.length(mur_lengths[,2], mur_seasons_arc_pa[,1:27][mur_seasons_arc_pa$Season == "Fall",][i,], "global")
}

xlim <- range(c(springL, summerL, fallL), na.rm = T)
h <- hist(springL, plot = F)
ylim <- range(h$counts)
col <- rgb(c(0,0.5,1), c(0,1,0.5), c(1,0.5,0), c(0.4,0.4,0.4))

hist(springL, xlim = xlim, ylim = ylim, main = "", xlab = "Bernoull network length (m)", col = col[1], 
     border = col[1])
par(new = TRUE)
hist(summerL, xlim = xlim, ylim = ylim, axes = F, main = "", xlab = "", col = col[2], border = col[2])
par(new = TRUE)
hist(fallL, xlim = xlim, ylim = ylim, axes = F, main = "", xlab = "", col = col[3], border = col[3])

legend("topleft", fill = col, legend = c("Spring", "Summer", "Fall"), bty = "n", cex = 1)

## -----------------------------------------------------------------------------
mean(springL) # mean spring network length
mean(summerL) # mean summer network length
mean(fallL) # mean fall network length

# mean spring network communication distance 
bern.length(mur_lengths[,2], 
            1/colMeans(mur_seasons_arc_pa[,1:27][mur_seasons_arc_pa$Season == "Spring",], 
                       na.rm = TRUE), "global")
# mean summer network communication distance 
bern.length(mur_lengths[,2], 
            1/colMeans(mur_seasons_arc_pa[,1:27][mur_seasons_arc_pa$Season == "Summer",], 
                       na.rm = TRUE), "global")
# mean fall network communication distance 
bern.length(mur_lengths[,2], 
            1/colMeans(mur_seasons_arc_pa[,1:27][mur_seasons_arc_pa$Season == "Fall",], 
                       na.rm = TRUE), "global")

## -----------------------------------------------------------------------------
data <- mur_arc_pres_abs[1:10,]
b <- beta.posterior(p.prior = 0.5, dat = data, length = mur_lengths[,2], w = 1/3)

## -----------------------------------------------------------------------------
b$alpha
b$beta

## ----globalw3, fig.cap = "Graphical summaries of posterior beta distributions for Murphy Creek stream segments from 06/01/2019 to 10/01/2019.  The posteriors represent distributions of probabilities of stream activity.  Arc distributions are colored by their mean values (darker distributions have smaller means).  The posterior means are overlain on the distributions with dashed lines."----
means <- b$alpha/(b$alpha + b$beta)
col <- gray(means/max(means))
par(mfrow = c(6,5), oma = c(4,4.5, 0.1, 1), mar = c(0,0,1.2,0.6))

for(i in 1:27){
  x <- seq(0.,1,by = .001)
  y <- dbeta(x, b$alpha[i], b$beta[i]) 
  n <- length(x)
  plot(x, yaxt = ifelse(i %in% c(1,6,11,16,21,26), "s", "n"), 
       xaxt = ifelse(i %in% 23:27, "s", "n"), type = "n", xlim = c(0,1), ylim = c(0,5.5), cex.axis = .8)
  
  polygon(c(x, x[n:1]), c(y, rep(0,n)), col = col[i], border = "grey")
  segments(means[i], 0, means[i], dbeta(means[i], b$alpha[i], b$beta[i]), lty = 2)
  mtext(side = 3, names(b$beta)[i], cex = .5)
}

#axis labels
mtext(side = 2, outer = T, expression(paste(italic(f),"(",theta[italic(k)],"|",italic(x[k]),")")), line = 2.5)
mtext(side = 1, outer = T, expression(paste(theta[italic(k)],"|",italic(x[k]))), line = 2.5)

## ----globalw4, fig.cap = "Graphical summaries of posterior inverse beta distributions for Murphy Creek stream segments from 06/01/2019 to 10/01/2019.  The posteriors represent distributions of inverse probabilities of stream activity.  Arc distributions are colored by their distributional mean values (darker distributions have smaller inverse probabilities).  The posterior means are overlain on the distributions with dashed lines."----
means <- (b$alpha + b$beta - 1)/(b$alpha - 1)
col <- gray(means/max(means))
par(mfrow = c(6,5), oma = c(4,4.5, 0.1, 1), mar = c(0,0,1.2,0.6))

for(i in 1:27){
  
  if(i %in% 1:10){lim <- c(0,1.1)} else {
    if(i %in% 11:15){lim <- c(0,2.3)} else {lim <- c(0,5)}}
  x <- seq(1,30,by = .01)
  y <- dinvbeta(x, b$alpha[i], b$beta[i]) 
  n <- length(x)
  plot(x, yaxt = ifelse(i %in% c(1,6,11,16,21,26), "s", "n"), 
       xaxt = ifelse(i %in% 23:27, "s", "n"), type = "n", xlim = c(1,15), ylim = lim, cex.axis = .8, log = "x")
  
  polygon(c(x, x[n:1]), c(y, rep(0,n)), col = col[i], border = "grey")
  segments(means[i], 0, means[i], dinvbeta(means[i], b$alpha[i], b$beta[i]), lty = 2)
  mtext(side = 3, names(b$beta)[i], cex = .5)
}

#axis labels
  mtext(side = 2, outer = T, expression(paste(italic(f),"(",theta[italic(k)],"|",italic(x[k]),")",""^{-1})), line = 2.5)
  mtext(side = 1, outer = T, expression(paste("(",theta[italic(k)],"|",italic(x[k]),")",""^{-1})), line = 2.5)

## ----kc, fig.cap = "The Kings Cr. watershed in central Kansas.", echo = F-----
include_graphics("kc.jpg")

## -----------------------------------------------------------------------------
kon_full <- streamDAGs("konza_full")

## ----k1, fig.cap = "Spatially explicit graph of the completely wetted Konza Prairie network."----
data(kon_coords)
spatial.plot(kon_full, kon_coords[,3], kon_coords[,2], names = kon_coords[,1])

## -----------------------------------------------------------------------------
A.mult(kon_full, power = 6, text.summary = TRUE)

## ----Kstr, fig.cap = "Strahler numbers for the complete Konza network."-------
sok <- stream.order(G = streamDAGs("konza_full"), sink = "SFM01_1", method = "strahler")

colk <- as.character(factor(sok, labels = topo.colors(3, rev = TRUE)))
spatial.plot(G = streamDAGs("konza_full"), x = kon_coords[,3], y = kon_coords[,2], 
             names = kon_coords[,1], pt.bg = colk, cex = 1.5, cex.text = 0, plot.bg = "white")
legend("bottomright", title = "Strahler\nStream order", legend = unique(sok), 
pch = 21, pt.cex = 1.5, pt.bg = unique(colk), ncol = 1, bty = "n", title.cex = 0.9)

## ----Kshr, fig.cap = "Shreve numbers for the complete Konza network."---------
sok <- stream.order(G = streamDAGs("konza_full"), sink = "SFM01_1", method = "shreve")
colk <- as.character(factor(sok, labels = topo.colors(6, rev = TRUE)))
spatial.plot(G = streamDAGs("konza_full"), x = kon_coords[,3], y = kon_coords[,2],  
             names = kon_coords[,1], pt.bg = colk, cex = 1.5, cex.text = 0, plot.bg = "white")
legend("bottomright", title = "Shreve\nStream order", legend = unique(sok), 
pch = 21, pt.cex = 1.5, pt.bg = unique(colk), ncol = 2, bty = "n", title.cex = 0.9)

## ----k2, fig.cap = "Local graph-theoretic summaries for Konza Prairie."-------
local <- local.summary(kon_full)
cols <- brewer.pal(8,"Spectral")
cc <- complete.cases(local)
local.cc <- local[cc,]
scaled.local <- scale(t(local.cc))

barplot(t(scaled.local), col = cols, las = 2, cex.names = .6,
        beside = T, legend.text = row.names(local)[cc],
        args.legend = list(x = "topright", cex = .7,
                           bty = "n", col = cols),
        ylab = "Standardized local measures")

## ----k3, fig.cap = "Nodal visibilities for for Konza Prairie based on nodal indegree."----
vis <- multi.path.visibility(kon_full, source = 
                               c("SFT01_1", "02M11_1","04W04_2", 
                                 "04T01_1", "04M13_1", 
                                 "SFT02_1","01M06_1", "20M05_1"),
                             sink = "SFM01_1", autoprint = F)
barplot(vis$visibility.summary, las = 2, cex.names = .6, ylab = "Visible nodes",
        legend.text = c("Downstream", "Upstream", "Both"),
        args.legend = list(x = "topleft", title = "Direction"))

## ----k4, fig.cap = "Spatial plot representations for Konza Prairie for (A) 05/21/2021 (B) 05/28/2021, and (C) 06/04/2021."----
K0521 <- streamDAGs("KD0521"); K0528 <- streamDAGs("KD0528"); K0604 <- streamDAGs("KD0604")
kx <- kon_coords[,3]; ky <- kon_coords[,2]; kn <- kon_coords[,1] 
par(mfrow = c(3,1), mar = c(0,0,1,0), oma = c(5,4,1,1))
spatial.plot(K0521, kx, ky, kn, xaxt = "n", cex.text = 0); legend("topright", bty = "n", legend = "A", cex = 2)
spatial.plot(K0528, kx, ky, kn, xaxt = "n", cex.text = 0); legend("topright", bty = "n", legend = "B", cex = 2)
spatial.plot(K0604, kx, ky, kn, cex.text = 0); legend("topright", bty = "n", legend = "C", cex = 2)
mtext(side = 1, "Longitude", outer = T, line = 3.5); mtext(side = 2, "Latitude", outer = T, line = 3)

## ----k5, fig.cap = "Global summaries for Konza Prairie for 05/21/2021, 05/28/2021, and 06/04/2021."----
g0521 <- global.summary(K0521, sink = "SFM01_1")
g0528 <- global.summary(K0528, sink = "SFM01_1")
g0604 <- global.summary(K0604, sink = "SFM01_1")
global <- cbind(g0521, g0528, g0604)
scaled.global <- scale(t(global))
                       
matplot(scaled.global, xaxt = "n", type = "l", col = hcl.colors(15, palette = "spectral"),
        ylab = "Standardized global measures")
legend("topleft", lty = 1:5, col = hcl.colors(15, palette = "spectral"),
       legend = row.names(global), cex = .5, bg = "white")
axis(side = 1, at = c(1,2,3), labels = c("5/21/2021","5/28/2021","6/04/2021"),las = 2)

## -----------------------------------------------------------------------------
mur_node_pres_abs[404:405,]

## -----------------------------------------------------------------------------
arc.pa.from.nodes(murphy_spring, mur_node_pres_abs[404:405,][,-1])

## -----------------------------------------------------------------------------
conversion <- arc.pa.from.nodes(murphy_spring, mur_node_pres_abs[,-1])
marginal <- colMeans(conversion, na.rm = TRUE)
corr <- cor(conversion, use = "pairwise.complete.obs")

## -----------------------------------------------------------------------------
corrected.corr <- R.bounds(marginal, corr)

## ---- eval = F----------------------------------------------------------------
#  library(mipfp)
#  p.joint.all <- ObtainMultBinaryDist(corr = corrected.corr, marg.probs = marginal,
#                                      tol = 0.001, tol.margins = .001, iter = 100)
#  out <- RMultBinary(n = 5, p.joint.all, target.values = NULL)$binary.sequences

