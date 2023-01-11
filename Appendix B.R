library(streamDAG)
murphy_spring <- graph_from_literal(IN_N --+ M1984 --+ M1909, IN_S --+ M1993,
                                    M1993 --+ M1951 --+ M1909 --+ M1799 --+ M1719 --+ M1653 --+ M1572 --+ M1452,
                                    M1452 --+ M1377 --+ M1254 --+ M1166 --+ M1121 --+ M1036 --+ M918 --+ M823,
                                    M823 --+ M759 --+ M716 --+ M624 --+ M523 --+ M454 --+ M380 --+ M233 --+ M153,
                                    M153 --+ M91 --+ OUT)


data(mur_coords)
data(mur_lengths)
data(mur_node_pres_abs)
data(mur_arc_pres_abs)
data(kon_coords)

#---------------------------------------------------------------------------#
# Fig 2
library(igraph)

set.seed(11)
m0 <- graph_from_literal(2--+ 1)
m1 <- graph_from_literal(3 --+ 2, 2--+ 1)
m2 <- graph_from_literal(4 --+ 3, 5 --+ 3, 3--+ 2, 2--+ 1)
m3 <- graph_from_literal(6 --+ 4, 5 --+ 4, 4--+ 3, 3--+ 2, 2--+ 1)
m4 <- graph_from_literal(6 --+ 3, 4 --+ 3, 5 --+ 3, 3--+ 2, 2--+ 1)
#m5 <- graph_from_literal(7 --+ 6, 6 --+ 3, 4 --+ 3, 5 --+ 3, 3--+ 2, 2--+ 1)
m5 <- graph_from_literal(7 --+ 3, 6 --+ 3, 4 --+ 3, 5 --+ 3, 3--+ 2)
m6 <- graph_from_literal(8 --+ 2, 7 --+ 6, 6 --+ 3, 4 --+ 3, 5 --+ 3, 3--+ 2, 2--+ 1)
m7 <- graph_from_literal(8 --+ 6, 7 --+ 6, 6 --+ 3, 4 --+ 3, 5 --+ 3, 3--+ 2, 9-+2, 2--+ 1)
m8 <- graph_from_literal(8 --+ 6, 7 --+ 6, 6 --+ 3, 4 --+ 3, 5 --+ 3, 3--+ 2, 9-+2, 10 --+ 1, 2--+ 1)



library(RColorBrewer)
pdf("DAGs.pdf")
cols <- brewer.pal(9, "Set3")
par(mfrow = c(3,3), mar = c(0,0,0,0))
set.seed(11)
plot(m0)#; text(.95,-1,"m0", cex = 2, col = "blue")
coord <- par("usr")
rect(coord[1],coord[3],coord[2],coord[4], col = cols[1])
plot(m0, add = TRUE, vertex.size = 20, edge.color = "blue",vertex.label=NA)
text(.95,-1,"1", cex = 1.8, col = "#53868B", font = 2)

set.seed(11)
plot(m1)
coord <- par("usr")
rect(coord[1],coord[3],coord[2],coord[4], col = cols[2])
plot(m1, add = TRUE, vertex.size = 20, edge.color = "blue",vertex.label=NA)
text(.95,-1,"2", cex = 1.8, col = "#8B864E", font = 2)

set.seed(11)
plot(m2)
coord <- par("usr")
rect(coord[1],coord[3],coord[2],coord[4], col = cols[3])
plot(m2, add = TRUE, vertex.size = 20, edge.color = "blue",vertex.label=NA)
text(.95,-1,"3", cex = 1.8, col = "#5D478B", font = 2)

set.seed(4)
plot(m3)
coord <- par("usr")
rect(coord[1],coord[3],coord[2],coord[4], col = cols[4])
plot(m3, add = TRUE, vertex.size = 20, edge.color = "blue",vertex.label=NA)
text(.95,-1,"4", cex = 1.8, col = "#8B3A3A", font = 2)

set.seed(13)
plot(m4)
coord <- par("usr")
rect(coord[1],coord[3],coord[2],coord[4], col = cols[5])
plot(m4, add = TRUE, vertex.size = 20, edge.color = "blue",vertex.label=NA)
text(.95,-1,"5", cex = 1.8, col = "#36648B", font = 2)

set.seed(12)
plot(m5)
coord <- par("usr")
rect(coord[1],coord[3],coord[2],coord[4], col = cols[6])
plot(m5, add = TRUE, vertex.size = 20, edge.color = "blue",vertex.label=NA)
text(.95,-1,"6", cex = 1.8, col = "#8B5A2B", font = 2)

set.seed(10)
plot(m6)
coord <- par("usr")
rect(coord[1],coord[3],coord[2],coord[4], col = cols[7])
plot(m6, add = TRUE, vertex.size = 20, edge.color = "blue",vertex.label=NA)
text(.95,-1,"7", cex = 1.8, col = "#6E8B3D", font = 2)

set.seed(9)
plot(m7)
coord <- par("usr")
rect(coord[1],coord[3],coord[2],coord[4], col = cols[8])
plot(m7, add = TRUE, vertex.size = 20, edge.color = "blue",vertex.label=NA)
text(.95,-1,"8", cex = 1.8, col = "#8B636C", font = 2)

set.seed(9)
plot(m8)
coord <- par("usr")
rect(coord[1],coord[3],coord[2],coord[4], col = cols[9])
plot(m8, add = TRUE, vertex.size = 20, edge.color = "blue",vertex.label=NA)
text(.95,-1,"9", cex = 1.8, col = "#737373", font = 2)
dev.off()

summary.func <- function(m = "gen.rand", a = -1/2){
  R0 <- I.D(m0, mode = m, alpha = a); R1 <- I.D(m1, mode = m, alpha = a)
  R2 <- I.D(m2, mode = m, alpha = a); R3 <- I.D(m3, mode = m, alpha = a)
  R4 <- I.D(m4, mode = m, alpha = a); R5 <- I.D(m5, mode = m, alpha = a)
  R6 <- I.D(m6, mode = m, alpha = a); R7 <- I.D(m7, mode = m, alpha = a)
  R8 <- I.D(m8, mode = m, alpha = a)
  c(R0, R1, R2, R3, R4, R5, R6, R7, R8)
}



R <- summary.func()
# Directed 2nd Zagreb
Z2 <- summary.func(a = 1)
# Directed 2nd modified Zagreb
MZ2 <- summary.func(a = -1)
# Sum connectivity
SC <- summary.func(m = "gen.sum.con",  a = -1/2)
# Directed 1st Zagreb
Z1 <- summary.func(m = "gen.sum.con",  a = 1)
# Atom bond connectivity
ABC <- summary.func(m = "ABC")
# Geometric arithmetic
GA <-  summary.func(m = "GA")
# Harmonic
H <-  summary.func(m = "harm")

RA <- summary.func(m = "rand.aug")

library(asbio)
I.Ds <- cbind(R, Z2, MZ2, SC, Z1, H, ABC, GA)


panel.up <- function(x, y, col = cols){
  usr <- par("usr")
  # h <- hist(x, plot = FALSE)
  # breaks <- h$breaks; nB <- length(breaks)
  # y <- h$counts; y <- y/max(y)
  rect(usr[1], usr[3], usr[2], usr[4], col = gray(.5))
  points(x, y, col = cols[1:9], pch = 19, cex = 1.3)
}

library(mgcv)

panel.gam <- function(x, y){
  usr <- par("usr")
  model <- lm(y ~ x)
  y.hat <- fitted(model)
  r2 <- summary(model)$r.sq
  rect(usr[1], usr[3], usr[2], usr[4], col = gray((1-r2)+.3))
  points(x, y.hat, type = "l", col = "white")
}

pdf("pairs.pdf")
pairs(I.Ds, labels = c("Randic\n", "2nd Zagreb\n", "Mod.\n 2nd Zagreb\n\n", "Sum\n connect.\n\n", "1st Zagreb\n", "Harmonic\n\n\n","ABC\n\n\n",  "Geometric\n Arithmetic\n\n"), gap=0, upper.panel = panel.up, lower.panel=panel.gam)
mtext(side = 1, at = 0.123, expression((xy)^{-1/2}), cex = .8, line = -25.1)
mtext(side = 1, at = 0.228, expression((xy)^{1}), cex = .8, line = -21.25)
mtext(side = 1, at = 0.34, expression((xy)^{-1}), cex = .8, line = -17.5)
mtext(side = 1, at = 0.445, expression((x+y)^{-1}), cex = .8, line = -13.8)
mtext(side = 1, at = 0.555, expression((x+y)^{1}), cex = .8, line = -10.1)
mtext(side = 1, at = 0.66, expression(over(2, x+y)), cex = .75, line = -6.2)
mtext(side = 1, at = 0.765, expression(over(sqrt(xy), 1/2(xy))), cex = .75, line = -2.5)
mtext(side = 1, at = 0.87, expression(sqrt(over(x+y, xy))), cex = .65, line = 1.45)
dev.off()


#---------------------------------------------------------------------#
# Fig 3
pdf("Figp3.pdf",width = 6.5, height = 9)
par(mfrow = c(2,1), mar = c(0,0,3,0), oma = c(5,4.5,0,2))
spatial.plot(murphy_spring, mur_coords$long, mur_coords$lat, mur_coords$Object.ID)
spatial.plot(streamDAGs("konza_full"), kon_coords$long, kon_coords$lat, kon_coords$Object.ID)
mtext(side = 1, "Longitude", line = 3, cex = 1.4, outer = T)
mtext(side = 2, "Latitude", line = 3, cex = 1.4, outer = T)
mtext(side = 3, "A", line = -4.5, cex = 1.4, outer = T, at = 0.04)
mtext(side = 3, "B", line = -24.5, cex = 1.4, outer = T, at = 0.04)
dev.off()
#---------------------------------------------------------------------#
# Fig 4
pdf("Figp4.pdf",width = 6.5, height = 5.5)
par(cex=1.1)
npa <- mur_node_pres_abs[650,][,-1]
G1 <- delete.nodes.pa(murphy_spring, npa)
spatial.plot(G1, mur_coords$long, mur_coords$lat, mur_coords$Object.ID, xlab = "Longitude", ylab = "Latitude", plot.dry = TRUE, col = "orange", pt.bg = "deepskyblue", cex = 1.2, arrow.lwd = 1.2, arrow.col = "deepskyblue4")
dev.off()

#---------------------------------------------------------------------#
# Fig not in manuscript
pdf("Figadd1.pdf",width = 6.5, height = 5.5)
G2 <- delete.arcs.pa(murphy_spring, mur_arc_pres_abs[6,]) 
spatial.plot(G2, mur_coords$long, mur_coords$lat, mur_coords$Object.ID, xlab = "Longitude", ylab = "Latitude")
dev.off()
#---------------------------------------------------------------------#

library(RColorBrewer)
kon_full <- streamDAGs("konza_full")
vis <- multi.path.visibility(kon_full, source =
                               c("SFT01_1", "02M11_1","04W04_2",
                                 "04T01_1", "04M13_1",
                                 "SFT02_1","01M06_1", "20M05_1", "04T02_2"),
                             sink = "SFM01_1", autoprint = FALSE)

knames <- colnames(vis$visibility.summary)

# Fig 5
pdf("Figp5.pdf",width = 9, height = 6.5)
cols <- brewer.pal(7,"Spectral")
local <- local.summary(kon_full)
m <- match(knames, colnames(local))
local1 <- local[,m]

b <- barplot(t(as.matrix(data.frame(scale(t(local1[c(1:4,6,11,12),]))))), beside = T, xlab = "", ylab = "Standardized local measures", yaxt = "n", xaxt = "n", space = c(0,.7), ylim = c(-2.1, 5.1), bty = "l")
cs <- par("usr") #x1, x2, y1, y2
lvl <- rep(1:46, each = 7)
x1 <- tapply(b, lvl, min)
x2 <- tapply(b, lvl, max)
gr <- gray(rep(c(0.75,0.85), 23))
rect(cs[1], cs[3], cs[2], cs[4], col = gray(.85)) #xleft, ybottom, xright, ytop
for(i in 1: 46){
  rect(x1[i], cs[3], x2[i], cs[4], col = gr[i], lwd = 2.5, border = gr[i])
}
#abline(h = c(-1,0,1,2,3,4), col = "lightgray")
#rug(xloc, col = "lightgray")
barplot(t(as.matrix(data.frame(scale(t(local1[c(1:4,6,11,12),]))))), beside = T, las = 2, 
        col = cols, cex.names = 0.65, border = NA, bty = "l", add = TRUE, space = c(0,0.7),
        legend.text = c(expression(paste(alpha, "-centrality")), 
                        "PageRank", "Improved closeness centrality", 
                        "Betweenness centrality", "Upstream network length", "In-eccentricity", "Mean efficiency"), 
        args.legend = list(x = "topleft", fill = cols, border = cols, box.col = gray(.7), cex = .8, bg = "white"), 
        ylab = "Standardized local measures", ylim = c(-2.1, 5))
dev.off()
#---------------------------------------------------------------------#
# Fig 6
pdf("Figp6.pdf",width = 6.5, height = 5.5)
vis <- multi.path.visibility(kon_full, source =
                               c("SFT01_1", "02M11_1","04W04_2",
                                 "04T01_1", "04M13_1",
                                 "SFT02_1","01M06_1", "20M05_1", "04T02_2"),
                             sink = "SFM01_1", autoprint = FALSE)
barplot(vis$visibility.summary, las = 2, cex.names = .6, ylab = "Visible nodes",
        legend.text = c("Downstream", "Upstream", "Both"),
        args.legend = list(x = "topleft", title = "Direction"))
dev.off()

#-----------------------------------------------------------------------#
# Fig 7
pdf("Figp7.pdf",width = 6.5, height = 5.5)
names <- c("Size", "Diameter", "Sources", "no. of paths to sink", "Mean path length to sink", expression(paste("Mean ", alpha,"-centrality")), "Strahler number", "Shreve number", "Randic-index", expression(paste(1^{st}," Zagreb index")), expression(paste(2^{nd}," Zagreb index")), "Atom bond connectivity", expression(paste(1^{st}," Geom.-arithmetic index")), "Harmonic index", "Harary index", "Global efficiency", expression(paste(italic(r),"(-,+)")), expression(paste(italic(r),"(-,-)")))

subset <- mur_node_pres_abs[seq(1,1163, length = 100),]
subset.nodate <- subset[,-1]

date2 <- matrix(ncol = 2, data = unlist(strsplit(subset[,1]," ")), byrow = T)
# walk global.summary through node presence / absence data
global <- matrix(ncol = 18, nrow = nrow(subset))
for(i in 1:nrow(subset)){
  global[i,] <- global.summary(delete.nodes.pa(murphy_spring, subset.nodate[i,]), sink = "OUT")
}
# standardize measures
scaled.global <- scale(global)
par(mar = c(6,4.2,1.5,9.5))
# plot
matplot(scaled.global, xaxt = "n", type = "l", col = hcl.colors(12, palette = "spectral"),
        ylab = "Standardized global measures")
legend(100, 1, lty = 1:5, col = hcl.colors(12, palette = "spectral"),
       legend = names, cex = .7, xpd = TRUE, xjust = -0.1, yjust = 0.7, bty = "n")
axis(side = 1, at = c(1,21,41,61,81,100), labels = date2[,1][c(1,21,41,61,81,100)],
     las = 2, cex.axis = .7)
mtext(side = 1, "Time", line = 4.5)
dev.off()
#---------------------------------------------------------------------#
# Fig 8
pdf("Figp8.pdf",width = 6.5, height = 5.5)
prob <- apply(mur_arc_pres_abs, 2, mean)
bsl <- bern.length(mur_lengths[,2], prob) # Bernoulli length
bcd <- bern.length(mur_lengths[,2], 1/prob) # Comm dist.
scale.both <- scale(cbind(bsl, bcd)) # standardize outcomes

par(mar = c(6.5,4.5,1,0.5)) # allow full arc names to be seen

barplot(t(scale.both), beside = T,las = 2, cex.names = .75, 
        legend.text = c("Bernoulli length", "Communication distance"), 
        args.legend = list(x = "topright", cex = 1), cex.axis = 1, cex.lab = 1.1,
        ylab = "Standardized measures")
dev.off()
#---------------------------------------------------------------------#
# Fig 9
pdf("Figp9.pdf",width = 6.5, height = 5.5)
icsl <- 1:nrow(subset) -> intact.to.sink -> a.cent -> harary -> global.eff
for(i in 1:nrow(subset)){
  temp.graph <- delete.nodes.pa(murphy_spring, subset.nodate[i,])
  # replace direction symbol for igraph comparability
  namelv <- gsub(" -> ", "|", mur_lengths[,1])
  a <- attributes(E(temp.graph))$vname
  w <- which(namelv %in% a)
  length.sub <- mur_lengths[,2][w]
  E(temp.graph)$weights <- length.sub
  icsl[i] <- ICSL(temp.graph)
  global.eff <- global.efficiency(temp.graph)
  intact.to.sink[i] <- size.intact.to.sink(temp.graph, "OUT")
  a.cent[i] <- mean(alpha.centrality(temp.graph), na.rm = T)
  harary[i] <- harary(temp.graph)
}
global <- cbind(icsl, global.eff, intact.to.sink, a.cent, harary)
# standardize measures
scaled.global <- scale(global)
par(mar = c(6,4.2,1.5,2))
# plot
matplot(scaled.global, xaxt = "n", type = "l", col = hcl.colors(5, palette = "spectral"),
        ylab = "Standardized global measures", lty = 1:2)
legend("topright", lty = 1:2, col = hcl.colors(5, palette = "spectral"),
       legend = c("ICSL", "Global efficiency", "Intact stream length to sink", "Alpha-centrality", "Harary"), cex = .8)
axis(side = 1, at = c(1,21,41,61,81,100), labels = date2[,1][c(1,21,41,61,81,100)],
     las = 2, cex.axis = .7)
mtext(side = 1, "Time", line = 4.5)

dev.off()
#---------------------------------------------------------------------#
# Fig 10
pdf("Figp10.pdf",width = 6.5, height = 6.5)
data <- mur_arc_pres_abs[1:10,]
b <- beta.posterior(p.prior = 0.5, dat = data, length = mur_lengths[,2], w = 1/3)

means <- b$alpha/(b$alpha + b$beta)
col <- gray(means/max(means))
par(mfrow = c(6,5), oma = c(4.5,5, 0.1, 1), mar = c(0,0,1.2,0.6))

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

plot(0:1, type = "n", axes = F, xlab = "", ylab = "")
legend(1.5, 0.8, xpd = NA, bty = "n", title = "Posterior mean", ncol = 3, fill = gray(c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)), legend = c("0-0.1","0.1-0.2", "0.2-0.3","0.3-0.4","0.4-0.5","0.5-0.6","0.7-0.8","0.8-0.9","0.9-1.0"), border = "grey")

#axis labels
mtext(side = 2, outer = T, expression(paste(italic(f),"(",theta[italic(k)],"|",italic(x[k]),")")), line = 2.8, cex = 1.2)
mtext(side = 1, outer = T, expression(paste(theta[italic(k)],"|",italic(x[k]))), line = 2.8, cex = 1.2)
dev.off()
#---------------------------------------------------------------------#
# Fig not in manuscript -- inverse beta distributions
pdf("Figadd2.pdf",width = 6.5, height = 6.5)
means <- (b$alpha + b$beta - 1)/(b$alpha - 1)
col <- gray(means/max(means))
par(mfrow = c(6,5), oma = c(4.5,5, 0.1, 1), mar = c(0,0,1.2,0.6))

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

plot(0:1, type = "n", axes = F, xlab = "", ylab = "")
legend(1.5, 0.8, xpd = NA, bty = "n", title = "Posterior mean", ncol = 3, fill = gray(c(0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)), legend = c("1.0-1.5","1.5-2.0", "2.0-2.5","2.5-3.0","3.0-3.5","3.5-4.5","4.5-5.0","5.0-7.0","7.0-10.0"), border = "grey")

#axis labels
mtext(side = 2, outer = T, expression(paste(italic(f),"(",theta[italic(k)],"|",italic(x[k]),")",""^{-1})), line = 2.8, cex = 1.2)
mtext(side = 1, outer = T, expression(paste("(",theta[italic(k)],"|",italic(x[k]),")",""^{-1})), line = 2.8, cex = 1.2)
dev.off()
