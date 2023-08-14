plot_degree.dist <- function(G, mode = "all", exp.lambda = c(1.1, 3/2, 2), leg.loc = "topright"){
  dd <- degree.distribution(G, mode = mode)
  d <- (length(dd)-1)
  
  cd1 <- degree.dists(0:d, exp.lambda = exp.lambda[1]) 
  cd2 <- degree.dists(0:d, exp.lambda = exp.lambda[2]) 
  cd3 <- degree.dists(0:d, exp.lambda = exp.lambda[3]) 
  
  lim <- range(c(dd,cd1,cd2,cd3))
  
  plot(0:d, dd, type = "o", col = 1, lty = 1, pch = 19, ylim = c(lim[1], lim[2]),
       xlab = "Degree", ylab = expression(paste(italic(f),"(degree)")))
  points(0:d, cd1, type = "o", col = 2, lty = 2, pch = 20)
  points(0:d, cd2, type = "o", col = 4, lty = 3, pch = 21)
  points(0:d, cd3, type = "o", col = 5, lty = 4, pch = 22)
  legend(leg.loc, pch = c(19,20,21,22), col = c(1,2,4,5), lty =c(1,2,3,4), 
         legend = c("Obs.", 
                 bquote(paste("exp(",lambda,")" == .(exp.lambda[1]))), 
                 bquote(paste("exp(",lambda,")" == .(exp.lambda[2]))), 
                 bquote(paste("exp(",lambda,")" == .(exp.lambda[3])))))
  
}