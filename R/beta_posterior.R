beta.posterior <- function(p.prior, dat, length = NULL, w = 0.5){
  n <- nrow(dat)
  x <- apply(dat,2,sum)
  alpha <- w * n * p.prior + x
  beta <- w * n * (1 - p.prior) + n - x
  mean <- alpha/(alpha + beta)
  var <- (alpha * beta)/((alpha + beta)^2 * (alpha + beta + 1))
  mean.inv <- (alpha + beta - 1)/(alpha - 1)
  var.inv <- (alpha + beta - 1)/(alpha - 1) * beta/((alpha - 1) * (alpha - 2))
  w <- which(var.inv <= 0)
  var.inv[w] <- NA
  Com <- sum(mean.inv * length)
  L <- sum(mean * length)
  
  res <- list()
  res$alpha <- alpha
  res$beta <- beta
  res$mean <- mean
  res$var <- var
  res$mean.inv <- mean.inv
  res$var.inv <- var.inv
  
  res$Com.dist <- Com
  res$Length <- L
  res$x <- x
  res
}  