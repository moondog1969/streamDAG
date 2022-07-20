#--- CDF ---#

pinvbeta <- function(x, alpha, beta){
  integrand <-  function(x){
    gamma(alpha + beta)/(gamma(alpha)*gamma(beta)) *
      (1/x)^(alpha + 1) * (1 - 1/x)^(beta - 1)}
  area <- integrate(integrand, 1, x)
  area$value
}
