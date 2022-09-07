dinvbeta <- function(x, alpha, beta){
  alpha <- rep(alpha, length(x))
  beta <- rep(beta, length(x))
  d <- ifelse(x <= 1 | x == Inf, 0,
              gamma(alpha + beta)/(gamma(alpha)*gamma(beta)) *
                (1/x)^(alpha + 1) * (1 - 1/x)^(beta - 1))
  d
}
