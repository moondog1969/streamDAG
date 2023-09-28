\name{R.bounds}
\alias{R.bounds}
\alias{max_r}
\alias{min_r}
\title{Bounds for the correlation of two (or more) Benrnoulli random variables
}
\description{
Replaces impossible correlations (values too small or too large) with minimum and maximum correlations, respectively.
}
\usage{
min_r(p1, p2)
max_r(p1, p2)
R.bounds(p, R, pad = 0.001)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p1}{
Probability of success for first random variable
}
  \item{p2}{
Probability of success for second random variable
}
  \item{p}{
Vector of marginal probabilities for multivariate Bernoulli random variables, for \code{R.bounds}.
}
  \item{R}{
Raw correlation matrix for random variables
}
  \item{pad}{
Padding (in correlation units) to adjust the returned correlation matrix with respect extremal values. 
}
}
\details{
The functions \code{r.min} and \code{r.max} define minimum and maximimum possible correlations. The function \code{R.bounds} replaces impossibly large or small values with maximally large or small values repectively.    
}
\value{Functions return a scalar defining minimum or maximimum possible correlations.  See Aho et al. (2023).}
\author{
Ken Aho
}
\references{Aho, K., Derryberry, D., Godsey, S. E., Ramos, R., Warix, S., Zipper, S. (2023) The communication distance of non-perennial streams. EarthArXiv \url{https://eartharxiv.org/repository/view/4907/}}
\examples{
min_r(0.6, 0.9)
max_r(0.1, 0.2)

x1 <- rep(c(1,0),5)
x2 <- c(rep(1,7), rep(0,3))
x3 <- c(rep(1,3), rep(0,7))
R <- cor(cbind(x1, x2, x3))
R.bounds(c(0.5, 0.7, 1), R)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
