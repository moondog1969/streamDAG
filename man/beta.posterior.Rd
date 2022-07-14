\name{beta.posterior}
\alias{beta.posterior}

\title{
Posterior Beta and Inverse-beta summaries
}
\description{
Calculates summaries for beta and inverse-beta posteriors given prior probabilities for success, binary data and prior weight specification. Summaries include beta and inverse beta posterior means and variances and stream length and communication distance summaries given that stream \code{length} is provided for intermittent stream segments.}
\usage{

beta.posterior(p.prior, dat, length = NULL, w = 0.5)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{p.prior}{
Prior probability for success for the beta prior.  The beta prior for the probability of success (e.g., stream presence) for \emph{k}th outcome (e.g., stream segment) is defined as: \eqn{ \theta_k \sim BETA(\alpha, \beta = t \alpha)}, where \eqn{\frac{1}{1+t} = p_{prior}}.  This results in: \eqn{E(\theta_k) = p_{prior}}.
}
  \item{dat}{An \eqn{n \times s} matrix of binary outcomes, wwhere \emph{n} is the number of observations (e.g., stream observations over time) and \emph{s} is the number experimental units observed, (e.g., stream segments).   
}
  \item{length}{
An \eqn{n \times 1} vector containing stream segement lengths to allow calculation of mean stream stream length and communication distance. 
}
  \item{w}{
Weight for the prior distribution compared to the actual data (generally a aproportion).
}
}
\details{
As a framework we assume a conjugate beta prior \eqn{ \theta_k \sim BETA(\alpha, \beta)} and a binomial likelihood framework \eqn{ \boldsymbol{x}_k \mid \theta_k \sim BIN(n, \theta_k)} resulting in the posterior \eqn{\theta_k \mid \boldsymbol{x}_k \sim BETA(\alpha + \sum \boldsymbol{x}_k, \beta + n - \sum \boldsymbol{x}_k)}. 
}
\value{
Returns a list with components:

\item{\code{alpha}}{The \eqn{\alpha} shape parameters for the beta and inverse beta posteriors.}
\item{\code{beta}}{The \eqn{\beta} shape parameters for the beta and inverse beta posteriors.}
\item{\code{mean}}{The means of the beta posteriors.}
\item{\code{var}}{The variances of the beta posteriors.}
\item{\code{mean.inv}}{The means of the inverse-beta posteriors.}
\item{\code{var.inv}}{The variances of the inverse-beta posteriors.}
\item{\code{Com.dist}}{If \code{length} is suppled, the mean communication distances of the network.}
\item{\code{var.inv}}{If \code{length} is suppled, the mean stream length of the network.}
}

\author{
Ken Aho
}
\seealso{
\code{\link{dinvbeta}}.
}


