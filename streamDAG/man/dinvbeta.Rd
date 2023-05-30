\name{dinvbeta}
\alias{dinvbeta}
\alias{pinvbeta}
\alias{rinvbeta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Inverse Beta Distribution
}
\description{
Calculates density (\code{dinvbeta}), lower-tailed probability (\code{pinvbeta}) and obtains random outcomes (\code{rinvbeta}) for an inverse beta distribution
}
\usage{
dinvbeta(x, alpha, beta)
pinvbeta(x, alpha, beta)
rinvbeta(n, alpha, beta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{x}{
Quantile vector or scalar at which to evaluate density or probability.
}
  \item{alpha}{
Alpha parameter
}
  \item{beta}{
Beta parameter
}
  \item{n}{
Number of random outcomes to be generated.
}
}

\value{
Returns density, probability, and random outcomes for an inverse beta distribution.
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Ken Aho and Dwayne Derryberry
}

\seealso{See Also \code{\link{dbeta}}.
}
\examples{
dinvbeta(1,1,1)
pinvbeta(1,1,1)
rinvbeta(1,1,1)
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
