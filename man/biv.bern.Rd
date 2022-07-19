\name{biv.bern}
\alias{biv.bern}

\title{
Bivariate Bernoulli Distribution
}
\description{
Densities (probabilities) of a bivariate Bernoulli distribution, \eqn{Y_1,Y_2}.
}
\usage{
biv.bern(p11, p10, p01, p00, y1, y2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{p11}{
     The probability that \eqn{y_1,y_2 = 1,1}.
}
  \item{p10}{
     The probability that \eqn{y_1,y_2 = 1,0}.
}
  \item{p01}{
     The probability that \eqn{y_1,y_2 = 0,1}.
}
  \item{p00}{
     The probability that \eqn{y_1,y_2 = 0,0}.
}
  \item{y1}{
     Outcome for \eqn{Y_1}.
}
  \item{y2}{
     Outcome for \eqn{Y_2}.
}
}

\value{
Densities (probability) of the joint Bernoulli distribution.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Ken Aho
}

\examples{
biv.bern(0.25,0.25,0.25,0.25,1,0)
biv.bern(0.1,0.4,0.3,0.2,1,0)
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
