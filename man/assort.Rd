\name{assort}
\alias{assort}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Assortativity
}
\description{
Calculates graph assortativity
}
\usage{
assort(G, mode = "in.out")
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{G}{
    Graph object of class \code{igraph}. See \code{\link[igraph]{graph_from_literal}}.
}
\item{mode}{One of \code{"in.in"}, \code{"in.out"}, \code{"out.out"}, \code{"out.in"}, or \code{"all"}.
}
}
\details{
The definitive measure of graph assortativity is the Pearson correlation coefficient of the degree of pairs of adjacent nodes (Newman, 2002). Let \eqn{\overrightarrow{u_iv_i}} define nodes and directionality of the ith arc, \eqn{i=1,2,3,\ldots,m}, let \eqn{\gamma,\tau\in{-,+}} index the degree type: \eqn{-=in, +=out}, and let \eqn{\left(u_i^\gamma,v_i^\tau\right)}, be the \eqn{\gamma-} and \eqn{\tau-}degree of the \emph{i}th arc. Then, the general form of assortativity index is:

\deqn{r\left(\gamma,\tau\right)=m^{-1}\frac{\sum_{i= 1}^m (u_i^\gamma-\bar{u}^\gamma)(v^\tau_i-\bar{v}^\tau)}{s^\gamma s^\tau}
}

where \eqn{\bar{u}^\gamma} and \eqn{\bar{v}^\gamma} are the arithmetic means of the \eqn{u_i^\gamma}s and \eqn{v_i^\tau}s, and \eqn{s^\gamma} and \eqn{s^\tau} are the population standard deviations of the \eqn{u_i^\gamma}s and \eqn{v_i^\tau}s. Under this framework, there are four possible forms to \eqn{r\left(\gamma,\tau\right)} (Foster et al., 2010). These are: \eqn{r\left(+,-\right), r\left(-,+\right), r\left(-,-\right)}, and \eqn{r\left(+,+\right)}.


}
\value{
Assortativity coefficeint outcome(s)
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
  Newman, M. E. (2002). Assortative mixing in networks. \emph{Physical Review Letters, 89}(20), 208701.

  Foster, J. G., Foster, D. V., Grassberger, P., & Paczuski, M. (2010). Edge direction and the structure of networks. \emph{Proceedings of the National Academy of Sciences, 107}(24), 10815-10820.
}
\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{degree}} 
}

\examples{
network_a <- graph_from_literal(a --+ b, c --+ d, d --+ e, b --+ e, e --+ j, 
j --+ m, f --+ g, g --+ i, h --+ i, i --+ k, k --+ l, l --+ m, m --+ n,  
n --+ o)
assort(network_a)
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
