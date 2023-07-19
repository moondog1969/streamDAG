\name{I.D}
\alias{I.D}
\title{
Generalized DAG indices
}
\description{
Calculates global generalized topopological indices for a digraph
}
\usage{

I.D(G, mode = "gen.rand", alpha = -1/2, mult = FALSE, degrees = "out.in")
}

\arguments{
  \item{G}{
    Graph object of class.  See \code{\link[igraph]{graph_from_literal}}.
}
  \item{mode}{
    One of \code{"gen.rand"},\code{"gen.sum.con"}, \code{"ABC"}, \code{"GA"}, \code{"harm"}, \code{"aug.rand"}.
}
  \item{alpha}{
  Exponent value for forms of omega with alpha exponent.
}
  \item{mult}{
     Logical if \code{TRUE} use experimental multiplicative measures. 
}
  \item{degrees}{
     Degree designations for the arc \eqn{\vec{uv}}.  One of \code{"out.in"}, \code{"out.out"}, \code{"in.out"}, \code{"in.in"}. See Details below. The default designation, \code{"out.in"}, is strongly reccomended for stream DAGs.
}

}
\details{
 For an arc \eqn{a={\overrightarrow{uv}}}, \eqn{a \in A}, we denote the out degree of \eqn{u} as \eqn{d_u^+}, and the in degree of \eqn{v} as \eqn{d_v^-}. Now let \eqn{I(D)} represent a generalized topopological index for a digraph, \eqn{D} (cf. Deng et al., 2021) that depends on \eqn{d_u^+} and \eqn{d_v^-}:

\deqn{I(D)=1/2 \sum_{uv \in A}\omega(d_u^+,d_v^-)}

Six basic configurations for \eqn{I(D)} can be recognized:

1.	If \eqn{\omega(x,y)=(xy)^\alpha}, for \eqn{\alpha \neq 0}, then \eqn{I(D)} is the \emph{general directed Randic index} (Kincaid et al., 2016) for \eqn{D}. Specific variants include the \emph{Randic index} \eqn{(\alpha =-1/2)}, the \emph{second Zagreb index} \eqn{(\alpha =1)} and the \emph{second modified Zagreb index} \eqn{(\alpha =-1)} (Anthony & Marr, 2021).

2.	If \eqn{\omega(x,y)=(x+y)^\alpha}, then \eqn{I(D)} is the \emph{general sum-connectivity index} for \emph{D} (Deng et al., 2021). Further, if \eqn{\omega(x,y)=2(x+y)^\alpha}, then \eqn{I(D)} is the \emph{sum connectivity} (Zhou & Trinajstic, 2009), and the directed \emph{first Zagreb index} (Anthony & Marr, 2021) for \eqn{\alpha =-1/2} and \eqn{\alpha=1}, respectively .

3.	If \eqn{\omega(x,y)=\sqrt{((x+y-2)/xy)}}, then \eqn{I(D)} is the \emph{atom bond connectivity} of \emph{D} (Estrada et al., 1998).

4.	If \eqn{\omega(x,y)=\sqrt{xy}/(1/2(x+y))}, then \eqn{I(D)} is the \emph{geometric-arithmetic index} for \eqn{D} (Vukicevic & Furtula, 2009).

5. If \eqn{\omega(x,y)=2/(x+y)}, then \eqn{I(D)} is the \emph{harmonic index} of \eqn{D} (Favaron et al., 1993).

6. If \eqn{\omega(x,y)=\left(\frac{xy}{x+y-2}\right)^3}, then \eqn{I(D)} is the \emph{augmented Randic index} of \eqn{D} (Furtula et al. 2010).  This index is not reccomended for stream DAGs as it will contained undefined terms for any network with unbranched paths.


More options are possible under the generalization of Kincaid (1996).  Specifically, for an arc \eqn{a=\vec{uv},a\in A}, let \eqn{\gamma,\tau\in-,+} index the degree type: \eqn{-= in, +=out}.  Then, four combinations of \eqn{d_u^\gamma, d_v^\tau} can occur, resulting in four different versions of each \eqn{I(D)} metric described above.  These combinations are: \eqn{d_u^+,d_v^- } (as shown above), \eqn{d_u^+,d_v^+},  \eqn{d_u^-,d_v^-}, and \eqn{d_u^-,d_v^+}.  The default \eqn{d_u^+,d_v^-} is strongly reccomended for stream DAGs over other variants.

}
\value{
Index values for a DAG
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Anthony, B. M., & Marr, A. M. (2021). Directed zagreb indices. \emph{Graphs and Combinatorial Optimization: From Theory to Applications: CTW 2020 Proceedings}, 181-193.

Deng, H., Yang, J., Tang, Z., Yang, J., & You, M. (2021). On the vertex-degree based invariants of digraphs. arXiv Preprint \emph{arXiv:2104.14742}.

Estrada, E., Torres, L., Rodriguez, L., & Gutman, I. (1998). \emph{An atom-bond connectivity index: Modelling the enthalpy of formation of alkanes}. NISCAIR-CSIR, India.

Favaron, O., Maheo, M., & Sacle, J.-F. (1993). Some eigenvalue properties in graphs (conjectures of graffitii). \emph{Discrete Mathematics, 111}(1-3), 197-220.

Furtula, B., Graovac, A., & Vukicevic, D. (2010). Augmented Zagreb index. \emph{Journal of Mathematical Chemistry, 48}(2), 370-380.

Kincaid, R. K., Kunkler, S. J., Lamar, M. D., & Phillips, D. J. (2016). Algorithms and complexity results for finding graphs with extremal Randic index. Networks, 67(4), 338-347.

Vukicevic, D., & Furtula, B. (2009). Topological index based on the ratios of geometrical and arithmetical means of end-vertex degrees of edges. \emph{Journal of Mathematical Chemistry, 46}(4), 1369-1376.

Zhou, B., & Trinajstic, N. (2009). On a novel connectivity index. \emph{Journal of Mathematical Chemistry}, 46(4), 1252-1270.
}

\author{
Ken Aho, Gabor Csardi wrote \code{\link[igraph]{degree}}
}


\seealso{
\code{\link[igraph]{degree}}
}

\examples{
network_a <- graph_from_literal(a --+ b, c --+ d, d --+ e, b --+ e, 
e --+ j, j --+ m, f --+ g, g --+ i, h --+ i, i --+ k, k --+ l, 
l --+ m, m --+ n,  n --+ o)
I.D(network_a)
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
