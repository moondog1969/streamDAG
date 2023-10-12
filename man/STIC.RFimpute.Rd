\name{STIC.RFimpute}
\alias{STIC.RFimpute}

\title{
A wrapper for missForest for random forest STIC imputation 
}
\description{
A simple wrapper for the \code{\link[missForest]{missForest}} random forest imputation algorithm. \code{STIC.RFimpute} first converts STIC (Stream Temperature, Intermittency, and Conductivity) presence/absence data to categorical outcomes to avoid regression fitting.  One should consult \code{\link[missForest]{missForest}} for specifics on the underlying algorithm.  
}
\usage{
STIC.RFimpute(p.a, ...)
}
\arguments{
  \item{p.a}{
Optimally, a dataframe containing presence absence data at sites (columns) over time (rows).
}
  \item{\dots}{
Additional arguments from \code{\link[missForest]{missForest}}
}
}
\value{
Provides the conventional unaltered \code{\link[missForest]{missForest}} output.
}
\references{
Stekhoven, D.J. and Buehlmann, P. (2012), 'MissForest - nonparametric missing value imputation for mixed-type data', Bioinformatics, 28(1) 2012, 112-118, doi: 10.1093/bioinformatics/btr597
}
\author{
Daniel J. Stekhoven, <stekhoven@stat.math.ethz.ch>
}
\examples{
arc.pa <- data.frame(matrix(ncol = 3, data = c(1,1,1, 0,1,1, 1,1,1, 0,NA,1), byrow = TRUE))
names(arc.pa) <- c("n1 --> n2", "n2 --> n3", "n3 --> n4")

STIC.RFimpute(arc.pa)
}

