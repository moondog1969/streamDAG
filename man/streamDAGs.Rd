\name{streamDAGs}
\alias{streamDAGs}

\title{
Stream DAG datasets
}
\description{
The function contains a number of stream direct acyclic graph datasets written in \emph{igraph} format. See: \code{\link[igraph]{graph_from_literal}}.  Many of the graphs were based on sampling regimes for the National Science Foundation Aquatic Intermittency Effects on Microbiomes in Streams (AIMS) project. 
}
\usage{
streamDAGs(graph = c("dc_piezo_full", "dc_full", "gj_full16", "gj_synoptic_2023", 
"gj_full", "gj_piezo_full", "jd_piezo_full", "jd_piezo_full_2023","jd_full", 
"konza_full", "KD0521", "KD0528", "KD0604", "mur_full", "td_full", "wh_full", 
"pr_full"))
}
\arguments{
\item{graph}{
Currently, one of  \code{"dc_piezo_full"}, \code{"dc_full"}, \code{"gj_full16"}, \code{"gj_full16"}, \code{"gj_synoptic_2023"}, \code{"gj_full"}, \code{"gj_piezo_full"}, \code{"jd_piezo_full"}, \code{"jd_piezo_full_2023"}, \code{"jd_full"}, \code{"konza_full"}, \code{"KD0521"}, \code{"KD0528"}, \code{"KD0604"}, \code{"mur_full"}, \code{"pr_full"},  \code{"td_full"}, or \code{"wh_full"} (see Details below).   
}  
}

\details{
  Currently, the following \code{graph} options exist.  Note that many of the graphs have associated datasets. Obtaining these datasets is now greatly simplified through the use of \code{\link{get.AIMS.data}} (code steps shown below are unnecessary).
  \enumerate{
     \item{\code{"dc_piezo_full"} codifies the Dry Creek stream network in southwestern Idaho for STIC (Stream Temperature, Intermittency, and Conductivity) sensors, confluences, and piezometer locations (outlet coordinates: 43.71839\eqn{^\circ}N, 116.13747\eqn{^\circ}W).
     \itemize{\item{Network spatial coordinates for this graph can be subset from \code{\link{AIMS.node.coords}} using: \code{AIMS.node.coords$site == "DC"}.}
     \item{Nodal surface water presence/absence data for this graph can be obtained directly from \code{\link{dc_node_pres_abs}}.}
          \item{Arc lengths for this graph can be obtained directly from \code{\link{dc_lengths}}.}
     }}
     
     \item{\code{"dc_full"} codifies the Dry Creek stream network in southwestern Idaho but only for STICs and confluences, not piezometer locations (outlet coordinates: 43.71839\eqn{^\circ}N, 116.13747\eqn{^\circ}W). 
\itemize{
\item{Network spatial coordinates for this graph can be subset from \code{\link{AIMS.node.coords}} using: \code{AIMS.node.coords$site == "DC" & AIMS.node.coords$STIC_inferred_PA}.}
     \item{Nodal surface water presence/absence data for this graph can be obtained using \code{dc <- streamDAGs("dc_full")} followed by \code{dc_node_pres_abs[attributes(V(dc))$names]}.}
}}
      
    \item{\code{"gj_full16"} codifies nodes established at the Gibson Jack drainage in southeast Idaho, as defined in 2016 (outlet coordinates: 42.767180\eqn{^\circ}N, 112.480240\eqn{^\circ}W).}
    
    \item{\code{"gj_full"} codifies nodes established at the Gibson Jack drainage in southeast Idaho for STIC sensors in 2022-2023, along with confluence locations. Piezometer locations not included (outlet coordinates: 42.767180\eqn{^\circ}N, 112.480240\eqn{^\circ}W). 
    \itemize{\item{Network spatial coordinates for this graph can be subset from \code{\link{AIMS.node.coords}} using: \code{AIMS.node.coords$site == "GJ" & AIMS.node.coords$STIC_inferred_PA}.}
    \item{Nodal surface water presence/absence data for this graph can be obtained from: \code{\link{gj_node_pres_abs}} using \code{gj <- streamDAGs("gj_full"); vnames <- attributes(V(gj))$names; w <- which(names(gj_node_pres_abs)[-1] \%in\% vnames) + 1; node.pa <- gj_node_pres_abs[,c(1,w)]}.}
    \item{Arc lengths for this graph can be obtained from \code{\link{gj_lengths_piezo_full}} using \code{gj <- streamDAGs("gj_full"); anames <- attributes(E(gj))$vnames; enames <- gsub("\\|", " -> ", anames); m <- match(enames, gj_lengths_piezo_full[,1])
    gj_lengths_piezo_full[m,]}.}}}

    \item{\code{"gj_piezo_full"} codifies nodes established at the Gibson Jack drainage in southeast Idaho, by the the AIMS team which include longterm STICs and piezometers, along with confluence locations (outlet coordinates: 42.767180\eqn{^\circ}N, 112.480240\eqn{^\circ}W). 
    \itemize{\item{Network spatial coordinates for this graph can be subset from \code{\link{AIMS.node.coords}} using: \code{AIMS.node.coords$site == "GJ" & (AIMS.node.coords$STIC_inferred_PA | AIMS.node.coords$piezo)}.}
    \item{Nodal surface water presence/absence data for this graph can be obtained using \code{gj <- streamDAGs("gj_piezo_full")} followed by \code{gj_node_pres_abs[attributes(V(gj))$names]}.}
    \item{Arc lengths for this graph can be obtained from \code{\link{gj_lengths_piezo_full}} using \code{gj <- streamDAGs("gj_piezo_full"); anames <- attributes(E(gj))$vnames; enames <- gsub("\\|", " -> ", anames); m <- match(enames, gj_lengths_piezo_full[,1]); gj_lengths_piezo_full[m,]}.}
    }}
    
    \item{\code{"gj_synoptic_2023"} codifies nodes established at the Gibson Jack drainage in southeast Idaho by the AIMS team during synoptic sampling in 2023, includes piezometers and additional sites to those sampled in \code{"gj_full"} (outlet coordinates: 42.767180\eqn{^\circ}N, 112.480240\eqn{^\circ}W).}
    
    \item{\code{"jd_piezo_full"} codifies the Johnson Draw stream network in southwestern Idaho for both STIC and and piezometer locations (outlet coordinates: 43.12256\eqn{^\circ}N, 116.77630\eqn{^\circ}W).
    \itemize{\item{Network spatial coordinates for this graph can be subset from \code{\link{AIMS.node.coords}} using: \code{AIMS.node.coords$site == "JD" & AIMS.node.coords$New_in_2023 == FALSE}.}
    \item{Nodal surface water presence/absence data for this graph can be obtained from \code{\link{jd_node_pres_abs}} using \code{jd <- streamDAGs("jd_piezo_full")} followed by \code{jd_node_pres_abs[attributes(V(jd))$names]}.}
        \item{Arc lengths can be subset from \code{\link{jd_lengths_2023}} using something like: \code{jd <- streamDAGs("jd_piezo_full"); anames <- attributes(E(jd))$vnames; nnames <- gsub("\\\|", " -> ", anames); jd_lengths_2023[which(jd_lengths_2023[,1] \%in\% nnames ),]}.}
    }
    }

\item{\code{"jd_piezo_full_2023"} codifies the Johnson Draw stream network in southwestern Idaho for both STIC and and piezometer locations, along with new STIC locations added in 2023 (outlet coordinates: 43.12256\eqn{^\circ}N, 116.77630\eqn{^\circ}W).
    \itemize{
    \item{Network spatial coordinates for this graph can be subset from \code{\link{AIMS.node.coords}} using: \code{AIMS.node.coords$site == "JD"}.}
        \item{Nodal surface water presence/absence data for this graph can be obtained from \code{\link{jd_node_pres_abs}} using \code{jd <- streamDAGs("jd_piezo_full_2023")} followed by \code{jd_node_pres_abs[attributes(V(jd))$names]}.}
    \item{Arc lengths can be subset from \code{\link{jd_lengths_2023}} using something like: \code{jd <- streamDAGs("jd_piezo_full_2023"); anames <- attributes(E(jd))$vnames; nnames <- gsub("\\\|", " -> ", anames); jd_lengths_2023[which(jd_lengths_2023[,1] \%in\% nnames ),]}}.}}
    
\item{\code{"jd_full"} codifies the Johnson Draw stream network in southwestern Idaho, but only for STICs, not piezometers (outlet coordinates: 43.12256\eqn{^\circ}N, 116.77630\eqn{^\circ}W).  
     \itemize{
     \item{Network spatial coordinates for this graph can be subset from \code{\link{AIMS.node.coords}} using: \code{AIMS.node.coords$site == "JD" & AIMS.node.coords$STIC_inferred_PA}.}
     \item{Nodal surface water presence/absence data for this graph can be obtained using \code{jd <- streamDAGs("jd_full")} followed by \code{jd_node_pres_abs[attributes(V(jd))$names]}.}
     \item{Arc lengths can be obtained from \code{\link{jd_lengths}}.}}}
     
     \item{\code{"konza_full"} provides codification of a complete intermittent stream network of Konza Prairie in the northern Flint Hills region of Kansas (outlet coordinates: 39.11394\eqn{^\circ}N, 96.61153\eqn{^\circ}W).  \itemize{\item{Network spatial coordinates for this graph can be obtained directly from \code{\link{kon_coords}}}
     \item{Arc lengths can be obtained from \code{\link{kon_lengths}}.}}}
     
     \item{Options \code{"KD0521"},\code{"KD0528"}, and \code{"KD0604"} provide networks for Konza Prairie at 05/21/2021 (before spring snow melt), 05/28/2021 (during spring snow melt) and 06/04/2021 (drying following snow melt), respectively.} 
    
    \item{\code{"mur_full"} is an \emph{igraph} codification of the complete Murphy Creek dataset from the Owyhee Mountains in SW Idaho (outlet coordinates: 43.256\eqn{^\circ}N, 116.817\eqn{^\circ}W) established in 2019 by Warix et al. (2021), also see Aho et al. (2023).  
    \itemize{\item{Network spatial coordinates for the graph can be obtained directly from \code{\link{mur_coords}}}
    \item{Nodal surface water presence/absence data for this graph can be obtained from \code{\link{mur_node_pres_abs}}}
    \item{Arc lengths can be obrtained from \code{\link{mur_lengths}}.}}}
    
      \item{\code{"pr_full"} codifies the Painted Rock stream network in northern Alabama (outlet coordinates: 34.96867\eqn{^\circ}N, 86.16544\eqn{^\circ}W). 
      \itemize{
      \item{Network spatial coordinates for this graph can be subset from \code{\link{AIMS.node.coords}} using: \code{AIMS.node.coords$site == "PR"}}.}}
      
      \item{\code{"td_full"} codifies the Talladega stream network in central Alabama (outlet coordinates: 33.76218\eqn{^\circ}N, 85.59552\eqn{^\circ}W).  
      \itemize{\item{Network spatial coordinates for this graph can be subset from \code{\link{AIMS.node.coords}} using: \code{AIMS.node.coords$site == "TD"}.}
}}

\item{\code{"wh_full"} codifies the Weyerhauser stream network in western Alabama (outlet coordinates: 32.98463\eqn{^\circ}N, 88.01227\eqn{^\circ}W). 
      \itemize{\item{Network spatial coordinates for this graph can be obtained from \code{\link{AIMS.node.coords}} using: \code{AIMS.node.coords$site == "WH"}.}}}
}}
      

\value{Returns a graph object of class \emph{igraph}.}
\references{
Aho, K., Derryberry, D., Godsey, S. E., Ramos, R., Warix, S., & Zipper, S. (2023). The communication distance of non-perennial streams.  \emph{EarthArvix} doi: 10.31223/X5Q367

Warix, S. R., Godsey, S. E., Lohse, K. A., & Hale, R. L. (2021), Influence of groundwater and topography on stream drying in semi-arid headwater streams. \emph{Hydrological Processes, 35}(5), e14185.
}
\author{
Ken Aho, Maggie Kraft, Rob Ramos, Rebecca L. Hale, Charles T. Bond, Arya Legg 
}
\examples{
streamDAGs("mur_full")
}
