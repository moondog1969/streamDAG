get.AIMS.data <- function(graph = "mur_full", supress.message = FALSE){
  indices <- c("dc_piezo_full", "dc_full", "gj_full16", "gj_synoptic_2023",  "gj_full", "gj_piezo_full", "jd_piezo_full", "jd_piezo_full_2023","jd_full", "konza_full", "KD0521", "KD0528", "KD0604", "mur_full", "td_full", "wh_full", 
               "pr_full")
  
  method <- match.arg(graph, indices)
  graphL <- streamDAGs(method)
  vnames <- attributes(V(graphL))$names
  anames <- attributes(E(graphL))$vnames
  enames <- gsub("\\|", " -> ", anames)
  
AIMS.node.coords <- mur_coords <- mur_lengths <- mur_node_pres_abs <- dc_lengths<- dc_node_pres_abs <- dc_lengths <- dc_node_pres_abs <- gj_lengths_piezo_full <- gj_node_pres_abs <- gj_lengths_piezo_full <- gj_node_pres_abs <- jd_lengths_2023 <- jd_node_pres_abs <- jd_lengths_2023 <- jd_node_pres_abs <- jd_lengths <- jd_node_pres_abs <- kon_coords <- kon_lengths <- NULL

  data(AIMS.node.coords, envir = environment())
  
  if(method == "mur_full"){
    
    #------------------- coords ----------------------#  
    data(mur_coords, envir = environment())
    m <- match(vnames, mur_coords$Object.ID)  
    coords <- mur_coords[m,]
    #------------------- lengths ----------------------#
    data(mur_lengths, envir = environment())
    m <- match(enames, mur_lengths[,1])
    arc.length <- mur_lengths[m,]    
    #------------------- node.pa ----------------------#
    data(mur_node_pres_abs, envir = environment())
    w <- which(names(mur_node_pres_abs)[-1] %in% vnames) + 1
    node.pa <- mur_node_pres_abs[,c(1,w)]
  }
  
  if(method == "dc_piezo_full"){
    #------------------- coords ----------------------#  
    coords <- AIMS.node.coords[AIMS.node.coords$site == "DC",]
    m <- match(vnames, coords$Object.ID)  
    coords <- coords[m,]
    #------------------- lengths ----------------------#
    data(dc_lengths, envir = environment())
    w <- which(dc_lengths[,1] %in% enames)
    arc.length <- dc_lengths[w,]    
    #------------------- node.pa ----------------------#
    data(dc_node_pres_abs, envir = environment())
    w <- which(names(dc_node_pres_abs)[-1] %in% vnames) + 1
    node.pa <- dc_node_pres_abs[,c(1,w)]
    }
  
  if(method == "dc_full"){
    #------------------- coords ----------------------#  
    coords <- AIMS.node.coords[AIMS.node.coords$site == "DC" & AIMS.node.coords$STIC_inferred_PA,]
    m <- match(vnames, coords$Object.ID)  
    coords <- coords[m,]
    #------------------- lengths ----------------------#
    data(dc_lengths, envir = environment())
    w <- which(dc_lengths[,1] %in% enames)
    arc.length <- dc_lengths[w,]   
    #------------------- node.pa ----------------------#
    data(dc_node_pres_abs, envir = environment())
    w <- which(names(dc_node_pres_abs)[-1] %in% vnames) + 1
    node.pa <- dc_node_pres_abs[,c(1,w)]
  }
  
  if(method == "gj_full16"){
    #------------------- coords ----------------------#  
    coords <- paste("No coords available for", method)
    #------------------- lengths ----------------------#
    arc.length <- paste("No arc lengths available for", method)   
    #------------------- node.pa ----------------------#
    node.pa <- paste("No node p.a data available for", method)   
}
  
  if(method == "gj_full"){
    #------------------- coords ----------------------#  
    coords <- AIMS.node.coords[AIMS.node.coords$site == "GJ" & AIMS.node.coords$STIC_inferred_PA,]
    m <- match(vnames, coords$Object.ID)  
    coords <- coords[m,]
    #------------------- lengths ----------------------#
    data(gj_lengths_piezo_full, envir = environment())
    m <- match(enames, gj_lengths_piezo_full[,1])
    arc.length <- gj_lengths_piezo_full[m,]
    
    #------------------- node.pa ----------------------#
    data(gj_node_pres_abs, envir = environment())
    w <- which(names(gj_node_pres_abs)[-1] %in% vnames) + 1
    node.pa <- gj_node_pres_abs[,c(1,w)]
  }
  
  if(method == "gj_piezo_full"){
    #------------------- coords ----------------------#  
    coords <- AIMS.node.coords[AIMS.node.coords$site == "GJ" & (AIMS.node.coords$STIC_inferred_PA | AIMS.node.coords$piezo),]
    m <- match(vnames, coords$Object.ID)  
    coords <- coords[m,]
    #------------------- lengths ----------------------#
    data(gj_lengths_piezo_full, envir = environment())
    m <- match(enames, gj_lengths_piezo_full[,1])
    arc.length <- gj_lengths_piezo_full[m,]
    #------------------- node.pa ----------------------#
    data(gj_node_pres_abs, envir = environment())
    m <- match(vnames, names(gj_node_pres_abs)[-1]) + 1
    node.pa <- gj_node_pres_abs[,c(1,m)]
  }
  

  if(method == "gj_synoptic_2023"){
    #------------------- coords ----------------------#  
    coords <- paste("No coords available for", method)
    #------------------- lengths ----------------------#
    arc.length <- paste("No arc lengths available for", method)   
    #------------------- node.pa ----------------------#
    node.pa <- paste("No node p.a data available for", method)   
  }
  
  
  if(method == "jd_piezo_full"){
    #------------------- coords ----------------------#  
    coords <- AIMS.node.coords[AIMS.node.coords$site == "JD" & AIMS.node.coords$New_in_2023 == FALSE,]
    m <- match(vnames, coords$Object.ID)  
    coords <- coords[m,]
    #------------------- lengths ----------------------#
    data(jd_lengths_2023, envir = environment())
    m <- match(enames, jd_lengths_2023[,1])
    arc.length <- jd_lengths_2023[m,]     
    #------------------- node.pa ----------------------#
    data(jd_node_pres_abs, envir = environment())
    m <- match(vnames, names(jd_node_pres_abs)[-1]) + 1
    node.pa <- jd_node_pres_abs[,c(1,m)]
  } 
  
  if(method == "jd_piezo_full_2023"){
    #------------------- coords ----------------------#  
    coords <- AIMS.node.coords[AIMS.node.coords$site == "JD",]
    m <- match(vnames, coords$Object.ID)  
    coords <- coords[m,]
    #------------------- lengths ----------------------#
    data(jd_lengths_2023, envir = environment())
    m <- match(enames, jd_lengths_2023[,1])
    arc.length <- jd_lengths_2023[m,]   
    #------------------- node.pa ----------------------#
    data(jd_node_pres_abs, envir = environment())
    m <- match(vnames, names(jd_node_pres_abs)[-1]) + 1
    node.pa <- jd_node_pres_abs[,c(1,m)]
  }
  
  if(method == "jd_full"){
    #------------------- coords ----------------------#  
    coords <- AIMS.node.coords[AIMS.node.coords$site == "JD" & AIMS.node.coords$STIC_inferred_PA,]
    m <- match(vnames, coords$Object.ID)  
    coords <- coords[m,]
    #------------------- lengths ----------------------#
    data(jd_lengths, envir = environment())
    arc.length <- jd_lengths     
    #------------------- node.pa ----------------------#
    data(jd_node_pres_abs, envir = environment())
    m <- match(vnames, names(jd_node_pres_abs)[-1]) + 1
    node.pa <- jd_node_pres_abs[,c(1,m)]
  }
  
  
  if(method == "konza_full"){
    #------------------- coords ----------------------#  
    data(kon_coords, envir = environment())
    coords <- kon_coords
    #------------------- lengths ----------------------#
    data(kon_lengths, envir = environment())
    arc.length <- kon_lengths     
    #------------------- node.pa ----------------------#
    node.pa <- paste("No node p.a data available for", method)   
  } 
  
  
  if(method == "KD0521" | method == "KD0528"| method == "KD0604"){
    #------------------- coords ----------------------#  
    coords <- paste("No coords available for", method)
    #------------------- lengths ----------------------#
    arc.length <- paste("No arc lengths available for", method)   
    #------------------- node.pa ----------------------#
    node.pa <- paste("No node p.a data available for", method)   
  }
  
  
  if(method == "pr_full"){
    #------------------- coords ----------------------#  
    coords <- AIMS.node.coords[AIMS.node.coords$site == "PR",]
    #------------------- lengths ----------------------#
    arc.length <- paste("No arc lengths available for", method)   
    #------------------- node.pa ----------------------#
    node.pa <- paste("No node p.a data available for", method)   
  }
  
  if(method == "td_full"){
    #------------------- coords ----------------------#  
    coords <- AIMS.node.coords[AIMS.node.coords$site == "TD",]
    #------------------- lengths ----------------------#
    arc.length <- paste("No arc lengths available for", method)   
    #------------------- node.pa ----------------------#
    node.pa <- paste("No node p.a data available for", method)   
  }
  
  
  if(method == "wh_full"){
    #------------------- coords ----------------------#  
    coords <- AIMS.node.coords[AIMS.node.coords$site == "WH",]
    #------------------- lengths ----------------------#
    arc.length <- paste("No arc lengths available for", method)   
    #------------------- node.pa ----------------------#
    node.pa <- paste("No node p.a data available for", method)   
  }
  
  if(supress.message == FALSE){message(paste("For the graph '", method, "', attempted to create spatial coordinate,\n arc length, and node presence/absence dataframes as list items:\n $coords, $arc.length, and $node.pa, respectively.", sep = ""))}
  
  out <- list()
  out$graph <- graphL
  out$coords <- coords
  out$arc.length <- arc.length
  out$node.pa <- node.pa
  invisible(out)
}

 