size.intact.to.arc <- function(G, arc.node = "in"){
  avn <- attributes(E(G))$vnames
  df <- data.frame(do.call("rbind",strsplit(avn,"|", fixed =  T)))
  names(df) <- c("in","out")
  crit <- df[,names(df) == arc.node]
  out <- 1:length(avn)
  for(i in 1:length(avn)){
   out[i] <- size.intact.to.node(G, node = crit[i]) 
  }
  nnames <- gsub(x = avn,"|"," --> ", fixed = T)
  names(out) <- nnames
out  
}