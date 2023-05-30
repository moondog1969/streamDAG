ICSL <- function(G, coords = NULL, names = NULL, lengths = NULL, dist.matrix = NULL, show.dist = FALSE){
  if(is.null(coords) & is.null(length) & length(E(G)$weight) == 0) stop("One of coords or lengths or lenghts using E(G)$weight must be specfied")
  if(length(attributes(E(G))$vnames) == 0) {icsl <- NA; d <- NA} else{
  a <- attributes(E(G))$vnames
  bounds <- matrix(ncol = 1, data = unlist(strsplit(a, "\\|")), byrow = TRUE)  
  
  if(is.null(lengths) & is.null(coords) & is.null(dist.matrix)){
    d <- distances(G) 
    d2 <- ifelse(d == Inf, NA, d)
    d2 <- as.dist(d2)
    icsl <- mean(d2, na.rm = TRUE)
  }
  
  if(is.null(lengths) & is.null(dist.matrix) & !is.null(coords)){
    row.names(coords) <- names
    dists <- dist(coords)
    m <- match(row.names(coords), unique(bounds))
    rn <- row.names(coords)[!is.na(m)]
    w <- which(row.names(coords) %in% rn)
    d <- dist(coords[w,])
    icsl <- mean(d)
  }
  if(!is.null(lengths) & is.null(dist.matrix) & is.null(coords))  {
  E(G)$weight <- lengths
  d <- distances(G) 
  d2 <- ifelse(d == Inf, NA, d)
  d2 <- as.dist(d2)
  icsl <- mean(d2, na.rm = TRUE)
  }
  if(!is.null(dist.matrix))  {
  if (!inherits(dist.matrix, "dist")) stop("dist.matrix must be of class dist")
  d <- dist.matrix
  cn <- colnames(as.matrix(d))
  w <- which(cn %in% attributes(V(G))$name)
  d <- as.matrix(d)[w,][,w]
  d2 <- ifelse(d == Inf, NA, d)
  d2 <- as.dist(d2)
  icsl <- mean(d2, na.rm = TRUE)
  }
  }
  if(show.dist) list(iscl = icsl, distance.matrix = d) else icsl  
}

