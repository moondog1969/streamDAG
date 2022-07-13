ICSL <- function(G, coords = NULL, names = NULL, lengths = NULL, show.dist = FALSE){
  if(is.null(coords) & is.null(length)) stop("One of coords or arc lengths must be specfied")
  
  a <- attributes(A(G))$vnames
  bounds <- matrix(ncol = 1, data = unlist(strsplit(a, "\\|")), byrow = TRUE)  
  
  if(is.null(lengths)){
    row.names(coords) <- names
    dists <- dist(coords)
    m <- match(row.names(coords), unique(bounds))
    rn <- row.names(coords)[!is.na(m)]
    w <- which(row.names(coords) %in% rn)
    d <- dist(coords[w,])
    icsl<- mean(d)
  }
  if(!is.null(lengths))  {
  E(G)$weight <- lengths
  d <- distances(G) 
  d2 <- ifelse(d == Inf, NA, d)
  icsl <- mean(d2[upper.tri(d2)], na.rm = TRUE)
  }
if(show.dist) list(iscl = icsl, distance.matrix = d) else icsl  
}

