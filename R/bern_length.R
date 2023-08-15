bern.length <- function(lengths, pa, mode = "local"){
  if(mode == "local") out <- lengths * pa
  if(mode == "global") out <- sum(lengths * pa)
 out
}