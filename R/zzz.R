.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to streamDAG!\n\nFor more information on using the package type:\n\nvignette(\"streamDAG\")", appendLF = FALSE)
}

#A reminder: ancillary data entries corresponding to stream DAG arcs\nand nodes (e.g., weights and other graph modifiers) should have\nthe same ordering as the arcs or nodes in the stream DAG igraph\nobject that the ancillary data correspond to. The order of nodes\nand arcs in an igraph object, G, can be checked using V(G) and E(G),\nrespectively. 