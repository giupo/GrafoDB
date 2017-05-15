#' @importFrom igraph V E is.dag vertex topological.sort edge

checkDAG <- function(network) {
  if (!is.dag(network)) {
    wrongsort <- try(topological.sort(network), silent=TRUE)
    network_seq <- V(network)
    cycles_seq <- network_seq[setdiff(network_seq, network_seq[wrongsort])]
    cycles_vertex <- cycles_seq$name
    stop("Cycles found: ", paste(unlist(cycles_vertex), collapse=", "))
  }
}
