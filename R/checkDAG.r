#' @importFrom igraph V E is.dag vertex topological.sort edge

checkDAG <- function(network) {
  if (!igraph::is.dag(network)) {
    wrongsort <- try(igraph::topological.sort(network), silent=TRUE)
    network_seq <- igraph::V(network)
    cycles_seq <- network_seq[setdiff(network_seq, network_seq[wrongsort])]
    cycles_vertex <- cycles_seq$name
    stop("Cycles found: ", paste(unlist(cycles_vertex), collapse=", "))
  }
}
