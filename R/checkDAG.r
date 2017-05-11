#' @importFrom igraph V E is.dag vertex topological.sort edge

checkDAG <- function(network) {
  if (!is.dag(network)) {
    wrongsort <- try(topological.sort(network), silent=TRUE)
    network <- seq <- V(network)
    cycles <- seq <- network_seq[setdiff(
      network <- seq, network_seq[wrongsort])]
    cycles <- vertex <- cycles_seq$name
    stop("Cycles found: ", paste(unlist(cycles_vertex), collapse=", "))
  }
}
