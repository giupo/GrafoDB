#' assert that a network is a DAG
#'
#' @name assert_dag
#' @param network igraph instance
#' @note if dag has cicles, raises an Exception
#'
#' TODO: dovrebbe tornare una lista di cicli se trovati. con la nuova
#' versione di igraph non funziona.

assert_dag <- function(network) {
  if (!igraph::is.dag(network)) {
    wrongsort <- try(igraph::topological.sort(network), silent=TRUE)
    network_seq <- igraph::V(network)
    cycles_seq <- network_seq[setdiff(network_seq, network_seq[wrongsort])]
    cycles_vertex <- cycles_seq$name
    stop("Cycles found: ", paste(unlist(cycles_vertex), collapse = ", "))
  }
}
