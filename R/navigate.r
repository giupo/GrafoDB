#' @include functions.r

.navigate <- function(object, nodes = NULL, order = 1L, mode = "out") {
  network <- object@network
  order <- as.integer(order)
  mmode <- as.character(mode)

  x <- if (!is.null(nodes)) {
    nodes <- as.character(nodes)
    unlist(igraph::neighborhood(
      graph = network,
      order = order,
      nodes = nodes,
      mode = mmode))
  } else {
    return(names(object))
  }

  g1 <- igraph::induced.subgraph(network, x)
  ret <- igraph::V(g1)$name
  ret <- ret[!ret %in% nodes]
  rutils::ifelse(length(ret) == 0, invisible(NULL), ret)
}
