#' @include functions.r
#' @importFrom igraph topological.sort neighborhood induced.subgraph V

.navigate <- function(object, nodes=NULL, order=1L, mode='out', plot=FALSE) {    
  if(!is.null(nodes))  {
    nodes <- as.character(nodes)
  }
  order <- as.integer(order)
  mmode <- as.character(mode)
  plot <- as.logical(plot)
  
  network <- object@network
  x <- if (!is.null(nodes)) {
    unlist(
      neighborhood(
        graph=network, order=order, nodes=nodes, mode=mmode))
  } else {
    nodes <- topological.sort(network)[1]
    unlist(
      neighborhood(
        graph=network, order=order, nodes=nodes, mode='out'))
  }
  
  g1 <- induced.subgraph(network, x)
  # V(g1)$label <- V(g1)$name
  ret <- V(g1)$name
  ret <- ret[ !ret %in% nodes ]
  if (length(ret)== 0) {
    invisible(NULL)
  } else {
    ret
  }
}
