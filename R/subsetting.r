# Questo file contiene tutte le funzioni per effettuare operazioni di subsetting
# sul 'GrafoDB

#' @include functions.r
NULL

setMethod(
  "[",
  c("GrafoDB", "character", "missing", "ANY"),
  function(x, i, j, ..., drop = TRUE) {
    raw <- x[[i]]
    if(length(i)==1) {
      ret <- list()
      ret[[i]] <- raw
    } else {
      ret <- raw
    }
    as.dataset(ret)
  })


setMethod(
  "[[",
  c("GrafoDB", "character"),
  .getdata)

#' implementazione per GrafoDB del subsetting '[<-'
#'
#' @name .subsetting
#' @usage .subsetting(x, i, j, ..., value)
#' @param x istanza di `GrafoDB`
#' @param i un character
#' @param j un'altro character o `missing`
#' @param ... ellipsis dio solo sa a cosa servono
#' @param value valore settato
#' @note funzione interna
#' @rdname subsetting_internal
#' @include functions.r core.r

.subsetting <- function(x, i, value) {
  network <- x@network
  all_names <- V(network)$name
  name <- i
  already_in_dag <- name %in% all_names
  network <- if(!already_in_dag) {
    network + vertex(name)
  } else {
    network - E(network)[to(name)]
  }
  if (is.function(value)) {
    ## assert all dependencies
    dependencies <- names(as.list(formals(value)))
    if (!all(dependencies %in% all_names)) {
      miss <- setdiff(dependencies, all_names)
      stop("Missing dependencies ", paste(miss, collapse=", "))
    }
    for(dep in dependencies) {
      network <- network + edge(dep, name)
    }

    if (!is.dag(network)) {
      wrongsort <- try(topological.sort(network), silent=TRUE)
      network_seq <- V(network)
      cycles_seq <- network_seq[setdiff(
        network_seq, network_seq[wrongsort])]
      cycles_vertex <- cycles_seq$name
      stop("Cycles found: ", paste(unlist(cycles_vertex), collapse=", "))
    }
    x@functions[[name]] <- .declutter_function(value)
    x@network <- network
    x <- .evaluate(x, name)
  } else {
    x@network <- network
    x@data[[name]] <- value
    subgraph <- describe(x, name, mode="out")
    if(length(subgraph)) {    
      x <- evaluate(x, name)
    }
  }
  nameObject <- deparse(substitute(x))
  assign(nameObject, x, envir=parent.frame())
  invisible(x)
}


setMethod(
  "[<-",
  signature("GrafoDB", "character", "missing", "ANY"),
  function(x, i, j, ..., value) {
    x <- .subsetting(x, i, value)
    nameObject <- deparse(substitute(x))
    assign(nameObject, x, envir=parent.frame())
    invisible(x)
  })

setMethod(
  "[[<-",
  signature("GrafoDB", "character", "missing", "ANY"),
  function(x, i, j, ..., value) {
    x <- .subsetting(x, i, value)
    nameObject <- deparse(substitute(x))
    assign(nameObject, x, envir=parent.frame())
    invisible(x)
  })
