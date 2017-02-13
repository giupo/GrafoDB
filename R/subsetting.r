#' @importFrom rdataset as.dataset

setMethod(
  "[",
  c("GrafoDB", "character", "missing", "ANY"),
  function(x, i, j, ..., drop = TRUE) {
    if(length(i) == 0) {
      return(Dataset())
    }
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
  c("GrafoDB", "character", "missing"),
  function(x, i, j, ...) {
    .getdata(x,i)
  })

#' implementazione per GrafoDB del subsetting '[<-'
#'
#' @name .subsetting
#' @usage .subsetting(x, i, j, ..., value)
#' @param x istanza di `GrafoDB`
#' @param i un character
#' @param value valore settato
#' @note funzione interna
#' @rdname subsetting_internal
#' @include functions.r core.r
#' @importFrom igraph V E is.dag vertex topological.sort edge
#' @importFrom rdataset is.dataset
#' @importFrom hash del

.subsetting <- function(x, i, value) {
  nameObject <- deparse(substitute(x))
  network <- x@network
  all_names <- V(network)$name
  name <- i
  
  already_in_dag <- all(name %in% all_names)  
  network <- if(!already_in_dag) {
    toBeAdded <- setdiff(name, all_names)
    network + vertex(toBeAdded)
  } else {
      if(!is.dataset(value) && length(E(network)) > 0) {
        if(packageVersion("igraph") >= '1.1.0') {
            network - E(network)[.to(name)]  # nocov
        } else {
            network - E(network)[to(name)]   # nocov
        }
    } else {
      network
    }
  }

  if (is.function(value)) {
    ## assert all dependencies
    dependencies <- names(as.list(formals(value)))

    if(length(dependencies) == 0) {
      ## e' una serie elementare
    }

    if (!all(dependencies %in% all_names)) {
      miss <- setdiff(dependencies, all_names)
      stop("Missing dependencies ", paste(miss, collapse=", "))
    }

    for(dep in dependencies) {
      network <- network + edge(dep, name)
    }
    ## rimuovo i tempedges perche' li ho appena inseriti
    if(name %in% keys(x@edges)) {
      suppressWarnings(del(name, x@edges))
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
    aggregate <- listAggregates(x)
    elementari <- listElementaries(x)
    tt <- intersect(name, c(aggregate, elementari))
    
    ci.sono.formule <- length(tt) != 0
    
    if(ci.sono.formule) {
      stop("Non puoi impostare una serie con formula con uno scalare: ",
           paste(tt, collapse=", "))
    }
    
    x@network <- network
    if(is.dataset(value) || is.list(value)) {
      value <- as.dataset(value)
      data <- x@data
      for(n in names(value)) {
        data[n] <- value[[n]]
      }
      x@data <- data
    } else {  
      x@data[[name]] <- value
    }
    subgraph <- navigate(x, name, mode="out")
    if(length(subgraph)) {    
      x <- evaluate(x, name)
    }
  }
  x@touched <- sort(unique(c(x@touched, name)))
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
