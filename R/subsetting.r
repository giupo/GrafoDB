
setMethod(
  "[",
  c("GrafoDB", "character", "missing", "ANY"),
  function(x, i, j, ..., drop = TRUE) {
    if(length(i) == 0) {
      return(rdataset::Dataset())
    }
    raw <- x[[i]]
    if(length(i)==1) {
      ret <- list()
      ret[[i]] <- raw
    } else {
      ret <- raw
    }
    rdataset::as.dataset(ret)
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
#' @include functions.r core.r checkDAG.r find_deps.r

.subsetting <- function(x, i, value) {
  nameObject <- deparse(substitute(x))
  network <- x@network
  all_names <- igraph::V(network)$name
  name <- i
  
  already_in_dag <- all(name %in% all_names)  
  network <- if(!already_in_dag) {
    toBeAdded <- setdiff(name, all_names)
    network + igraph::vertex(toBeAdded)
  } else {
      if(!rdataset::is.dataset(value) && length(igraph::E(network)) > 0) {
        if(packageVersion("igraph") >= '1.1.0') {
            network - igraph::E(network)[.to(name)]  # nocov
        } else {
            network - igraph::E(network)[to(name)]   # nocov
        }
    } else {
      network
    }
  }

  if (is.function(value)) {
    ## assert all dependencies
    
    declutted <- .declutter_function(value)
    .dependencies <- names(as.list(formals(value)))
    dependencies <- c()
    if(length(.dependencies) > 0) {
      for(dep in .dependencies) {
        if(!grepl(dep, declutted)) {
          warning(dep, " not in formula: ", declutted)
        } else {
          dependencies <- c(dependencies, dep)
        }
      }
    } else  {
      ## e' una serie elementare
    } 
    
    if (!all(dependencies %in% all_names)) {
      miss <- setdiff(dependencies, all_names)
      stop("Missing dependencies ", paste(miss, collapse=", "))
    }
    
    for(dep in dependencies) {
      network <- network + igraph::edge(dep, name)
    }
    ## rimuovo i tempedges perche' li ho appena inseriti
    if(name %in% hash::keys(x@edges)) {
      suppressWarnings(hash::del(name, x@edges))
    }
    
    checkDAG(network)

    x@functions[[name]] <- declutted
    x@network <- network
    x <- .evaluate(x, name)
  } else if (is.character(value) && grepl("^function", value)) {
    return(.subsetting(x, i, eval(parse(text=value))))
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
    if(rdataset::is.dataset(value) || is.list(value)) {
      value <- rdataset::as.dataset(value)
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
  invisible(x)
}

setMethod(
  "[<-",
  signature("GrafoDB", "character", "missing", "ANY"),
  function(x, i, j, ..., value) {
    nameObject <- deparse(substitute(x))
    x <- .subsetting(x, i, value)
    #TODO(giupo): this causes a warning, fix me.
    assign(nameObject, x, envir=parent.frame())
    invisible(x)
  })

setMethod(
  "[[<-",
  signature("GrafoDB", "character", "missing", "ANY"),
  function(x, i, j, ..., value) {
    nameObject <- deparse(substitute(x))
    x <- .subsetting(x, i, value)
    #TODO(giupo): this causes a warning, fix me.
    assign(nameObject, x, envir=parent.frame())
    invisible(x)
  })
