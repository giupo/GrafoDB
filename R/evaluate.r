
#' @include expr.r

.evaluateSingle1 <- function(name, graph) {
  tsformula <- .expr(graph, name, echo=FALSE)
  nomi_padri <- upgrf(graph, name, livello=1)
  if(length(nomi_padri) == 0 && is.null(tsformula)) {
    return(graph[[name]])
  }

  ## this is a patch due limitations of R language and
  ## deeeeep limitations of my patience
  if(length(nomi_padri) > 100) {
    padri <- list()
    sliced <- slice(nomi_padri, n=100)
    foreach(sliced_names = sliced) %do% {
      padri[sliced_names] <- graph[[sliced_names]]
    }
    names(padri) <- nomi_padri
    padri
  } else if ( length(nomi_padri) > 1 ) {
    padri <- graph[[nomi_padri]]
  } else if ( length(nomi_padri) == 1 ) {
    padri <- list()
    padri[[nomi_padri]] <- graph[[nomi_padri]]
  } else {
    padri <- list()
  }
  
  if(isElementary(graph, name)) {
    ## Se e' elementare cmq la carico (e' nato prima l'uovo o la gallina?)
    ## e la metto nei nomi_padri
    padri[[name]] <- graph[[name]]
    assign(name, padri[[name]])
    nomi_padri <- name
  }
  
  ## cmd <- .clutter_function(tsformula, name)
  cmd <- .clutter_with_params_and_return(tsformula, name, nomi_padri)
  tryCatch({    
    eval(parse(text=cmd))
    do.call(proxy, padri)    
  }, error = function(err) {
    stop(name, ": ", err)
  })           
}

.evaluateSingle3 <- function(name, graph) {
  tsformula <- .expr(graph, name, echo=FALSE)
  nomi_padri <- upgrf(graph, name, livello=1)
  if(length(nomi_padri) == 0) {
    return(graph[[name]])
  }
  if(length(nomi_padri) > 100) {
    padri <- list()
    sliced <- slice(nomi_padri, n=100)
    foreach(sliced_names = sliced) %do% {
      padri[sliced_names] <- graph[[sliced_names]]
    }
    names(padri) <- nomi_padri
    padri
  } else {
    padri <- graph[[nomi_padri]]
  }
  
  if(length(nomi_padri) == 1) {
    ## boxing
    ppp <- list()
    ppp[[nomi_padri]] <- padri
    padri <- ppp
  }

  attach(padri)
  on.exit(detach(padri))
  cmd <- .clutter_function(tsformula, name)
  tryCatch({
    eval(parse(text=cmd))
    proxy()
  }, error = function(err) {
    stop(name, ": ", err)
  })           
}

.evaluateSingle2 <- function(name, graph) {
  tsformula <- .expr(graph, name, echo=FALSE)
  nomi_padri <- upgrf(graph, name, livello=1)
  if(length(nomi_padri) == 0) {
    return(graph[[name]])
  }
  if(length(nomi_padri) > 100) {
    padri <- list()
    sliced <- slice(nomi_padri, n=100)
    foreach(sliced_names = sliced) %do% {
      padri[sliced_names] <- graph[[sliced_names]]
    }
    names(padri) <- nomi_padri
    padri
  } else {
    padri <- graph[[nomi_padri]]
  }
  
  if(length(nomi_padri) == 1) {
    ## boxing
    ppp <- list()
    ppp[[nomi_padri]] <- padri
    padri <- ppp
  }
  env <- as.environment(padri)
  env$graph <- graph
  cmd <- .clutter_function(tsformula, name)
  tryCatch({
    eval(parse(text=cmd))
    attach(env)
    do.call(proxy, list())
  }, error = function(err) {
    stop(name, ": ", err)
  })           
}

#' Valuta un singolo oggetto del grafo identificato da `name`
#'
#' @name .evaluateSingle
#' @aliases evaluateSingle
#' @usage .evaluateSingle(name, object)
#' @usage evaluateSingle(name, object)
#' @param name `character` nome della serie
#' @param graph istanza di `GrafoDB`
#' @return la serie storica calcolata.
#' @export
#' @rdname evaluateSingle-internal
#' @note la scelta di evaluateSingle ricade su .evaluateSingle1, le altre due
#'       implementazioni NON SUPPORTANO LE SERIE ELEMENTARI

.evaluateSingle <- .evaluateSingle1

#' Implementazione del generic `evaluate` definito nel package `grafo`
#' per la classe `GrafoDB`
#'
#' Questa funzione valuta i nodi del grafo in parallelo
#'
#' @name .evaluate
#' @usage .evaluate(object)
#' @usage .evaluate(object, v_start)
#' @return il grafo con i dati correttamente valutato
#' @importFrom igraph V induced.subgraph neighborhood delete.vertices degree
#' @importFrom rprogressbar ProgressBar updateProgressBar kill
#' @importFrom foreach foreach %do% %dopar%
#' @rdname evaluate-internal

.evaluate <- function(object, v_start=NULL, deep=T, ...) {
  params <- list(...)

  debug <- if("debug" %in% names(params)) {
    as.logical(params[["debug"]])
  } else {
    FALSE
  }
  
  tag <- object@tag
  data <- object@data
  network <- object@network
  all_names <- names(object)
  
  if(!all(v_start %in% all_names)) {
    not.in.graph <- setdiff(v_start, all_names)
    stop("Non sono serie del grafo: ", paste(not.in.graph, collapse=", "))
  }
  
  if(!is.null(v_start)) {
    ## le voglio valutare tutte
    ## if(!tag %in% c(PRELOAD_TAG, "biss", "pne", "dbcong")) {
    #sources_id <- V(network)[degree(network, mode="in") == 0]
    #nomi_sources <- V(network)[sources_id]$name
    #sources <- getdb(nomi_sources, tag)
    #if(length(sources) > 0) {
    #  if(is.bimets(sources)) {
    #    data[preload_V_start] <- sources
    #  } else {
    #    data[names(sources)] <- sources
    #  }
    #  ## fonti gia' valutate, le tolgo
    #  network <- delete.vertices(network, sources_id)
    #}
  #} else {
    v_start <- as.character(v_start)
    network <- induced.subgraph(
      network,
      V(network)[unlist(
        neighborhood(network, order=.Machine$integer.max,
                     nodes=v_start, mode="out"))])
  }
  
  ## se il network e' vuoto dopo l'eliminazione delle sorgenti,
  ## ritorno senza fare nulla
  if(!length(V(network))) {
    return(invisible(object))
  }
  
  deep <- as.logical(deep)
  
  total <- length(V(network))
  i <- 0
  is.interactive <- interactive()
  if(is.interactive) {
    pb <- ProgressBar(min=0, max=total)
    updateProgressBar(pb, i, "Try Cluster...")
  }  
  
  cl <- initCluster()
  is.multi.process <- !is.null(cl) && !debug 
  
  if(is.multi.process) {
    if(wasWorking()) {
      stopCluster(cl)
      cl <- initCluster()
    } else {
      clusterStartWorking()
    }
    clusterExport(
      cl, ".evaluateSingle",
      envir=environment())
    if(is.interactive) updateProgressBar(pb, i, "Cluster OK")
  } else {
    if(is.interactive) updateProgressBar(pb, i, "No Cluster")
  }

  if(is.interactive) updateProgressBar(pb, i, "Starting...")
  
  sources_id <- V(network)[degree(network, mode="in") == 0]
  
  proxy <- function(name, object) {
    serie <- .evaluateSingle(name, object)
    list(serie)
  }
  
  while(length(sources_id)) {
    sources <- V(network)[sources_id]$name
    
    if(!is.multi.process) {
      evaluated <- foreach(name=sources, .combine=c) %do% {
        i <- i + 1
        if(is.interactive) {
          updateProgressBar(pb, i, name)
        }
        proxy(name, object)
      }
    } else {
      evaluated <- foreach(name=sources, .combine=c) %dopar% {
        proxy(name, object)
      }
      i <- i + length(sources)
      if(is.interactive) {
        updateProgressBar(pb, i, last(sources))
      }
    }
    
    names(evaluated) <- sources
    
    ## evaluated <- Filter(function(x) length(x) != 0, evaluated)
    
    if(length(evaluated) == 1) {
      data[[sources]] <- evaluated[[sources]]
    } else {
      data[sources] <- evaluated
    }
    
    network <- delete.vertices(network, sources_id)
    sources_id <- V(network)[degree(network, mode="in") == 0]
  }

  doneWithCluster()
  if(is.interactive) kill(pb)
  object@data <- data
  object
}
