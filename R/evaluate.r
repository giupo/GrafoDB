#' @include expr.r
#' @importFrom rutils slice

.evaluateSingle1 <- function(name, graph) {
  tsformula <- .expr(graph, name, echo=FALSE)
  nomi_padri <- upgrf(graph, name, livello=1)
  if(length(nomi_padri) == 0 && is.null(tsformula)) {
    return(graph[[name]])
  }

  if ( length(nomi_padri) > 1 ) {
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
    nomi_padri <- name
      tt <- tryCatch({
      ## sopprimo il warning perche' e' normale che non ci sia la serie elementare
      ## se la valuto per la prima volta  
      suppressWarnings(graph[[name]])
    }, error = function(cond) {
      NA
    })
    padri[[name]] <- tt
    assign(name, tt)
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
#' @importFrom foreach foreach %dopar%
#' @importFrom hash keys
#' @rdname evaluate-internal

.evaluate <- function(object, v_start=NULL, ...) {
  params <- list(...)

  debug <- if("debug" %in% names(params)) {
    as.logical(params[["debug"]]) # nocov used only for debugging
  } else {
    FALSE
  }
  
  tag <- object@tag
  data <- object@data
  functions <- object@functions
  network <- object@network
  all_names <- names(object)
  
  if(!all(v_start %in% all_names)) {
    not.in.graph <- setdiff(v_start, all_names)
    stop("Non sono serie del grafo: ", paste(not.in.graph, collapse=", "))
  }

  ## preload primitive
  primitive <- listPrimitives(object)
  
  
  if(!is.null(v_start)) {
    v_start <- as.character(v_start)
    network <- induced.subgraph(
      network,
      V(network)[
        unlist(
          neighborhood(
            network, order=.Machine$integer.max,
            nodes=v_start, mode="out")
        )])
  }
  
  ## se il network e' vuoto dopo l'eliminazione delle sorgenti,
  ## ritorno senza fare nulla
  if(!length(V(network))) {
    return(invisible(object))
  }
  
  total <- length(V(network))
  i <- 0
  is.interactive <- interactive()
  if(is.interactive) { # nocov start
    pb <- ProgressBar(min=0, max=total)
    updateProgressBar(pb, i, "Starting...")
  } # nocov end
  
  if(is.interactive) updateProgressBar(pb, i, "Starting...") # nocov
  
  sources_id <- V(network)[degree(network, mode="in") == 0]
  
  proxy <- function(name, object) {
    serie <- .evaluateSingle(name, object)
    list(serie)
  }
  
  while(length(sources_id)) {
    sources <- V(network)[sources_id]$name
    sprimitive <- intersect(sources, primitive)
    i <- i + length(sprimitive)
    prim_non_in_data <- setdiff(sprimitive, keys(data))
    prim_non_in_data <- setdiff(prim_non_in_data, keys(functions))
    if(length(prim_non_in_data)) {
      datip <- g[prim_non_in_data]
      for(n in names(datip)) {
        data[n] <- datip[[n]]
      }
    }
    sources <- setdiff(sources, sprimitive)
    
    if(length(sources)) {
        evaluated <- foreach(name=sources, .combine=c) %dopar% {
            proxy(name, object)
        }
        i <- i + length(sources)
        if(is.interactive) {
            updateProgressBar(pb, i, tail(sources, n=1)) # nocov
        }
     
      
      names(evaluated) <- sources
    
      if(length(evaluated) == 1) {
        data[[sources]] <- evaluated[[sources]]
      } else {
        data[sources] <- evaluated
      }
    }
    network <- delete.vertices(network, sources_id)
    sources_id <- V(network)[degree(network, mode="in") == 0]
  }

  if(is.interactive) kill(pb)
  object@data <- data
  object
}
