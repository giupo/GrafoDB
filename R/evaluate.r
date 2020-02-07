#' @include expr.r

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
    env <- as.environment(padri)
    # defines the proxy
    eval(parse(text=cmd))
    # executes the call
    env$proxy <- proxy
    env[[name]] <- do.call("proxy", padri, envir=env)
    # lookup the name in the env
    ret <- get(name, envir=env)
    if (is.call(ret) || is.function(ret)) {
      stop("evaluated as a function: check your function definition")
    }
    ret

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
#' @importFrom rprogressbar ProgressBar updateProgressBar kill
#' @importFrom foreach %dopar%
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
    network <- igraph::induced.subgraph(
      network,
      V(network)[
        unlist(igraph::neighborhood(
          network, order=.Machine$integer.max,
          nodes=v_start, mode="out")
        )])
  }

  ## se il network e' vuoto dopo l'eliminazione delle sorgenti,
  ## ritorno senza fare nulla
  total <- length(igraph::V(network))
  if(total == 0) {
    return(invisible(object))
  }

  i <- 0
  is.interactive <- interactive()
  if(is.interactive) { # nocov start
    pb <- ProgressBar(min=0, max=total)
    updateProgressBar(pb, i, "Starting...")
  } # nocov end

  if(is.interactive) updateProgressBar(pb, i, "Starting...") # nocov

  sources_id <- igraph::V(network)[igraph::degree(network, mode="in") == 0]

  proxy <- function(name, object) {
    serie <- .evaluateSingle(name, object)
    ret <- list()
    ret[[name]] <- serie
    ret
  }

  while(length(sources_id)) {
    sources <- V(network)[sources_id]$name
    sprimitive <- intersect(sources, primitive)
    i <- i + length(sprimitive)
    prim_non_in_data <- setdiff(sprimitive, hash::keys(data))
    prim_non_in_data <- setdiff(prim_non_in_data, hash::keys(functions))
    if(length(prim_non_in_data)) {
      datip <- object[prim_non_in_data]
      for(n in names(datip)) {
        data[n] <- datip[[n]]
      }
    }

    sources <- setdiff(sources, sprimitive)

    if(length(sources)) {
      evaluated <- foreach::foreach(name=sources, .combine=c) %dopar% {
        proxy(name, object)
      }

      i <- i + length(sources)
      if(is.interactive) {
        updateProgressBar(pb, i, tail(names(evaluated), n=1)) # nocov
      }

      # ignore why %dopar% is loosing data.
      # the following is a patch in the meantime
      motherfucker <- setdiff(sources, names(evaluated))

      while(length(motherfucker)) {
        evaluated0 <- foreach::foreach(name=motherfucker, .combine=c) %dopar% {
          proxy(name, object)
        }
        evaluated <- c(evaluated, evaluated0)
        motherfucker <- setdiff(motherfucker, names(evaluated0))
      }
      # R you are a plain joke. <endofpatch>

      if(length(evaluated) != length(sources)) {
        stop("evaluated and sources are different in length")
      }

      if(length(evaluated) == 1) {
        data[[names(evaluated)]] <- evaluated[[names(evaluated)]]
      } else {
        data[names(evaluated)] <- evaluated
      }
    }

    network <- igraph::delete.vertices(network, sources_id)
    sources_id <- igraph::V(network)[igraph::degree(network, mode="in") == 0]
  }

  if(is.interactive) kill(pb)

  object@data <- data
  object@touched <- sort(unique(c(object@touched, hash::keys(data))))
  object
}

#' patch to evaluate
#' 
#' @name evaluate_plain
#' @export 

evaluate_plain <- function(x, i=names(x)) {
  pb <- progress::progress_bar$new(
    total = length(i), 
    format = ":what [:bar] :current/:total :percent eta: :eta",
  )

  for(name in i) {
    pb$tick(tokens=list(what=name))
    g@data[[name]] <- .evaluateSingle(name, g)
  }

  g@touched <- sort(unique(c(g@touched, hash::keys(data))))
  invisible(g)
}