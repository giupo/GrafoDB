#' @include expr.r

.evaluateSingle1 <- function(name, graph) {
  tsformula <- .expr(graph, name, echo=FALSE)
  nomi_padri <- upgrf(graph, name, livello=1)
  if(length(nomi_padri) == 0 && is.null(tsformula)) {
    return(graph[[name]])
  }

  if (length(nomi_padri) > 1) {
    padri <- graph[[nomi_padri]]
  } else if (length(nomi_padri) == 1) {
    padri <- list()
    padri[[nomi_padri]] <- graph[[nomi_padri]]
  } else {
    padri <- list()
  }

  if (isElementary(graph, name)) {
    ## Se e' elementare cmq la carico (e' nato prima l'uovo o la gallina?)
    ## e la metto nei nomi_padri
    nomi_padri <- name
    tt <- tryCatch({
      ## sopprimo il warning perche' e' normale che non ci
      ## sia la serie elementare se la valuto per la prima volta
      suppressWarnings(graph[[name]])
    }, error = function(cond) {
      NA
    })
    padri[[name]] <- tt
    assign(name, tt)
  }

  cmd <- .clutter_with_params_and_return(tsformula, name, nomi_padri)
  tryCatch({
    env <- as.environment(padri)
    # defines the proxy
    eval(parse(text = cmd))
    # executes the call
    env$proxy <- proxy
    env[[name]] <- do.call("proxy", padri, envir = env)
    # lookup the name in the env
    ret <- get(name, envir = env)
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
#' @name .evaluate
#' @usage .evaluate(object)
#' @usage .evaluate(object, v_start)
#' @return il grafo con i dati correttamente valutato
#' @rdname evaluate-internal


.evaluate00 <- function(object, v_start = NULL, ...) {
  names_object <- names(object)

  if (!all(v_start %in% names_object)) {
    not_in_graph <- setdiff(v_start, names_object)
    stop("Not in graph: ", paste(not_in_graph, collapse=", "))
  }

  successors <- downgrf(object, v_start)
  ordered_successors <- successors[order(match(successors, names_object))]


  if (interactive())
    pb <- progress::progress_bar$new(
      total = length(ordered_successors),
      format = ":what [:bar] :current/:total :percent eta: :eta")

  data <- object@data

  for (name in ordered_successors) {
    if (interactive()) pb$tick(tokens = list(what = name))
    data[[name]] <- .evaluateSingle(name, object)
  }

  object@data <- data
  object@touched <- sort(unique(c(object@touched, hash::keys(data))))
  invisible(object)
}


.evaluate <- function(object, v_start = NULL, ...) {
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

  if (!all(v_start %in% all_names)) {
    not_in_graph <- setdiff(v_start, all_names)
    stop("Not in graph: ", paste(not_in_graph, collapse=", "))
  }

  ## preload primitive
  primitive <- listPrimitives(object)

  if (!is.null(v_start)) {
    v_start <- as.character(v_start)
    network <- igraph::induced.subgraph(
      network,
      igraph::V(network)[
        unlist(igraph::neighborhood(
          network, order = .Machine$integer.max,
          nodes = v_start, mode = "out")
        )])
  }

  ## se il network e' vuoto dopo l'eliminazione delle sorgenti,
  ## ritorno senza fare nulla
  total <- length(igraph::V(network))
  if (total == 0) {
    return(invisible(object))
  }

  i <- 0
  is_interactive <- interactive()
  if (is_interactive) { # nocov start
    pb <- rprogressbar::ProgressBar(min = 0, max = total)
    rprogressbar::updateProgressBar(pb, i, "Starting...")
  } # nocov end

  if (is_interactive) rprogressbar::updateProgressBar(pb, i, "Starting...") # nocov

  sources_id <- igraph::V(network)[igraph::degree(network, mode = "in") == 0]

  proxy <- function(name, object) {
    serie <- .evaluateSingle(name, object)
    ret <- list()
    ret[[name]] <- serie
    ret
  }

  while (length(sources_id)) {
    sources <- igraph::V(network)[sources_id]$name
    sprimitive <- intersect(sources, primitive)
    i <- i + length(sprimitive)
    prim_non_in_data <- setdiff(sprimitive, hash::keys(data))
    prim_non_in_data <- setdiff(prim_non_in_data, hash::keys(functions))
    if (length(prim_non_in_data)) {
      datip <- object[prim_non_in_data]
      for(n in names(datip)) {
        data[n] <- datip[[n]]
      }
    }

    sources <- setdiff(sources, sprimitive)

    if(length(sources)) {
      evaluated <- foreach::`%dopar%`(foreach::foreach(name = sources, .combine = c), {
        proxy(name, object)
      })

      i <- i + length(sources)
      if (is_interactive) {
        rprogressbar::updateProgressBar(pb, i, tail(names(evaluated), n=1)) # nocov
      }

      if (length(evaluated) != length(sources)) {
        stop("evaluated and sources are different in length")
      }

      if (length(evaluated) == 1) {
        data[[names(evaluated)]] <- evaluated[[names(evaluated)]]
      } else {
        data[names(evaluated)] <- evaluated
      }
    }

    network <- igraph::delete.vertices(network, sources_id)
    sources_id <- igraph::V(network)[igraph::degree(network, mode = "in") == 0]
  }

  if (is_interactive) rprogressbar::kill(pb)

  object@data <- data
  object@touched <- sort(unique(c(object@touched, hash::keys(data))))
  object
}

#' patch to evaluate
#'
#' @name evaluate_plain
#' @export

evaluate_plain <- function(x, i = names(x)) {
  if(interactive())
    pb <- progress::progress_bar$new(
      total = length(i), 
      format = ":what [:bar] :current/:total :percent eta: :eta")

  data <- g@data

  for(name in i) {
    if(interactive()) pb$tick(tokens = list(what=name))
    data[[name]] <- .evaluateSingle(name, g)
  }

  g@data <- data
  g@touched <- sort(unique(c(g@touched, hash::keys(data))))
  invisible(g)
}
