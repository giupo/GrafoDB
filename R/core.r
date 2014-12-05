#' ricerca nei metadati del `GrafoDB`
#'
#' @name lookup
#' @usage lookup(x, key, value)
#' @param x istanza di `GrafoDB`
#' @param key `character` che specifica la chiave del metadato
#' @param value `character` che specifica il valore del metadato
#' @return un character array di nomi di serie che rispettano la clausola `key` = `value`. Se non esistono ritorna un character(0) (array vuoto)
#' @examples \dontrun{
#' g = GrafoDB(...) # istanzia il grafo
#' lookup(g, "TAVOLA_DI_OUTPUT", "BRI") # ritorna i nomi di serie che hanno TAVOLA_DI_OUTPUT=BRI
#' }
#' @rdname lookup_generic
#' @export

setGeneric(
  "lookup",
  function(x, key, value) {
    standardGeneric("lookup")
  })

#' Formule del `GrafoDB`
#'
#' Ritorna come named list le formule per ogni serie specificata in `nomi`
#'
#' @name expr
#' @usage expr(x, nomi)
#' @param x istanza di oggetto R
#' @param nomi character array di nomi di serie storiche
#' @param echo stampa con un messaggio su standard output il valore della formula
#' @return `list` con nomi (i nomi sono gli stess del parametro `nomi`) con le formule
#' @examples \dontrun{
#' g <- GrafoDB(...)
#' expr(g, "TETSZ0AC") # ritorna list(TETSZ0AC = "TETSZ0AC = ASTSZ0AC...")
#' }
#' @rdname expr_generic
#' @export

setGeneric(
  "expr",
  function(x, nomi, echo=TRUE) {
    standardGeneric("expr")
  })

setGeneric(
  "isRoot",
  function(x, name) {
    standardGeneric("isRoot")
  })

setGeneric(
  "upgrf",
  function(x, name, livello=.Machine$integer.max) {
    standardGeneric("upgrf")
  })

setGeneric(
  "downgrf",
  function(x, name, livello=.Machine$integer.max) {
    standardGeneric("downgrf")
  })


## Definizione della classe GrafoDB

#' Classe per accedere ai dati immagazzinati in PostgreSQL del Grafo CF
#'
#' @name GrafoDB
#' @rdname GrafoDB
#' @aliases GrafoDB-class
#' @title Database Grafo
#' @param ... don't know if used
#' @slot data \link[hash]{hash} containing changed data of this \link{GrafoDB}
#' @slot tag edition of this Graph
#' @slot network \link[igraph]{igraph} containing the DAG
#' @slot metadati temporary metadata added by the user to be saved on the DB
#' @exportClass GrafoDB
#' @export GrafoDB
#' @import igraph hash methods rcf

GrafoDB <- setClass(
  "GrafoDB",
  representation(
    tag = "character",
    network = "igraph",
    data = "hash",
    functions = "hash",
    metadati = "data.frame",
    timestamp = "POSIXct"
  ),
  contains = "DBDataset")

#' costruttore per la classe GrafoDB
#'
#' @name initialize
#' @rdname GraphDB_initialize
#' @aliases GrafoDB-initialize
#' @import igraph RPostgreSQL rcf

setMethod(
  "initialize",
  signature("GrafoDB"),
  function(.Object, tag="cf10") {
    if(is.null(tag)) {
      tag <- "cf10"
    }
    .Object@tag <- tag
    .Object@data <- hash()
    .Object@functions <- hash()
    .Object@metadati <- data.frame(name=character(),
                                   key=character(),
                                   value=character())
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    archi_table_name <- paste0("archi_", tag)

    network <- if(dbExistsTable(con, archi_table_name)) {
      edges <- dbReadTable(con, paste0("archi_", tag))
      edges$id <- NULL ## cancello gli id
      edges$tag <- NULL ## cancello il tag
      graph.data.frame(edges,directed=TRUE)
    } else {
      graph.empty(directed=TRUE)
    }

    df <- dbGetPreparedQuery(
      con,
      "select * from grafi where tag = ?",
      bind.data = tag)

    if(nrow(df)) {
      .Object@timestamp <- df$last_updated
      if(interactive()) {
        message(df$comment)
      }
    } else {
      .Object@timestamp <- Sys.time()
    }

    .Object@network <- network
    .Object
  })


setMethod(
  "show",
  signature("GrafoDB"),
  function(object) {
    data <- object@data
    functions <- object@functions

    con <- pgConnect()
    on.exit(dbDisconnect(con))
    num <- as.numeric(dbGetPreparedQuery(
      con, "select count(id) from dati where tag = ?", bind.data = object@tag))
    msg <- paste0("GrafoDB with ", num, " series")
    if(length(data)) {
      msg <- paste0(msg, ", ", length(data), " data changes")
    }

    if(length(functions)) {
      msg <- paste0(msg, ", ", length(functions), " function changes")
    }

    message(msg)
  })

#' ricerca nei metadati del `GrafoDB`
#'
#' @name lookup
#' @usage lookup(x, key, value)
#' @param x istanza di `GrafoDB`
#' @param key `character` che specifica la chiave del metadato
#' @param value `character` che specifica il valore del metadato
#' @return un character array di nomi di serie che rispettano la clausola `key` = `value`. Se non esistono ritorna un character(0) (array vuoto)
#' @examples \dontrun{
#' g = GrafoDB(...) # istanzia il grafo
#' lookup(g, "TAVOLA_DI_OUTPUT", "BRI") # ritorna i nomi di serie che hanno TAVOLA_DI_OUTPUT=BRI
#' }
#' @export

setMethod(
  "lookup",
  c("GrafoDB", "character", "character"),
  function(x, key, value) {
    metadati <- x@metadati
    tag <- x@tag
    cond <- cbind(tag, key, value)
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    ## non ci sono prepared statement funzionanti. maledetti.
    df <- dbGetPreparedQuery(
      con, "select name from metadati where tag = ? and key = ? and value = ?",
      bind.data = cond)

    ## merge con i dati appena cambiati

    in.memory <- as.character(metadati[metadati$key == key & metadati$value == value,]$name)
    ret <- unique(c(df$name, in.memory))
    if(length(ret)) {
      ret
    } else {
      stop("Non esistono serie con i criteri ", key, " == ", value)
    }
  })

setMethod(
  "-",
  c("GrafoDB", "GrafoDB"),
  function(e1,e2) {
    nomi1 <- names(e1)
    nomi2 <- names(e2)
    common <- intersect(nomi1,nomi2)
    notCommon <- setdiff(nomi1,nomi2)
    if(length(notCommon) > 0) {
      lapply(notCommon, function(name) {
        warning(paste0(name, " not common, excluded from diff"))
      })
    }
    result <- Dataset()
    data <- .myParLapply(common, function(nome) {
      tryCatch(
        e1[[nome]] - e2[[nome]],
        error = function(err) {
          stop(nome, ": ", err, " ", class(e1), ",", class(e2))
        })
    })
    names(data) <- common
    result@data <- hash(data)
    result
  })

setMethod(
  "names",
  c("GrafoDB"),
  function(x) {
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    network <- x@network
    nodes <- V(network)[topological.sort(x@network)]$name

    all_names <- dbGetPreparedQuery(
      con,"select name from dati where tag=?", bind.data = x@tag)$name

    remaining <- setdiff(all_names, nodes)
    # per preservare l'ordinamento topologico
    c(nodes, remaining)
  })


#' Formule del `GrafoDB`
#'
#' Ritorna come named list le formule per ogni serie specificata in `nomi`
#'
#' @name expr
#' @usage expr(x, nomi)
#' @param x istanza di oggetto R
#' @param nomi character array di nomi di serie storiche
#' @return `list` con nomi (i nomi sono gli stess del parametro `nomi`) con le formule
#' @examples \dontrun{
#' g <- GrafoDB(...)
#' expr(g, "TETSZ0AC") # ritorna list(TETSZ0AC = "TETSZ0AC = ASTSZ0AC...")
#' }

setMethod(
  "expr",
  c("GrafoDB", "character", "ANY"),
  function(x, nomi, echo=TRUE) {
    functions <- x@functions
    in.functions <- intersect(keys(functions), nomi)
    da.caricare.db <- setdiff(nomi, in.functions)
    from.db <- if(length(da.caricare.db)) {
      con <- pgConnect()
      on.exit(dbDisconnect(con))
      params <- as.data.frame(cbind(x@tag, da.caricare.db))
      names(params) <- c("tag", "name")
      dbGetPreparedQuery(
        con,
        "select name, value from metadati where tag = ? and key='FORMULA' and name = ?",
        bind.data = params)
    } else {
      data.frame(name=character(), value=character())
    }


    in.functions <- foreach(row=iter(in.functions, by='row'), .combine=rbind) %do% {
      data.frame(name=row, value=functions[[row]])
    }

    formule <- rbind(in.functions, from.db)

    if(nrow(formule) == 0) {
      NULL
    } else if(nrow(formule) == 1) {
      task <- as.character(formule$value)
      if(interactive() && echo) {
        message(task)
      }
      invisible(task)
    } else {
      formule
      nomi <- formule$name
      formule$name
      ## rownames(formule) <- nomi

      ret <- vector(length(nomi), mode="list")
      for(i in seq_along(nomi)) {
        name <- nomi[[i]]
        ret[i] <- formule[formule$name == name,]$value
      }
      names(ret) <- nomi
      ret
    }
  })

#' Implementazione del generic `evaluate` del package `grafo`
#' per la classe `GrafoDB`
#'
#' Esegue il calcolo delle serie storiche presenti in questo Database
#'
#' @name evaluate
#' @seealso .evaluate
#' @usage evaluate(object)
#' @usage evaluate(object, v_start)
#' @include functions.r
#' @export
#' @note il generic e' definito nel package `grafo`
#' @examples \dontrun{
#' g <- GrafoDB(...) # istanzia il grafo
#' g <- evaluate(g)  # lo valuta tutto
#' v_start <- "ZERIQ"
#' g <- evaluate(g, v_start) # lo valuta solo a partire da ZERIQ
#' }


setMethod(
  "evaluate",
  signature("GrafoDB", "ANY", "ANY"),
  .evaluate)

.setfunction <- function(object, name, f) {
  tag <- object@tag

  ## epurazione di f
  task <- .declutter_function(f)
  con <- pgConnect()
  on.exit(dbDisconnect(con))
  df <- dbGetPreparedQuery(
    con,
    "select id from metadata where tag=? and name=? and key='FORMULA'",
    bind.data = name)

  if(nrow(df)) {
    sql <- "update metadati set value=? where tag = ? and id = ?"
    dbGetPreparedQuery(con, sql, bind.data = data.frame(value=task, tag=tag, id=df$id))
  } else {
    sql <- "insert into metadati(name, key, value, tag) values (?, 'FORMULA', ?, ?)"
    dbGetPreparedQuery(con, sql, bind.data = data.frame(name=name, value=task, tag=tag))
  }
}

setGeneric(
  "setFunction",
  function(object, name, f) {
    standardGeneric("setFunction")
  })

setMethod(
  "setFunction",
  signature("GrafoDB", "character", "function"),
  .setfunction)

setMethod(
  "isRoot",
  signature("GrafoDB", "character"),
  function(x, name) {
    network <- x@network
    sources <- V(network)[degree(network, mode="in") == 0]$name
    all(name %in% sources)
  })

#' implementazione di describe di `package::grafo`
#'
#' @name describe
#' @title Funzioni del package `grafo`
#' @usage describe(graph, nodes, order, mode, plot)
#' @seealso `grafo::describe`

setMethod(
  "describe",
  signature("GrafoDB", "ANY", "ANY", "ANY", "ANY"),
  function(object, nodes=NULL, order=1L, mode='out', plot=FALSE) {

    if(!is.null(nodes))  {
      nodes <- as.character(nodes)
    }
    order <- as.integer(order)
    mmode <- as.character(mode)
    plot <- as.logical(plot)

    network <- object@network
    x <- if (!is.null(nodes)) {
      unlist(
        neighborhood(graph=network, order=order, nodes=nodes, mode=mode))
    } else {
      nodes <- topological.sort(network)[1]
      unlist(
        neighborhood(graph=network, order=order, nodes=nodes, mode='out'))
    }

    g1 <- induced.subgraph(network, x)
    V(g1)$label=V(g1)$name
    ret <- V(g1)$name
    ret <- ret[ which(ret != nodes) ]
    if (length(ret)== 0) {
      return(invisible(NULL))
    }
    ret
  })

setMethod(
  "upgrf",
  signature("GrafoDB", "character", "ANY"),
  function(x, name, livello=.Machine$integer.max) {
    describe(x, name, order=livello, mode="in")
  })

setMethod(
  "downgrf",
  signature("GrafoDB", "character", "ANY"),
  function(x, name, livello=.Machine$integer.max) {
    describe(x, name, order=livello, mode="out")
  })

setMethod(
  "getMetadata",
  signature("GrafoDB", "character", "ANY"),
  function(object, tsName, full=FALSE) {
    con <- pgConnect()
    on.exit(dbDisconnect(con))

    metadati <- object@metadati

    df <- dbGetPreparedQuery(
      con,
      "select name, key, value from metadati where tag = ? and name = ?",
      bind.data <- cbind(object@tag, tsName))
    
    ## overwrite metadata modifed in local session
    if(nrow(df)) {
      df[df$name == metadati$name & df$key == metadati$key & df$value == metadati$value, ] <- NULL
    }
    rbind(metadati[metadati$name == tsName,], df)
  })
