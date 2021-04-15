#' Ottiene i valori di un `metadato` per una `serie``
#'
#' Ritorna i valori di un metadato (o piu' metadati) specificati
#' dal parametro `metadato` della serie specificata dal
#' parametro `serie`
#'
#' @name getMeta
#' @param x istanza di `GrafoDB`
#' @param serie nome della serie d'interesse
#' @param metadato nome del metadato di cui si vogliono i valori
#' @return un character vector con i nomi dei valori del metadati
#' @examples \dontrun{
#'   g <- GrafoDB()
#'   getMeta(g, "TETSZ0AC",
#'              "TAVOLA_DI_OUTPUT") ## ritorna tutti i valori del metadato
#'                                  ## 'TAVOLA_DI_OUTPUT' per 'TETSZ0AC'
#' }
#' @exportMethod getMeta
#' @export

methods::setGeneric(
  "getMeta",
  function(x, serie, metadato) {
    standardGeneric("getMeta")
  })

#' ricerca nei metadati del `GrafoDB`
#'
#' @name lookup
#' @param x istanza di `GrafoDB`
#' @param key `character` che specifica la chiave del metadato
#' @param value `character` che specifica il valore del metadato
#' @return un character array di nomi di serie che rispettano la
#'         clausola `key` = `value`.
#'         Se non esistono ritorna un character(0) (array vuoto)
#' @examples \dontrun{
#' g = GrafoDB(...) # istanzia il grafo
#' lookup(g, "TAVOLA_DI_OUTPUT", "BRI") # ritorna i nomi di serie
#'                                      # che hanno TAVOLA_DI_OUTPUT=BRI
#' }
#' @rdname lookup
#' @include lookup.r
#' @export

methods::setGeneric(
  "lookup",
  function(x, key, value) {
    standardGeneric("lookup")
  })

#' Formule del `GrafoDB`
#'
#' Ritorna come named list le formule per ogni serie specificata in `nomi`
#'
#' @name expr
#' @param x istanza di oggetto R
#' @param nomi character array di nomi di serie storiche
#' @param echo stampa con un messaggio su standard output il
#'             valore della formula
#' @return `list` con nomi (i nomi sono gli stess del parametro `nomi`)
#'         con le formule
#' @examples \dontrun{
#' g <- GrafoDB(...)
#' expr(g, "TETSZ0AC") # ritorna list(TETSZ0AC = "TETSZ0AC = ASTSZ0AC...")
#' }
#' @export

methods::setGeneric(
  "expr",
  function(x, nomi, echo = FALSE) {
    standardGeneric("expr")
  })


#' predicato che ritorna `TRUE` se le serie sono radici
#'
#' Ritorna `TRUE` se le serie date nel parametro `name` non hanno serie
#' entranti
#'
#' @name isRoot
#' @param x istanza di GrafoDB
#' @param name vettore di nomi di serie
#' @return `TRUE` se `name` sono serie senza archi entranti
#' @export

methods::setGeneric(
  "isRoot",
  function(x, name) {
    standardGeneric("isRoot")
  })

#' predicato che ritorna `TRUE` se le serie sono foglie
#'
#' Ritorna `TRUE` se le serie date nel parametro `name` non hanno serie
#' uscienti
#'
#' @name isLeaf
#' @param x istanza di GrafoDB
#' @param name vettore di nomi di serie
#' @return `TRUE` se `name` sono serie senza archi uscenti
#' @export

methods::setGeneric(
  "isLeaf",
  function(x, name) {
    standardGeneric("isLeaf")
  })

#' Ritorna i figli delle serie
#'
#' ritorna i nomi delle serie che sono generate dalle serie date in `name`
#'
#' @name downgrf
#' @param x un istanza di GrafoDB
#' @param name array di nomi di serie
#' @param livello numero di livelli (ordine) da considerare (di default, tutti)
#' @return nomi di serie
#' @export

methods::setGeneric(
  "downgrf",
  function(x, name, livello = .Machine$integer.max) {
    standardGeneric("downgrf")
  })

methods::setOldClass("igraph")

#' Main GrafoDB class
#'
#' Class for manipulating data described with formulas and evaluated as
#' a topological sorting of nodes in a graph data structure.
#' This class provides a persistence layer towards generic DBMS
#' (by now only PostgreSQL) has been implemented
#'
#' @export GrafoDB
#' @name GrafoDB-class
#' @slot tag edition of this Graph
#' @slot network \link[igraph]{igraph} containing the DAG
#' @slot data \link[hash]{hash} containing changed data of this \link{GrafoDB}
#' @slot functions \link[hash]{hash} containing function data
#'    of this \link{GrafoDB}
#' @slot ordinal ordinale dei dati storici (0 per la produzione corrente)
#' @slot timestamp of the current GrafoDB
#' @slot touched object being modified
#' @slot edges the edges being modified
#' @slot dbdati data.frame containing data
#' @slot dbformule data.frame containing functions
#' @slot helper SQLHelper class
#' @exportClass GrafoDB
#' @include sqlhelper.r
#' @importClassesFrom rdataset Dataset
#' @importFrom methods new
#' @examples \dontrun{
#'    g = GrafoDB("cf10") # istanzia il grafo chiamato 'cf10'
#'                        # in questo caso ordinal e' 0
#'    g = GrafoDB("cf10p2") # istanzia il grafo cf10 con provvisorio p2;
#'                          # in questo caso ordinal e' 2
#' }
#' @rdname GrafoDB-class


GrafoDB <- methods::setClass( # nolint
  "GrafoDB",
  slots = list(
    tag = "character",
    network = "igraph",
    data = "hash",
    functions = "hash",
    ordinal = "numeric",
    timestamp = "numeric",
    touched = "character",
    edges = "hash",
    dbdati = "data.frame",
    dbformule = "data.frame",
    helper = "SQLHelper"),
  contains = "Dataset")


#' Default Constructor for GrafoDB
#'
#' @name initialize
#' @rdname GraphDB_initialize
#' @param .Object GrafoDB object instance
#' @param tag label identifying the GrafoDB

methods::setMethod(
  "initialize",
  signature("GrafoDB"),
  function(.Object, tag = "cf10") { # nolint
    init_grafo_impl(.Object, tag)
  })


#' 'navigates' in the graph
#'
#' This method lets you move through the nodes of the graph data
#' structure, movigin surfing the edges of the graph.
#'
#'
#' @name navigate
#' @title Funzioni del package `grafo`
#' @include navigate.r
#' @export
#' @param object GrafoDB instance
#' @param nodes array of node names
#' @param order number o levels (orders) to extend
#' @param mode direction of navigation: `"in"``, `"out"`` or `"all"`,
#'  default `"out"``

methods::setGeneric(
  "navigate",
  function(object, nodes=NULL, order=1L, mode="out") {
    standardGeneric("navigate")
  })

#' @rdname navigate

methods::setMethod(
  "navigate",
  signature("GrafoDB", "ANY", "ANY", "ANY"),
  function(object, nodes=NULL, order=1L, mode="out") {
    navigate_impl(object,
      nodes = nodes,
      order = order,
      mode = mode)
  })

#' Prints a brief summary for the graph
#'
#' @name show
#' @aliases show,GrafoDB
#' @usage show(object)
#' @param object GrafoDB instance
#' @note this is the default method called in the REPL when
#'   an object is printed out
#' @method

methods::setMethod(
  "show",
  signature("GrafoDB"),
  function(object) {
    data <- object@data
    functions <- object@functions
    tag <- object@tag
    num <- length(names(object))
    timestamp <- object@timestamp

    msg <- paste0("GrafoDB [", tag, "] with ", num, " series, ",
      as.character(as.POSIXct(timestamp / 1000, origin = "1970/01/01")))

    if (length(data)) {
      msg <- paste0(msg, ", ", length(data), " data changes")
    }
    if (length(functions)) {
      msg <- paste0(msg, ", ", length(functions), " function changes")
    }

    message(msg)
  })

#' @rdname lookup

methods::setMethod(
  "lookup",
  c("GrafoDB", "character", "character"),
  function(x, key, value) {
    lookup_impl(x, key, value)
  })

#' @rdname lookup

methods::setMethod(
  "lookup",
  c("GrafoDB", "numeric", "missing"),
  function(x, key, value) {
    lookup_dati_impl(x, key)
  })

#' @rdname lookup

methods::setMethod(
  "lookup",
  c("GrafoDB", "character", "missing"),
  function(x, key, value) {
    lookup_formula_impl(x, key)
  })


#' Esegue la differenza tra due grafi
#'
# TODO: test me!!!
#' @param e1 instance of GrafoDB
#' @param e2 instance of GrafoDB
#' @examples \dontrun{
#'  nasecg <- GrafoDB("nasecg2004")
#'  cf10 <- GrafoDB()
#'  diff <- cf10 - nasecg
#' }

methods::setMethod(
  "-",
  c("GrafoDB", "GrafoDB"),
  function(e1, e2) {

    is_scalar <- function(e) {
      is.numeric(e) & length(e) == 1 & !stats::is.ts(e)
    }

    nomi1 <- names(e1)
    nomi2 <- names(e2)
    common <- intersect(nomi1, nomi2)
    not_common <- setdiff(union(nomi1, nomi2), common)
    if (length(not_common) > 0) {
      lapply(not_common, function(name) {
        warning(paste0(name, " not common, excluded from difference"))
      })
    }

    result <- rdataset::Dataset()
    name <- NULL
    data <- foreach::`%dopar%`(foreach::foreach(
        name = iterators::iter(common), .combine = append), {
      ret <- list()
      ret[[name]] <- tryCatch({
        e11 <- e1[[name]]
        e22 <- e2[[name]]

        stopifnot(is.numeric(e11))
        stopifnot(is.numeric(e22))

        if (is_scalar(e11) && stats::is.ts(e22) ||
             stats::is.ts(e11) && is_scalar(e22)) {
          stop("Different object classes")
        }
        e11 - e22
      },
        error = function(err) {
          stop(name, ": ", err, " ", class(e11), ",", class(e22))
        })
      ret
    })

    names(data) <- common
    result@data <- hash::hash(data)
    result
  })

#' Returns all the keys in this graph.
#'
#' The array returned is already sorted by topological sorting
#' algorithm
#'
#' @rdname GrafoDB-class
#' @param x GrafoDB instance
#' @return all the nodes in the graph, topologically sorted.

methods::setMethod(
  "names",
  c("GrafoDB"),
  function(x) {
    data <- x@data
    dbdati <- x@dbdati
    network <- x@network
    nodes <- igraph::V(network)[igraph::topological.sort(network)]$name

    all_names <- union(
      hash::keys(data),
      if (is.null(dbdati$name)) {
        character()
      } else {
        dbdati$name
      })
    all_names <- union(all_names, igraph::V(network)$name)

    remaining <- setdiff(all_names, nodes)
    # per preservare l'ordinamento topologico
    ret <- c(nodes, remaining)
    if (is.null(ret)) {
      character(0)
    } else {
      ret
    }
  })

#' @rdname expr

methods::setMethod(
  "expr",
  c("GrafoDB", "character", "ANY"),
  function(x, nomi, echo = FALSE) {
    expr_impl(x, nomi, echo)
  })

#' Implementazione del generic `evaluate` del package `grafo`
#' per la classe `GrafoDB`
#'
#' Esegue il calcolo delle serie storiche presenti in questo Database
#'
#' @seealso evaluate_impl
#' @include functions.r
#' @export
#' @note il generic e' definito nel package `grafo`
#' @examples \dontrun{
#' g <- GrafoDB(...) # istanzia il grafo
#' g <- evaluate(g)  # lo valuta tutto
#' v_start <- "ZERIQ"
#' g <- evaluate(g, v_start) # lo valuta solo a partire da ZERIQ
#' }
#' @include evaluate.r
#' @exportMethod evaluate
#' @param object GrafoDB instance
#' @param v_start node to evaluate, if `NULL` evaluates the whole GrafoDB
#' @param ... other eventual params (for debugging purposes...)

methods::setGeneric(
  "evaluate",
  function(object, v_start = NULL, ...) {
    standardGeneric("evaluate")
  })

#' @rdname evaluate

methods::setMethod(
  "evaluate",
  signature("GrafoDB", "ANY"),
  function(object, v_start = NULL, ...) {
    evaluate_impl(object, v_start, ...)
  })

#' returns `TRUE` if `name` is a root of the graph
#'
#' @name isRoot
#' @param x GrafoDB instance
#' @param name name of the object to be checked
#' @return `TRUE` if a root, `FALSE` otherwise

methods::setGeneric(
  "isRoot",
  function(x, name) {
    standardGeneric("isRoot")
  })

#' @rdname isRoot

methods::setMethod(
  "isRoot",
  signature("GrafoDB", "character"),
  function(x, name) {
    .isRoot(x, name)
  })

#' returns `TRUE` if `name` is a leaf
#'
#' A leaf is a node without outgoing edges.
#'
#' @name isLeaf
#' @param x GrafoDB instance
#' @param name name of the object identifying the node
#' @return `TRUE` if is leaf, `FALSE` otherwise

methods::setGeneric(
  "isLeaf",
  function(x, name) {
    standardGeneric("isLeaf")
  })

#' @rdname isLeaf

methods::setMethod(
  "isLeaf",
  list(x = "GrafoDB", name = "character"),
  function(x, name) {
    .isLeaf(x, name)
  })

#' @rdname downgrf

methods::setMethod(
  "downgrf",
  signature("GrafoDB", "character", "ANY"),
  function(x, name, livello = .Machine$integer.max) {
    navigate(x, name, order = livello, mode = "out")
  })



#' get Metadata for object
#'
#' @name getMetadata
#' @param object GrafoDB instance
#' @param ts_name object name
#' @param full boolean, triggers full report
#' @include metadati.r
#' @exportMethod getMetadata

methods::setGeneric(
  "getMetadata",
  function(object, ts_name, full = FALSE) {
    standardGeneric("getMetadata")
  })

#' @rdname getMetadata

methods::setMethod(
  "getMetadata",
  signature("GrafoDB", "character", "ANY"),
  function(object, ts_name, full = FALSE) {
    get_metadata_impl(object, ts_name)
  })

#' Edita un la formula di una serie storica.
#'
#' @name edita
#' @param x istanza di grafo
#' @param name nome della serie storica
#' @param ... altri parametri generici
#' @return il grafo con la formula modificata
#' @include functions.r
#' @export

methods::setGeneric(
  "edita",
  function(x, name, ...) {
    standardGeneric("edita")
  })

#' @rdname edita

methods::setMethod(
  "edita",
  signature("GrafoDB", "character"),
  function(x, name, ...) {
    object_name <- deparse(substitute(x))
    x <- .edita(x, name)
    assign(object_name, x, envir = parent.frame())
    invisible(x)
  })

methods::setMethod(
  "as.list",
  signature("GrafoDB"),
  function(x) {
    x[[names(x)]]
  })

#' Valuta una singola serie del grafo e ritorna il risultato
#'
#' @name ser
#' @param x un istanza di GrafoDB
#' @param name nome della serie da valutare
#' @param debug debug mode (`FALSE` default)
#' @return la serie valutata
#' @export
#' @exportMethod ser

methods::setGeneric(
  "ser",
  function(x, name, debug=FALSE) {
    standardGeneric("ser")
  })

#' @rdname ser

methods::setMethod(
  "ser",
  signature("GrafoDB", "character"),
  function(x, name, debug=FALSE) {
    ser_impl(x, name, debug = debug)
  }
)

#' Yields the dependencies of an object
#'
#' @name deps
#' @param x istanza di GrafoDB
#' @param name nome della serie
#' @return a character array of object names, NULL if not present
#' @export

methods::setGeneric(
  "deps",
  function(x, name) {
    standardGeneric("deps")
  })

#' @rdname deps

methods::setMethod(
  "deps",
  signature("GrafoDB", "character"),
  function(x, name) {
    upgrf(x, name, livello = 1)
  })

#' @importMethodsFrom rdataset as.dataset

methods::setMethod(
  "as.dataset",
  list(x = "GrafoDB"),
  function(x, ...) {
    x[names(x)]
  })

methods::setMethod(
  "as.list",
  list(x = "GrafoDB"),
  function(x, ...) {
    x[[names(x)]]
  })

#' @include metadati.r

#' @rdname getMeta

methods::setMethod(
  "getMeta",
  signature("GrafoDB", "character", "character"),
  function(x, serie, metadato) {
    get_meta_impl(x, serie, metadato)
  })

#' @rdname getMeta

methods::setMethod(
  "getMeta",
  signature("GrafoDB", "character", "ANY"),
  function(x, serie, metadato) {
    getMetadata(x, serie)
  })

#' @rdname getMeta
#' 
methods::setMethod(
  "getMeta",
  signature("GrafoDB", "missing", "missing"),
  function(x) {
    con <- build_connection()
    on.exit(disconnect(con))
    DBI::dbGetQuery(con, sql_by_key(
      x@helper, "GET_ALL_META",
      tag = x@tag))
  })

#' returns all the keys in the metadata table
#'
#' @rdname GrafoDB-class
#' @include metadati.r
#' @importMethodsFrom hash keys
#' @exportMethod keys
#' @aliases GrafoDB

methods::setMethod(
  "keys",
  list(x = "GrafoDB"),
  function(x) {
    keys_impl(x)
  })

#' Returns all the values in the metadata table
#'
#' @rdname GrafoDB-class
#' @include metadati.r
#' @importMethodsFrom hash values
#' @param x GrafoDB instance
#' @param key key of the values 
#' @exportMethod values
#' @aliases GrafoDB

methods::setMethod(
  "values",
  signature("GrafoDB"),
  function(x, key) {
    key <- rutils::ifelse(length(key) == 1, key[[1]], NULL)
    values_by_key_impl(x, key = key)
  })


#' Rinomina una serie del grafo
#'
#' L'operazione lavora direttamente sui dati in modo persistente.
#'
#' @name rename
#' @param x istanza di `GrafoDB`
#' @param vecchio nome vecchio da sostituire
#' @param nuovo nome nuovo da sostituire
#' @return grafo modificato
#' @examples \dontrun{
#'     g <- GrafoDB("test")
#'     rename(g, "TS1", "TS2")
#'     "TS2" %in% names(g) # questo e' `TRUE`
#'     "TS1" %in% names(g) # questo e' `FALSE`
#' }
#' @include rename.r

methods::setGeneric(
  "rename",
  function(x, vecchio, nuovo) {
    standardGeneric("rename")
  })

methods::setMethod(
  "rename",
  signature("GrafoDB", "character", "character"),
  function(x, vecchio, nuovo) {
    rename_impl(x, vecchio, nuovo)
  })


#' Returns the roots of the Graph
#'
#' 'Roots' are those nodes *not* having incoming edges
#'
#' @name roots
#' @param x GrafoDB instance
#' @param ... other params (unused)
#' @return character array with the root names
#' @include functions.r
#' @export

methods::setGeneric(
  "roots",
  function(x, ...) {
    standardGeneric("roots")
  })

#' @rdname roots

methods::setMethod(
  "roots",
  signature("GrafoDB"),
  function(x)  {
    .roots(x)
  })

#' Returns all the leaves in a graph
#'
#' @name leaves
#' @param x GrafoDB instance
#' @param ... other params (unused)
#' @return character array with the leaves names
#' @include functions.r
#' @export

methods::setGeneric(
  "leaves",
  function(x, ...) {
    standardGeneric("leaves")
  })

#' @rdname leaves

methods::setMethod(
  "leaves",
  signature("GrafoDB"),
  function(x)  {
    .leaves(x)
  })


#' Extract data from GrafoDB
#'
#' returns data from GrafoDB (seen here as a key-value container)
#' by specifying the key (a string) for a value
#'
#' @aliases GrafoDB,character
#' @param x an instance of GrafoDB
#' @param ... nothing important (just to comply on generic)
#' @rdname GrafoDB-methods
#' @return a Dataset or a single object
#' @note you can specify string separated by commas to obtain multiple gets:
#'      `g$"A,B,C"` yields `A`,`B`,`C` in a `list`

methods::setMethod(
  "$",
  list(x = "GrafoDB"),
  function(x, name) {
    x[[unlist(stringr::str_split(name, " "))]]
  })

#' internal function to implement $<-
#'
#' @name dollar_impl
#' @param x GrafoDB instance
#' @param name key of the object to be set
#' @param value value of the object to set
#' @return an invisible `x`

dollar_impl <- function(x, name, value) {
  x <- subsetting(x, name, value)
  invisible(x)
}



#' Extract data from GrafoDB
#'
#' returns data from GrafoDB (seen here as a key-value container)
#' by specifying the key (a string) for a value
#'
#' @aliases GrafoDB,ANY
#' @param x an instance of GrafoDB
#' @param name name of the object
#' @param value object to be set
#' @param ... nothing important (just to comply on generic)
#' @rdname GrafoDB-methods
#' @return a Dataset or a single object

methods::setReplaceMethod(
  "$", c(x = "GrafoDB", value = "ANY"),
  function(x, name, value) {
    dollar_impl(x, name, value)
  })
