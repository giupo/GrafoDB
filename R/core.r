#' Ottiene i valori di un `metadato` per una `serie``
#'
#' Ritorna i valori di un metadato (o piu' metadati) specificati
#' dal parametro `metadato` della serie specificata dal
#' parametro `serie`
#'
#' @name getMeta
#' @usage getMeta(x, serie, metadato)
#' @usage getMeta(x, serie)
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
#' @importFrom methods setGeneric
#' @export


setGeneric(
  "getMeta",
  function(x, serie, metadato) {
    standardGeneric("getMeta")
  })

#' ricerca nei metadati del `GrafoDB`
#'
#' @name lookup
#' @usage lookup(x, key, value)
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
#' @rdname lookup_generic
#' @include lookup.r
#' @importFrom methods setGeneric
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
#' @param echo stampa con un messaggio su standard output il
#'             valore della formula
#' @return `list` con nomi (i nomi sono gli stess del parametro `nomi`)
#'         con le formule
#' @examples \dontrun{
#' g <- GrafoDB(...)
#' expr(g, "TETSZ0AC") # ritorna list(TETSZ0AC = "TETSZ0AC = ASTSZ0AC...")
#' }
#' @rdname expr_generic
#' @export

setGeneric(
  "expr",
  function(x, nomi, echo=FALSE) {
    standardGeneric("expr")
  })


#' predicato che ritorna `TRUE` se le serie sono radici
#'
#' Ritorna `TRUE` se le serie date nel parametro `name` non hanno serie
#' entranti
#'
#' @name isRoot
#' @usage isRoot(x, name)
#' @param x istanza di GrafoDB
#' @param name vettore di nomi di serie
#' @return `TRUE` se `name` sono serie senza archi entranti
#' @export

setGeneric(
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
#' @usage isLeaf(x, name)
#' @param x istanza di GrafoDB
#' @param name vettore di nomi di serie
#' @return `TRUE` se `name` sono serie senza archi uscenti
#' @importFrom methods setGeneric
#' @export

setGeneric(
  "isLeaf",
  function(x, name) {
    standardGeneric("isLeaf")
  })

#' Ritorna i figli delle serie
#'
#' ritorna i nomi delle serie che sono generate dalle serie date in `name`
#'
#' @name downgrf
#' @usage downgrf(x, name)
#' @usage downgrf(x, name, livello)
#' @param x un istanza di GrafoDB
#' @param name array di nomi di serie
#' @param livello numero di livelli (ordine) da considerare (di default, tutti)
#' @importFrom methods setGeneric
#' @return nomi di serie
#' @export

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
#' @title Database Grafo
#' @param ... don't know if used
#' @slot data \link[hash]{hash} containing changed data of this \link{GrafoDB}
#' @slot tag edition of this Graph
#' @slot network \link[igraph]{igraph} containing the DAG
#' @slot metadati temporary metadata added by the user to be saved on the DB
#' @slot ordinal ordinale dei dati storici (0 per la produzione corrente)
#' @slot touched serie modificate in area di lavoro
#' @exportClass GrafoDB
#' @include sqlhelper.r
#' @export GrafoDB
#' @importClassesFrom rdataset Dataset
#' @importFrom methods setGeneric setOldClass setClass setReplaceMethod new loadMethod
#' @examples \dontrun{
#'    g = GrafoDB("cf10") # istanzia il grafo chiamato 'cf10'
#'                        # in questo caso ordinal e' 0
#'    g = GrafoDB("cf10p2") # istanzia il grafo cf10 con provvisorio p2;
#'                          # in questo caso ordinal e' 2
#' }

setOldClass("igraph")

GrafoDB <- setClass(
  "GrafoDB",
  representation(
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

#' @export

grafodb <- GrafoDB

#' costruttore per la classe GrafoDB
#'
#' @name initialize
#' @rdname GraphDB_initialize
#' @aliases GrafoDB-initialize
#' @importFrom methods setMethod

setMethod(
  "initialize",
  signature("GrafoDB"),
  function(.Object, tag="cf10") {
    .init(.Object, tag)
  })


#' naviga nel grafo
#'
#' @name navigate
#' @title Funzioni del package `grafo`
#' @usage navugate(graph, nodes, order, mode)
#' @seealso `grafo::describe`
#' @include navigate.r
#' @export

setGeneric(
  "navigate",
  function(object, nodes=NULL, order=1L, mode="out") {
    standardGeneric("navigate")
  })

setMethod(
  "navigate",
  signature("GrafoDB", "ANY", "ANY", "ANY"),
  function(object, nodes=NULL, order=1L, mode="out") {
    .navigate(object, nodes=nodes, order=order, mode=mode)
  })

setMethod(
  "show",
  signature("GrafoDB"),
  function(object) {
    data <- object@data
    functions <- object@functions
    tag <- object@tag
    num <- length(names(object))
    timestamp <- object@timestamp
    msg <- paste0("GrafoDB [",tag,"] with ", num, " series, ",
                  as.character(as.POSIXct(timestamp/1000, origin="1970/01/01")))
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
#' @return un character array di nomi di serie che rispettano la
#'         clausola `key` = `value`. Se non esistono ritorna un
#'         character(0) (array vuoto)
#' @examples \dontrun{
#' g = GrafoDB(...) # istanzia il grafo
#' lookup(g, "TAVOLA_DI_OUTPUT", "BRI") # ritorna i nomi di serie
#'                                      # che hanno TAVOLA_DI_OUTPUT=BRI
#' }
#' @export
#' @include lookup.r

setMethod(
  "lookup",
  c("GrafoDB", "character", "character"),
  function(x, key, value) {
    .lookup(x, key, value)
  })

setMethod(
  "lookup",
  c("GrafoDB", "numeric", "missing"),
  function(x, key, value) {
    .lookup_dati(x, key)
  })

setMethod(
  "lookup",
  c("GrafoDB", "character", "missing"),
  function(x, key, value) {
    .lookup_formula(x, key)
  })


#' Esegue la differenza tra due grafi
#'
#' @importFrom foreach %dopar%
#' @importFrom iterators iter
# TODO: test me!!!

setMethod(
  "-",
  c("GrafoDB", "GrafoDB"),
  function(e1, e2) {

    is.scalar <- function(e) {
      is.numeric(e) & length(e) == 1 & !is.ts(e)
    }

    nomi1 <- names(e1)
    nomi2 <- names(e2)
    common <- intersect(nomi1, nomi2)
    not_common <- setdiff(union(nomi1, nomi2), common)
    if(length(not_common) > 0) {
      lapply(not_common, function(name) {
        warning(paste0(name, " not common, excluded from difference"))
      })
    }

    result <- Dataset()
    data <- foreach(name=iter(common), .combine=append) %dopar% {
      ret <- list()
      ret[[name]] <- tryCatch({
        e11 <- e1[[name]]
        e22 <- e2[[name]]

        stopifnot(is.numeric(e11))
        stopifnot(is.numeric(e22))

        if(is.scalar(e11) && is.ts(e22) ||
             is.ts(e11) && is.scalar(e22)) {
          stop("Different object classes")
        }
        e11 - e22
      },
        error = function(err) {
          stop(name, ": ", err, " ", class(e11), ",", class(e22))
        })
      ret
    }

    names(data) <- common
    result@data <- hash(data)
    result
  })

setMethod(
  "names",
  c("GrafoDB"),
  function(x) {
    data <- x@data
    dbdati <- x@dbdati
    network <- x@network
    nodes <- igraph::V(network)[igraph::topological.sort(network)]$name

    all_names <- union(
      keys(data),
      if(is.null(dbdati$name)) {
        character()
      } else {
        dbdati$name
      })
    all_names <- union(all_names, igraph::V(network)$name)

    remaining <- setdiff(all_names, nodes)
    # per preservare l'ordinamento topologico
    ret <- c(nodes, remaining)
    if(is.null(ret)) {
      character(0)
    } else {
      ret
    }
  })


#' Formule del `GrafoDB`
#'
#' Ritorna come named list le formule per ogni serie specificata in `nomi`
#'
#' @name expr
#' @usage expr(x, nomi)
#' @param x istanza di oggetto R
#' @param nomi character array di nomi di serie storiche
#' @return `list` con nomi (i nomi sono gli stess del parametro `nomi`)
#'         con le formule
#' @examples \dontrun{
#' g <- GrafoDB(...)
#' expr(g, "TETSZ0AC") # ritorna list(TETSZ0AC = "TETSZ0AC = ASTSZ0AC...")
#' }
#' @export

setMethod(
  "expr",
  c("GrafoDB", "character", "ANY"),
  function(x, nomi, echo=FALSE) {
    .expr(x, nomi, echo)
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
#' @include evaluate.r
#' @exportMethod evaluate


setGeneric(
  "evaluate",
  function(object, v_start=NULL, deep=F, ...) {
    standardGeneric("evaluate")
  })

setMethod(
  "evaluate",
  signature("GrafoDB", "ANY", "ANY"),
  function(object, v_start=NULL, deep=F, ...) {
    .evaluate(object, v_start, deep, ...)
  })

setGeneric(
  "isRoot",
  function(x, name) {
    standardGeneric("isRoot")
  })

setMethod(
  "isRoot",
  signature("GrafoDB", "character"),
  function(x, name) {
    .isRoot(x, name)
  })

setGeneric(
  "isLeaf",
  function(x, name) {
    standardGeneric("isLeaf")
  })

setMethod(
  "isLeaf",
  signature("GrafoDB", "character"),
  function(x, name) {
    .isLeaf(x, name)
  })

#' implementazione di describe di `package::grafo`
#'
#' @name describe
#' @title Funzioni del package `grafo`
#' @usage describe(graph, nodes, order, mode)
#' @seealso `grafo::describe`
#' @exportMethod describe

setGeneric(
  "describe",
  function(object, nodes=NULL, order=1L, mode="out") {
    standardGeneric("describe")
  })

setMethod(
  "describe",
  signature("GrafoDB", "ANY", "ANY", "ANY"),
  function(object, nodes = NULL, order = 1L, mode = "out") {
    navigate(object, nodes, order, mode)
  })



#' @exportMethod downgrf

setGeneric(
  "downgrf",
  function(x, name, livello=.Machine$integer.max) {
    standardGeneric("downgrf")
  })

setMethod(
  "downgrf",
  signature("GrafoDB", "character", "ANY"),
  function(x, name, livello=.Machine$integer.max) {
    navigate(x, name, order = livello, mode = "out")
  })

#' @include metadati.r
#' @exportMethod getMetadata

setGeneric(
  "getMetadata",
  function(object, tsName, full = FALSE) {
    standardGeneric("getMetadata")
  })

setMethod(
  "getMetadata",
  signature("GrafoDB", "character", "ANY"),
  function(object, tsName, full = FALSE) {
    .getMetadata(object, tsName)
  })

#' Edita un la formula di una serie storica.
#'
#' @name edita
#' @usage edita(x, name)
#' @param x istanza di grafo
#' @param name nome della serie storica
#' @return il grafo con la formula modificata
#' @include functions.r
#' @export

setGeneric(
  "edita",
  function(x, name, ...) {
    standardGeneric("edita")
  })


#' Edita un la formula di una serie storica.
#'
#' @name edita
#' @usage edita(x, name)
#' @param x istanza di grafo
#' @param name nome della serie storica
#' @return il grafo con la formula modificata
#' @include functions.r
#' @export

setMethod(
  "edita",
  signature("GrafoDB", "character"),
  function(x, name, ...) {
    nameObject <- deparse(substitute(x))
    x <- .edita(x, name, ...)
    assign(nameObject, x, envir=parent.frame())
    invisible(x)
  })

setMethod(
  "as.list",
  signature("GrafoDB"),
  function(x) {
    x[[names(x)]]
  })

#' Valuta una singola serie del grafo e ritorna il risultato
#'
#' @name ser
#' @usage ser(x, name)
#' @param x un istanza di GrafoDB
#' @param name nome della serie da valutare
#' @param debug debug mode (`FALSE` default)
#' @return la serie valutata
#' @export

setGeneric(
  "ser",
  function(x, name, debug=FALSE) {
    standardGeneric("ser")
  })

#' Valuta una singola serie del grafo e ritorna il risultato
#'
#' @name ser
#' @usage ser(x, name)
#' @param x un istanza di GrafoDB
#' @param name nome della serie da valutare
#' @param debug se `TRUE` attiva la modalita' di debugging
#' @return la serie valutata
#' @include ser.r
#' @export

setMethod(
  "ser",
  signature("GrafoDB", "character"),
  function(x, name, debug=FALSE) {
    .ser(x, name, debug=debug)
  }
)

#' Ritorna le dipendeze di una serie
#'
#' Ritorna come array di caratteri i nomi delle serie su cui la serie specificata
#' dal parametro `name` dipende
#'
#' @name deps
#' @usage deps(x, name)
#' @param x istanza di GrafoDB
#' @param name nome della serie
#' @return un character array di nomi, NULL se la serie non ha dipendenze
#' @export

setGeneric(
  "deps",
  function(x, name) {
    standardGeneric("deps")
  })

setMethod(
  "deps",
  signature("GrafoDB", "character"),
  function(x, name) {
    upgrf(x, name, livello=1)
  })

#' @importMethodsFrom rdataset as.dataset

setMethod(
  "as.dataset",
  signature("GrafoDB"),
  function(x, ...) {
    x[names(x)]
  })

setMethod(
  "as.list",
  signature("GrafoDB"),
  function(x, ...) {
    x[[names(x)]]
  })

#' @include metadati.r

setMethod(
  "getMeta",
  signature("GrafoDB", "character", "character"),
  function (x, serie, metadato){
    .getMeta(x, serie, metadato)
  })

setMethod(
  "getMeta",
  signature("GrafoDB", "character", "ANY"),
  function (x, serie, metadato){
    getMetadata(x, serie)
  })

setMethod(
  "getMeta",
  signature("GrafoDB", "missing", "missing"),
  function(x) {
    con <- buildConnection()
    on.exit(disconnect(con))
    dbGetQuery(con, getSQLbyKey(
      x@helper, "GET_ALL_META",
      tag=x@tag))
  })

#' @include metadati.r
#' @exportMethod keys

setMethod(
  "keys",
  signature("GrafoDB"),
  function(x) {
    .keys(x)
  })

#' @include metadati.r
#' @exportMethod values

setMethod(
  "values",
  signature("GrafoDB"),
  function(x, ...) {
    nomemetadato <- list(...)
    key <- if(length(nomemetadato) == 1) {
      nomemetadato[[1]]
    } else {
      NULL
    }
    .values(x, key=key)
  })

#' Rinomina una serie del grafo
#'
#' L'operazione lavora direttamente sui dati in modo persistente.
#'
#' @name rename
#' @usage rename(x, vecchio, nuovo)
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

setGeneric(
  "rename",
  function(x, vecchio, nuovo) {
    standardGeneric("rename")
  })

setMethod(
  "rename",
  signature("GrafoDB", "character", "character"),
  function(x, vecchio, nuovo){
    .rename(x, vecchio, nuovo)
  })


#' Ritorna le radici del grafo
#'
#' @name roots
#' @usage roots(x)
#' @param x istanza di grafo
#' @return la lista di nomi delle radici del grafo
#' @include functions.r
#' @export

setGeneric(
  "roots",
  function(x, ...) {
    standardGeneric("roots")
  })

setMethod(
  "roots",
  signature("GrafoDB"),
  function (x)  {
    .roots(x)
  })

#' Ritorna le foglie del grafo
#'
#' @name leaves
#' @usage leaves(x)
#' @param x istanza di grafo
#' @return la lista di nomi delle foglie del grafo
#' @include functions.r
#' @export

setGeneric(
  "leaves",
  function(x, ...) {
    standardGeneric("leaves")
  })

setMethod(
  "leaves",
  signature("GrafoDB"),
  function (x)  {
    .leaves(x)
  })


#' @importFrom stringr str_split
setMethod(
  "$",
  signature("GrafoDB"),
  function(x, name) {
    x[[unlist(str_split(name, " "))]]
  })

.dollar <- function(x, name, value) {
  #nameObject <- deparse(substitute(x))
  x <- .subsetting(x, name, value)
  #assign(nameObject, x, envir=parent.frame())
  invisible(x)
}

setReplaceMethod(
  '$', c( x="GrafoDB", value="ANY"),
  function(x, name, value) {
    .dollar(x, name, value)
  })

