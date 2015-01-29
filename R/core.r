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

#' Ritorna i genitori delle serie
#'
#' ritorna i nomi delle serie che entrano nelle serie date in `name`
#'
#' @name upgrf
#' @usage upgrf(x, name)
#' @usage upgrf(x, name, livello)
#' @param x un istanza di GrafoDB
#' @param name array di nomi di serie
#' @param livello numero di livelli (ordine) da considerare (di default, tutti)
#' @return nomi di serie
#' @export

setGeneric(
  "upgrf",
  function(x, name, livello=.Machine$integer.max) {
    standardGeneric("upgrf")
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
    .init(.Object, tag)
  })


#' naviga nel grafo
#'
#' @name navigate
#' @title Funzioni del package `grafo`
#' @usage navugate(graph, nodes, order, mode, plot)
#' @seealso `grafo::describe`
#' @import igraph
#' @export

setGeneric(
  "navigate",
  function(object, nodes=NULL, order=1L, mode='out', plot=FALSE) { 
    standardGeneric("navigate")
  })

setMethod(
  "navigate",
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
  "show",
  signature("GrafoDB"),
  function(object) {
    data <- object@data
    functions <- object@functions
    tag <- object@tag
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    num <- as.numeric(dbGetPreparedQuery(
      con, "select count(id) from dati where tag = ?", bind.data = object@tag))
    msg <- paste0("GrafoDB [",tag,"] with ", num, " series, ", as.character(object@timestamp))
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
    tag <- x@tag
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    ## non ci sono prepared statement funzionanti. maledetti.
    df <- dbGetPreparedQuery(
      con, "select distinct name from metadati where tag = ? and key = ? and value = ?",
      bind.data = cbind(tag, key, value))
    as.character(df$name)    
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


setMethod(
  "evaluate",
  signature("GrafoDB", "ANY", "ANY"),
  function(object, v_start=NULL, deep=F) {
    .evaluate(object, v_start, deep)
  })

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
#' @export

setMethod(
  "describe",
  signature("GrafoDB", "ANY", "ANY", "ANY", "ANY"),
  function(object, nodes=NULL, order=1L, mode='out', plot=FALSE) {
    navigate(object, nodes, order, mode, plot)
  })

setMethod(
  "upgrf",
  signature("GrafoDB", "character", "ANY"),
  function(x, name, livello=.Machine$integer.max) {
    navigate(x, name, order=livello, mode="in")
  })

setMethod(
  "downgrf",
  signature("GrafoDB", "character", "ANY"),
  function(x, name, livello=.Machine$integer.max) {
    navigate(x, name, order=livello, mode="out")
  })

setMethod(
  "getMetadata",
  signature("GrafoDB", "character", "ANY"),
  function(object, tsName, full=FALSE) {
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    dbGetPreparedQuery(
      con,
      "select name, key, value from metadati where tag = ? and name = ?",
      bind.data <- cbind(object@tag, tsName))
  })


.simpleGraph <- function(tag) {
  g <- GrafoDB(tag)
  g["A"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["B"] <- TSERIES(runif(10), START=c(1990,1), FREQ=4)
  g["C"] <- function(A,B) {
    C = A + B    
  }
  
  g <- setMeta(g, "A", "key", "value")
  g <- setMeta(g, "B", "key", "value")
  setMeta(g, "C", "key", "value1")
}

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
