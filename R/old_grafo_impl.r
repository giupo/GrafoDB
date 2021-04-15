# Questo file contiene le implementazioni dei generics specificate
# nel package `grafo`


#' implementazione di isNode di `package::grafo`
#'
#' @name isNode
#' @title Funzioni del package `grafo`
#' @seealso `grafo::isNode`
#' @include core.r
#' @exportMethod isNode
#' @param graph GrafoDB instance
#' @param ts_name name of the object

methods::setGeneric(
  "isNode",
  function(graph, ts_name) {
    standardGeneric("isNode")
  })

#' @rdname isNode

methods::setMethod(
  "isNode",
  signature("GrafoDB", "character"),
  function(graph, ts_name) {
    ts_name %in% names(graph)
  })

#' implementazione di isPrimitive di `package::grafo`
#'
#' @name isPrimitive
#' @title Funzioni del package `grafo`
#' @seealso `grafo::isPrimitive`
#' @exportMethod isPrimitive
#' @param graph GrafoDB instance
#' @param ts_name name of the object

methods::setGeneric(
  "isPrimitive",
  function(graph, ts_name) {
    standardGeneric("isPrimitive")
  })

#' @rdname isPrimitive

methods::setMethod(
  "isPrimitive",
  signature("GrafoDB", "character"),
  function(graph, ts_name) {
    all(isRoot(graph, ts_name) && is.null(expr(graph, ts_name)))
  })

#' implementazione di isAggregate di `package::grafo`
#'
#' @name isAggregate
#' @title Funzioni del package `grafo`
#' @param graph GrafoDB instance
#' @param ts_name name of the object
#' @seealso `grafo::isAggregate`
#' @exportMethod isAggregate

methods::setGeneric(
  "isAggregate",
  function(graph, ts_name) {
    standardGeneric("isAggregate")
  })

#' @rdname isAggregate

methods::setMethod(
  "isAggregate",
  signature("GrafoDB", "character"),
  function(graph, ts_name) {
    !isRoot(graph, ts_name)
  })

#' implementazione di isElementary di `package::grafo`
#'
#' @name isElementary
#' @title Funzioni del package `grafo`
#' @seealso `grafo::isElementary`
#' @note `GrafoDB` non prevede l'utilizzo di serie "elementari" come il `grafo`
#'       Quindi per compliance ritorna sempre `FALSE`, ma il metodo non ha senso
#' @exportMethod isElementary
#' @param graph GrafoDB instance
#' @param ts_name name of the object

methods::setGeneric(
  "isElementary",
  function(graph, ts_name) {
    standardGeneric("isElementary")
  })

#' @rdname isElementary

methods::setMethod(
  "isElementary",
  signature("GrafoDB", "character"),
  function(graph, ts_name) {
    all(isRoot(graph, ts_name) && !is.null(expr(graph, ts_name)))
  })

#' implementazione di listAggregates di `package::grafo`
#'
#' @name listAggregates
#' @title Funzioni del package `grafo`
#' @seealso `grafo::listAggregates`
#' @exportMethod listAggregates
#' @param graph GrafoDB instance

methods::setGeneric(
  "listAggregates",
  function(graph) {
    standardGeneric("listAggregates")
  })

#' @rdname listAggregates

methods::setMethod(
  "listAggregates",
  signature("GrafoDB"),
  function(graph) {
    network <- graph@network
    formule <- graph@dbformule$name
    leaves <- igraph::V(network)[
      igraph::degree(network, mode = "out") == 0]$name
    setdiff(formule, leaves)
  })


#' implementazione di listElementaries di `package::grafo`
#'
#' @name listElementaries
#' @title Funzioni del package `grafo`
#' @seealso `grafo::listElementaries`
#' @exportMethod listElementaries
#' @param graph GrafoDB instance

methods::setGeneric(
  "listElementaries",
  function(graph) {
    standardGeneric("listElementaries")
  })

#' @rdname listElementaries

methods::setMethod(
  "listElementaries",
  signature("GrafoDB"),
  function(graph) {
    network <- graph@network
    tag <- graph@tag
    formule <- union(graph@dbformule$name, hash::keys(graph@functions))
    sources <- igraph::V(network)[
      igraph::degree(network, mode = "in") == 0]$name
    intersect(sources, formule)
  })

#' implementazione di listPrimitives di `package::grafo`
#'
#' @name listPrimitives
#' @title Funzioni del package `grafo`
#' @seealso `grafo::listPrimitives`
#' @exportMethod listPrimitives
#' @param graph GrafoDB instance

methods::setGeneric(
  "listPrimitives",
  function(graph) {
      standardGeneric("listPrimitives")
  })

#' @rdname listPrimitives

methods::setMethod(
  "listPrimitives",
  signature("GrafoDB"),
  function(graph) {
    network <- graph@network
    sources <- igraph::V(network)[
      igraph::degree(network, mode = "in") == 0]$name
    elementary <- listElementaries(graph)
    setdiff(sources, elementary)
  })

#' implementazione di listNodes di `package::grafo`
#'
#' @name listNodes
#' @title Funzioni del package `grafo`
#' @seealso `grafo::listNodes`
#' @exportMethod listNodes
#' @param graph GrafoDB instance

methods::setGeneric(
  "listNodes",
  function(graph) {
    standardGeneric("listNodes")
  })

#' @rdname listNodes

methods::setMethod(
  "listNodes",
  signature("GrafoDB"),
  function(graph) {
    names(graph)
  })

#' Salva il grafo
#'
#' Implementazione di saveGraph per `GrafoDB` dal package `grafo`
#'
#' La funzione si preoccupa di serializzare la transazione al DB, e controllare
#' l'eventualita' di Conflitti, segnalandoli all'utenza.
#'
#' Funzione per salvare un grafo
#'
#' La funzione controlla la presenza di eventuali conflitti e necessita'
#' di risincronizzare i dati del DB con quelli presenti nel Grafo.
#'
#' \itemize{
#'  \item{"1"}{
#'      Identificare le serie aggregate (solo formule) - primitive (solo dati)
#'      cambiate, escludendo eventuali conflitti}
#'   \item{"2"}{Caricarle nel grafo}
#'   \item{"3"}{Rieseguirle}
#'   \item{"4"}{Risalvare il grafo}
#' }
#'
#' La funzione controlla se esistono conflitti nel seguente modo:
#' \itemize{
#'   \item{"dati"}{
#'        Se esistono serie primitive nel DB e nel grafo
#'        in sessione che sono state aggiornate in
#'        contemporanea}
#'   \item{"formule"}{
#'        Se esistono formule nel DB e nel grafo in
#'        sessione aggiornati in contemporanea}
#' }
#'
#' Qualora uno dei due casi si verificasse il grafo va in "conflitto",
#' vengono salvate sia le proprie modifiche che le modifiche fatte da
#' altri e si attende la risoluzione del conflitto attraverso i metodi
#' `fixConflict`. La soluzione dei conflitti non e' un atto di fede:
#' occorre incontrarsi e decidere quale "formula" o quale versione dei dati
#' sia da preferire.
#'
#' @seealso save_graph_impl
#' @name saveGraph
#' @param object istanza di `GrafoDB`
#' @param path erroneamente, dovuta al generic su `grafo` questo sarebbe il
#'  "tag" da dare al grafo. Non c'e' modo di ovviare questo problema. Vedere
#'   il prototipo di funzione di `save_graph_impl`, e' sicuramente piu' chiaro.
#' @param ... Parametri aggiuntivi alla `saveGraph`
#' @include persistence.r
#' @exportMethod saveGraph

methods::setGeneric(
  "saveGraph",
  function(object, path=object@tag, ...) {
    standardGeneric("saveGraph")
  })

#' @rdname saveGraph

methods::setMethod(
  "saveGraph",
  signature("GrafoDB", "ANY"),
  function(object, path=object@tag, ...) {
    object_name <- deparse(substitute(object))
    save_graph_impl(object, path, ...)
    object <- GrafoDB(path)
    invisible(object)
  })

#' Removes an attribute
#'
#' @name deleteMeta
#' @param object graph instance
#' @param ts_name name of the timeseries
#' @param attr_name name of the attribute
#' @param attr_value value of the attributes
#' @export

methods::setGeneric(
  "deleteMeta",
  function(object, ts_name, attr_name, attr_value=NULL) {
    standardGeneric("deleteMeta")
  })

#' @rdname deleteMeta

methods::setMethod(
  "deleteMeta",
  signature("GrafoDB", "character", "character", "ANY"),
  function(object, ts_name, attr_name, attr_value=NULL) {
    deleteMeta_impl(object, ts_name, attr_name, value = attr_value)
  })


#' Returns the dependencies of an object in the Graph
#'
#' @name get_deps
#' @title Funzioni del package `grafo`
#' @seealso `grafo::get_deps`
#' @param object GrafoDB instance
#' @param ts_name name of the object 
#' @return returns a character array of names
#' @exportMethod get_deps

methods::setGeneric(
  "get_deps",
  function(object, ts_name) {
    standardGeneric("get_deps")
  })

#' @rdname get_deps

methods::setMethod(
  "get_deps",
  signature("GrafoDB", "character"),
  function(object, ts_name) {
    ret <- list()
    for (name in ts_name) {
      ret[[name]] <- navigate(object, name, order=1, mode = "in")
    }

    if (length(ret) == 1) {
      ret <- ret[[name]]
    }
    ret
  })

#' implementazione di getTask di `package::grafo`
#'
#' @name getTask
#' @title Funzioni del package `grafo`
#' @seealso `grafo::getTask`
#' @param object GrafoDB instance
#' @param ts_name object name 
#' @return the formula defining the `name` in GrafoDB
#' @exportMethod getTask

methods::setGeneric(
  "getTask",
  function(object, ts_name) {
    standardGeneric("getTask")
  })

#' @rdname getTask
methods::setMethod(
  "getTask",
  signature("GrafoDB", "character"),
  function(object, ts_name) {
    expr(object, ts_name)
  })

#' implementazione di getData di `package::grafo`
#'
#' @name getData
#' @title Funzioni del package `grafo`
#' @seealso `grafo::getData`
#' @param graph GrafoDB instance
#' @param ts_names character array of names
#' @exportMethod getData

methods::setGeneric(
  "getData",
  function(graph, ts_names) {
    standardGeneric("getData")
  })

#' @rdname getData

methods::setMethod(
  "getData",
  signature("GrafoDB", "character"),
  function (graph, ts_names) {
    graph[[ts_names]]
  })

#' implementazione di searchNode di `package::grafo`
#'
#' @name searchNode
#' @title Funzioni del package `grafo`
#' @seealso `grafo::searchNode`
#' @exportMethod searchNode
#' @export
#' @param graph instanza di GrafoDB
#' @param attr_name nome dell'attributo da cercare
#' @param attr_value valore da ricercare 

methods::setGeneric(
  "searchNode",
  function(graph, attr_name, attr_value) {
    standardGeneric("searchNode")
  })

#' @rdname searchNode

methods::setMethod(
  "searchNode",
  signature("GrafoDB", "character", "character"),
  function(graph, attr_name, attr_value) {
    ret <- lookup(graph, attr_name, attr_value)
    if (length(ret) == 0) {
      stop("Non esistono serie con i criteri ", attr_name, " == ", attr_value)
    }
    ret
  })

#' Imposta un metadato per una particolare serie
#'
#' Imposta un metadato `attr_name`=`value` per la serie `ts_name`
#' Se la serie non esiste, va in errore.
#'
#' @name setMeta
#' @return il GrafoDB modificato
#' @param object instanza di GrafoDB
#' @param ts_name nome della serie
#' @param attr_name nome dell'attributo
#' @param value valore dell'attributo da settare
#' @examples \dontrun{
#' g <- GrafoDB()
#' g["A"] <- TSERIES(...)
#' g <- setMeta(g, "A", "FONTE", "CODICE_FONTE")
#'
#' searchNode(g, "FONTE", "CODICE_FONTE") # deve tornare il nome "A"
#' }
#' @exportMethod setMeta
#' @include metadati.r

methods::setGeneric(
  "setMeta",
  function(object, ts_name, attr_name, value) {
    standardGeneric("setMeta")
  })

#' @rdname setMeta

methods::setMethod(
  "setMeta",
  signature("GrafoDB", "character", "character", "character"),
  function(object, ts_name, attr_name, value) {
    set_meta_impl(object, ts_name, attr_name, value)
  })


#' Elimina un nodo dal `GrafoDB`
#'
#' L'eliminazione prevede l'eliminazione dai dati, formule, archi e metadati
#'
#' @name rmNode
#' @param graph istanza di `GrafoDB`
#' @param ts_name nomi di serie da eliminare
#' @param recursive `TRUE` se l'eliminazione deve essere rivorsiva sugli archi
#'   uscenti di ogni serie nel parametro `ts_name`, `FALSE` altrimenti. Se il
#'   parametro e' impostato a `FALSE` e' condizione necessaria che le serie in
#'   `ts_name` siano tutte foglie, ovvero serie senza archi uscenti
#' @note Metodo interno
#' @seealso rm_node_impl
#' @return il grafo modificato
#' @exportMethod rmNode
#' @include functions.r
#' @exportMethod rmNode
#' @export

methods::setGeneric(
  "rmNode",
  function(graph, ts_name, recursive=FALSE) {
    standardGeneric("rmNode")
  })

#' @rdname rmNode

methods::setMethod(
  "rmNode",
  signature("GrafoDB", "character", "ANY"),
  function(graph, ts_name, recursive=FALSE) {
    rm_node_impl(graph, ts_name, recursive)
  })
