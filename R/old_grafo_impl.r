# Questo file contiene le implementazioni dei generics specificate nel package `grafo`


#' implementazione di isNode di `package::grafo`
#'
#' @name isNode
#' @title Funzioni del package `grafo`
#' @usage isNode(graph, tsName)
#' @seealso `grafo::isNode`
#' @include core.r
#' @importFrom grafo isNode

setMethod(
  "isNode",
  signature("GrafoDB", "character"),
  function(graph, tsName) {
    tsName %in% names(graph)
  })

#' implementazione di isPrimitive di `package::grafo`
#'
#' @name isPrimitive
#' @title Funzioni del package `grafo`
#' @usage isPrimitive(graph, tsName)
#' @seealso `grafo::isPrimitive`
#' @importFrom grafo isPrimitive

setMethod(
  "isPrimitive",
  signature("GrafoDB", "character"),
  function(graph, tsName) {
    all(isRoot(graph, tsName) && is.null(expr(graph, tsName)))
  })

#' implementazione di isAggregate di `package::grafo`
#'
#' @name isAggregate
#' @title Funzioni del package `grafo`
#' @usage isAggregate(graph, tsName)
#' @seealso `grafo::isAggregate`
#' @importFrom grafo isAggregate

setMethod(
  "isAggregate",
  signature("GrafoDB", "character"),
  function(graph, tsName) {
    !isRoot(graph, tsName)
  })

#' implementazione di isElementary di `package::grafo`
#'
#' @name isElementary
#' @title Funzioni del package `grafo`
#' @usage isElementary(graph, tsName)
#' @seealso `grafo::isElementary`
#' @note `GrafoDB` non prevede l'utilizzo di serie "elementari" come il `grafo`
#'       Quindi per compliance ritorna sempre `FALSE`, ma il metodo non ha senso
#' @importFrom grafo isElementary

setMethod(
  "isElementary",
  signature("GrafoDB", "character"),
  function(graph, tsName) {
    all(isRoot(graph, tsName) && !is.null(expr(graph, tsName)))
  })

#' implementazione di listAggregates di `package::grafo`
#'
#' @name listAggregates
#' @title Funzioni del package `grafo`
#' @usage listAggregates(graph)
#' @seealso `grafo::listAggregates`
#' @importFrom grafo listAggregates

setMethod(
  "listAggregates",
  signature("GrafoDB"),
  function(graph) {
    network <- graph@network
    formule <- graph@dbformule$name
    leaves <- V(network)[degree(network, mode="out") == 0]$name
    setdiff(formule, leaves)
  })


#' implementazione di listElementaries di `package::grafo`
#'
#' @name listElementaries
#' @title Funzioni del package `grafo`
#' @usage listElementaries(graph)
#' @seealso `grafo::listElementaries`
#' @importFrom grafo listElementaries

setMethod(
  "listElementaries",
  signature("GrafoDB"),
  function(graph) {
    network <- graph@network
    tag <- graph@tag
    formule <-  formule <- graph@dbformule$name
    sources <- V(network)[degree(network, mode="in") == 0]$name
    intersect(sources, formule)
  })

#' implementazione di listPrimitives di `package::grafo`
#'
#' @name listPrimitives
#' @title Funzioni del package `grafo`
#' @usage listPrimitives(graph)
#' @seealso `grafo::listPrimitives`
#' @importFrom grafo listPrimitives

setMethod(
  "listPrimitives",
  signature("GrafoDB"),
  function(graph) {
    network <- graph@network
    sources <- V(network)[degree(network, mode="in") == 0]$name
    elementary <- listElementaries(graph)
    setdiff(sources, elementary)
  })

#' implementazione di listNodes di `package::grafo`
#'
#' @name listNodes
#' @title Funzioni del package `grafo`
#' @usage listNodes(graph)
#' @seealso `grafo::listNodes`
#' @importFrom grafo listNodes

setMethod(
  "listNodes",
  signature("GrafoDB"),
  function(graph) {
    names(graph)
  })

#' implementazione di saveGraph per `GrafoDB` dal package `grafo`
#'
#' La funzione si preoccupa di serializzare la transazione al DB, e controllare
#' l'eventualita' di Conflitti, segnalandoli all'utenza.
#'
#' @seealso .saveGraph
#' @name saveGraph
#' @usage saveGraph(object)
#' @usage saveGraph(object, path)
#' @param object istanza di `GrafoDB`
#' @param path erroneamente, dovuta al generic su `grafo` questo sarebbe il "tag" da
#'             dare al grafo. Non c'e' modo di ovviare questo problema. Vedere il
#'             prototipo di funzione di `.saveGraph`, e' sicuramente piu' chiaro.
#' @param ... Parametri aggiuntivi alla `saveGraph`
#' @note R sometimes sucks.
#' @include persistence.r
#' @importFrom grafo saveGraph

setMethod(
  "saveGraph",
  signature("GrafoDB", "ANY"),
  function(object, path=object@tag, ...) {
    nameObject <- deparse(substitute(object))
    .saveGraph(object, path, ...)
    object <- GrafoDB(path)
    if(object@tag == path) {
      assign(nameObject, object, envir=parent.frame())
    }
    object
  })

#' implementazione di showInternalChanges di `package::grafo`
#'
#' @name showInternalChanges
#' @title Funzioni del package `grafo`
#' @usage showInternalChanges(graph)
#' @seealso `grafo::showInternalChanges`
#' @importFrom grafo showInternalChanges

setMethod(
  "showInternalChanges",
  signature("GrafoDB"),
  function(object) {
    data <- graph@data
    functions <- graph@functions
  })

#' implementazione di listNodes di `package::grafo`
#'
#' @name deleteMeta
#' @title Funzioni del package `grafo`
#' @usage deleteMeta(graph, tsName, attrName, attrValue)
#' @seealso `grafo::deleteMeta`
#' @importFrom grafo deleteMeta

setMethod(
  "deleteMeta",
  signature("GrafoDB", "character", "character", "ANY"),
  function(object, tsName, attrName, attrValue) {
    con <- pgConnect()
    on.exit(dbDisconnect(con))
    tryCatch({
      dbBegin(con)
      sql <- "delete from metadati where tag = ? and name = ? and key = ? nad value =?"
      params <- cbind(object@tag, tsName, attrName, attrValue)
      dbGetPreparedQuery(con, sql, bind.data=params)
      dbCommit(con)
    }, error = function(err) {
      dbRollback(con)
      stop(err)
    })
    
    invisible(object)
  })


#' implementazione di getDependencies di `package::grafo`
#'
#' @name getDependencies
#' @title Funzioni del package `grafo`
#' @usage getDependencies(graph, tsName)
#' @seealso `grafo::getDependencies`
#' @importFrom grafo getDependencies

setMethod(
  "getDependencies",
  signature("GrafoDB", "character"),
  function(object, tsName) {
    ret <- list()
    for(name in tsName) {
      ret[[name]] <- navigate(object, name, order=1, mode="in")
    }
    
    if(length(ret) == 1) {
      ret <- ret[[name]]
    }
    ret
  })

#' implementazione di getTask di `package::grafo`
#'
#' @name getTask
#' @title Funzioni del package `grafo`
#' @usage getTask(graph, tsName)
#' @seealso `grafo::getTask`
#' @importFrom grafo getTask

setMethod(
  "getTask",
  signature("GrafoDB", "character"),
  function(object, tsName) {
    expr(object, tsName)
  })

#' implementazione di getData di `package::grafo`
#'
#' @name getData
#' @title Funzioni del package `grafo`
#' @usage getData(graph, tsNames)
#' @seealso `grafo::getData`
#' @importFrom grafo getData


setMethod(
  "getData",
  signature("GrafoDB", "character"),
  function (graph, tsNames) {
    graph[[tsNames]]
  })

#' implementazione di searchNode di `package::grafo`
#'
#' @name searchNode
#' @title Funzioni del package `grafo`
#' @usage searchNode(graph, tsNames)
#' @seealso `grafo::searchNode`
#' @importFrom grafo searchNode
#' @export

setMethod(
  "searchNode",
  signature("GrafoDB", "character", "character"),
  function(graph, attrName, attrValue) {
    ret <- lookup(graph, attrName, attrValue)
    if(length(ret) == 0) {
      stop("Non esistono serie con i criteri ", attrName, " == ", attrValue)
    } 
    ret
  })

#' Imposta un metadato per una particolare serie
#'
#' Imposta un metadato `attrName`=`value` per la serie `tsName`
#' Se la serie non esiste, va in errore.
#'
#' @name setMeta
#' @return il GrafoDB modificato
#' @examples \dontrun{
#' g <- GrafoDB()
#' g["A"] <- TSERIES(...)
#' g <- setMeta(g, "A", "FONTE", "CODICE_FONTE")
#'
#' searchNode(g, "FONTE", "CODICE_FONTE") # deve tornare il nome "A"
#' }
#' @importFrom grafo setMeta


setMethod(
  "setMeta",
  signature("GrafoDB", "character", "character", "character"),
  function(object, tsName, attrName, value) {
    nomiobj <- names(object)
    if(!all(tsName %in% nomiobj)) {
      nong <- setdiff(tsName, nomiobj)
      stop("Non e' una serie del grafo: ", paste(nong, collapse=", "))
    }

    domain <- lookup(object, attrName, value)
    if(any(tsName %in% domain)) {
      already <- intersect(domain, tsName)
      warning("Ha gia' un metadato ", attrName, " = ", value, " :", paste(already, collapse=", "))
    } else {
      con <- pgConnect()
      on.exit(dbDisconnect(con))
      sql <- "insert into metadati(tag, name, key, value, autore) values (?, ?, ?, ?, ?)"
      autore <- whoami()
      params <- cbind(object@tag, tsName, attrName, value, autore)
      dbGetPreparedQuery(con, sql, bind.data=params)
    }
    object
  })


#' Elimina un nodo dal `GrafoDB`
#'
#' L'eliminazione prevede l'eliminazione dai dati, formule, archi e metadati
#'
#' @name rmNode
#' @usage rmNode(graph, tsName, recursive)
#' @param graph istanza di `GrafoDB`
#' @param tsName nomi di serie da eliminare
#' @param recursive `TRUE` se l'eliminazione deve essere rivorsiva sugli archi
#'                  uscenti di ogni serie nel parametro `tsName`.
#'                  `FALSE` altrimenti. Se il parametro e' impostato a `FALSE` e'
#'                  condizione necessaria che le serie in `tsName` siano tutte
#'                  foglie, ovvero serie senza archi uscenti
#' @note Metodo interno
#' @seealso .rmNode
#' @return il grafo modificato
#' @exportMethod rmNode
#' @include functions.r
#' @importFrom grafo rmNode
#' @export 

setMethod(
  "rmNode",
  signature("GrafoDB", "character", "ANY"),
  function(graph, tsName, recursive=FALSE) {
    .rmNode(graph, tsName, recursive)
  })
